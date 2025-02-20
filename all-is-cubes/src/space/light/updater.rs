//! Lighting algorithms for `Space`. This module is closely tied to `Space`
//! and separated out for readability, not modularity.

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::{fmt, mem};

use euclid::Vector3D;
use manyfmt::Fmt;

#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelRefMutIterator as _, ParallelIterator as _};

use super::debug::LightComputeOutput;
use crate::block::{self, EvaluatedBlock};
use crate::math::{
    Cube, CubeFace, Face6, Face7, FaceMap, OpacityCategory, PositiveSign, Rgb, Rgba, Vol,
};
use crate::raycast::Ray;
use crate::space::light::{
    LightUpdateQueue, LightUpdateRayInfo, LightUpdateRequest, Priority, chart::LightChart,
};
use crate::space::palette::Palette;
use crate::space::{
    BlockIndex, BlockSky, ChangeBuffer, GridAab, LightPhysics, LightStatus, PackedLight,
    PackedLightScalar, Sky, SpaceChange, SpacePhysics,
};
use crate::time::{Duration, Instant};
use crate::util::StatusText;

/// Storage and update queue for a [`Space`]'s light.
///
/// Design note: Currently this is simply owned by the [`Space`], but eventually we want to make
/// it accessible by a background thread which can continuously update it.
pub(crate) struct LightStorage {
    /// Per-cube light data.
    ///
    /// The bounds of this are always either equal to the owning [`Space`]'s,
    /// or zero if light is disabled. Therefore, the same cube indices may be used.
    // ---
    // TODO: stop making this pub
    pub(crate) contents: Vol<Box<[PackedLight]>>,

    /// Queue of cubes whose light values should be updated.
    pub(in crate::space) light_update_queue: LightUpdateQueue,

    /// Debug log of the updated cubes from last frame.
    /// Empty unless this debug function is enabled.
    pub(crate) last_light_updates: Vec<Cube>,

    /// Estimated ratio of (wall-time seconds / light update cost units).
    light_cost_scale: f32,

    /// Redundant copy of `SpacePhysics::light` for local access.
    /// The Space will keep this updated via  [`Self::reinitialize_for_physics_change()`].
    physics: LightPhysics,

    pub(in crate::space) sky: Sky,
    pub(in crate::space) block_sky: BlockSky,
}

/// Methods on Space that specifically implement the lighting algorithm.
impl LightStorage {
    pub(crate) fn new(
        physics: &SpacePhysics,
        contents: Vol<Box<[PackedLight]>>,
        light_update_queue: LightUpdateQueue,
    ) -> Self {
        Self {
            contents,
            light_update_queue,
            last_light_updates: Vec::new(),
            light_cost_scale: 1e-6,
            physics: physics.light.clone(),
            sky: physics.sky.clone(),
            block_sky: physics.sky.for_blocks(),
        }
    }

    pub(in crate::space) fn maybe_reinitialize_for_physics_change(
        &mut self,
        uc: UpdateCtx<'_>,
        physics: &SpacePhysics,
        opacity: OpacityCategory,
    ) {
        let old_physics = mem::replace(&mut self.physics, physics.light.clone());
        self.sky = physics.sky.clone();
        self.block_sky = physics.sky.for_blocks();

        if self.physics != old_physics {
            // TODO: If the new physics is broadly similar, then reuse the old data as a
            // starting point instead of immediately throwing it out.
            self.contents = self
                .physics
                .initialize_lighting(uc.contents.without_elements(), opacity);

            match self.physics {
                LightPhysics::None => {
                    self.light_update_queue.clear();
                }
                LightPhysics::Rays { .. } => {
                    self.fast_evaluate_light(uc);
                }
            }
        } else {
            // TODO: if only sky color is different, trigger light updates
        }
    }

    pub(crate) fn light_needs_update(&mut self, cube: Cube, priority: Priority) {
        if self.contents.bounds().contains_cube(cube) {
            self.light_update_queue
                .insert(LightUpdateRequest { priority, cube });
        }
    }

    #[mutants::skip] // lots of ways for this to still work when modified
    pub(crate) fn light_needs_update_in_region(&mut self, region: GridAab, priority: Priority) {
        let Some(region) = region.intersection_cubes(self.contents.bounds()) else {
            return;
        };
        if region.volume().is_none_or(|v| v > 400) {
            self.light_update_queue.sweep(region, priority);
        } else {
            for cube in region.interior_iter() {
                self.light_needs_update(cube, priority);
            }
        }
    }

    pub(in crate::space) fn modified_cube_needs_update(
        &mut self,
        uc: UpdateCtx<'_>,
        change_buffer: &mut ChangeBuffer<'_>,
        cube: Cube,
        evaluated: &EvaluatedBlock,
        contents_index: usize,
    ) {
        if self.physics == LightPhysics::None {
            return;
        }

        if opaque_for_light_computation(evaluated) {
            // Since we already have the information, immediately update light value
            // to zero rather than putting it in the queue.
            // (It would be mostly okay to skip doing this entirely, but doing it gives
            // more determinism, and the old value could be temporarily revealed when
            // the block is removed.)
            self.contents.as_linear_mut()[contents_index] = PackedLight::OPAQUE;

            // Cancel any previously scheduled light update.
            // (Note: This does not empirically have any significant effect on overall
            // lighting performance — these trivial updates are not most of the cost.
            // But it'll at least save a little bit of memory.)
            self.light_update_queue.remove(cube);

            change_buffer.push(SpaceChange::CubeLight { cube });
        } else {
            self.light_needs_update(cube, Priority::NEWLY_VISIBLE);
        }
        for face in Face6::ALL {
            if let Some(neighbor) = cube.checked_add(face.normal_vector()) {
                // Perform neighbor light updates if they can be affected by us
                if !uc.get_evaluated(neighbor).opaque()[face.opposite()] {
                    self.light_needs_update(neighbor, Priority::NEWLY_VISIBLE);
                }
            }
        }
    }

    #[allow(unused, reason = "currently only used on feature=save and tests")]
    pub(crate) fn in_light_update_queue(&self, cube: Cube) -> bool {
        self.light_update_queue.contains(cube)
    }

    /// Do some lighting updates.
    pub(in crate::space) fn update_lighting_from_queue<I: Instant>(
        &mut self,
        uc: UpdateCtx<'_>,
        change_buffer: &mut ChangeBuffer<'_>,
        budget: Option<Duration>,
    ) -> LightUpdatesInfo {
        let mut light_update_count: usize = 0;
        self.last_light_updates.clear();
        let mut max_difference: PackedLightScalar = 0;

        if self.physics != LightPhysics::None && !budget.is_some_and(|d| d.is_zero()) {
            let t0 = I::now();
            let mut cost = 0;
            // We convert the time budget to an arbitrary cost value in order to avoid
            // the overhead of frequently making syscalls to check the clock.
            //
            // The unit of measure is one raycast step; other operations are arbitrarily assigned
            // higher cost values. (TODO: Profile to assign more consistent cost values.)
            //
            // TODO: Is this worthwhile?
            let max_cost = match budget {
                Some(budget) => (budget.as_secs_f32() / self.light_cost_scale) as usize,
                None => usize::MAX,
            };

            // TODO: More efficient threading. Instead of creating batches up front,
            // run a continuous pipeline of calculations in the background and only apply
            // them when we have this current opportunity.
            // This will require making stored light data `Arc`ed and double-buffered or atomic,
            // so it can be consulted by the calculation.
            #[cfg(feature = "auto-threads")]
            while self.light_update_queue.len() > 0 {
                use core::array::from_fn;

                enum Calc {
                    None,
                    In(LightUpdateRequest),
                    Out(ComputedLight<()>),
                }

                // TODO: empirical tuning suggests that 128 is a good minimum batch size,
                // but is too big for the amount of time we want to take
                let mut data: [Calc; 32] =
                    from_fn(|_| self.light_update_queue.pop().map_or(Calc::None, Calc::In));

                data.par_iter_mut().for_each(|calc| {
                    if let Calc::In(LightUpdateRequest { cube, .. }) = *calc {
                        *calc = Calc::Out(self.compute_lighting(uc, cube));
                    }
                });
                for calc in data {
                    let output = match calc {
                        Calc::None => continue,
                        Calc::In(_) => unreachable!(),
                        Calc::Out(output) => output,
                    };

                    if false {
                        // Log cubes that were updated for debug visualization.
                        self.last_light_updates.push(output.cube);
                    }
                    light_update_count += 1;
                    let (difference, cube_cost) =
                        self.apply_lighting_update(uc, change_buffer, output);
                    max_difference = max_difference.max(difference);
                    cost += cube_cost;
                }

                if cost >= max_cost {
                    break;
                }
            }

            #[cfg(not(feature = "auto-threads"))]
            while let Some(LightUpdateRequest { cube, .. }) = self.light_update_queue.pop() {
                if false {
                    // Log cubes that were updated for debug visualization.
                    self.last_light_updates.push(cube);
                }
                light_update_count += 1;

                let computation = self.compute_lighting(uc, cube);

                let (difference, cube_cost) =
                    self.apply_lighting_update(uc, change_buffer, computation);
                max_difference = max_difference.max(difference);
                cost += cube_cost;
                if cost >= max_cost {
                    break;
                }
            }

            let t1 = I::now();
            let cost_scale = t1.saturating_duration_since(t0).as_secs_f32() / cost as f32;
            if cost_scale.is_finite() {
                // TODO(time-budget): don't let this grow or shrink too fast due to outliers
                self.light_cost_scale = 0.125 * cost_scale + 0.875 * self.light_cost_scale;
            }
        }

        LightUpdatesInfo {
            update_count: light_update_count,
            max_update_difference: max_difference,
            queue_count: self.light_update_queue.len(),
            max_queue_priority: self.light_update_queue.peek_priority(),
        }
    }

    /// Given a fresh [`ComputedLight`], actually insert it into the space light data
    /// and enqueue more cubes, then return a cost value accounting for the update.
    #[inline]
    fn apply_lighting_update(
        &mut self,
        uc: UpdateCtx<'_>,
        change_buffer: &mut ChangeBuffer<'_>,
        computation: ComputedLight<()>,
    ) -> (PackedLightScalar, usize) {
        let ComputedLight {
            cube,
            light: new_light_value,
            dependencies,
            mut cost,
            debug: (),
        } = computation;

        let old_light_value: PackedLight = self.contents[cube];
        // Compare and set new value. Note that we MUST compare only the packed value so
        // that changes are detected in terms of that rounding, not float values.
        let difference_priority = new_light_value.difference_priority(old_light_value);
        if difference_priority > 0 {
            cost += 200;
            // TODO: compute volume index of the cube only once
            self.contents[cube] = new_light_value;
            change_buffer.push(SpaceChange::CubeLight { cube });

            // If neighbors have missing (not just stale) light values, fill them in too.
            for dir in Face6::ALL {
                let neighbor_cube = cube + dir.normal_vector();
                let Some(neighbor_light) = self.contents.get_mut(neighbor_cube) else {
                    // neighbor is out of bounds
                    continue;
                };
                match neighbor_light.status() {
                    LightStatus::Uninitialized => {
                        if *neighbor_light == new_light_value {
                            continue;
                        }
                        if uc.get_evaluated(neighbor_cube).opaque() == FaceMap::splat(true) {
                            // neighbor is fully opaque — don't light it
                            continue;
                        }
                        *neighbor_light = PackedLight::guess(new_light_value.value());
                        change_buffer.push(SpaceChange::CubeLight {
                            cube: neighbor_cube,
                        });
                        // We don't put the neighbor on the update queue because it should
                        // already be there.
                    }
                    LightStatus::Opaque | LightStatus::Visible | LightStatus::NoRays => {
                        // Already valid, or possibly valid; do nothing.
                    }
                }
            }

            // The light algorithm, in its current form, can spend a very long time
            // evaluating 1-unit differences and possibly even loop infinitely. As a
            // pragmatic solution, don't bother queueing them at all. This means that
            // there may be 1-unit random differences lying around, but then, the
            // "reevaluate all the dependencies of the current evaluation" strategy
            // is also not perfect at updating everything that theoretically should be,
            // since the rays are not perfectly reciprocal.
            if difference_priority > 1 {
                let priority = Priority::from_difference(difference_priority);
                for cube in dependencies {
                    self.light_needs_update(cube, priority);
                }
            }
        }
        (difference_priority, cost)
    }

    /// Compute the new lighting value for a cube, returning it rather than storing it.
    #[inline]
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu for debugging
    pub(in crate::space) fn compute_lighting<D>(
        &self,
        uc: UpdateCtx<'_>,
        cube: Cube,
    ) -> ComputedLight<D>
    where
        D: LightComputeOutput,
    {
        let mut cube_buffer = LightBuffer::new();
        let mut info_rays = D::RayInfoBuffer::default();

        let maximum_distance = match self.physics {
            LightPhysics::None => 0,
            LightPhysics::Rays { maximum_distance } => maximum_distance,
        };

        let ev_origin = uc.get_evaluated(cube);
        let origin_is_opaque = ev_origin.opaque() == FaceMap::splat(true);
        if origin_is_opaque {
            // Opaque blocks are always dark inside — unless they are light sources.
            if !opaque_for_light_computation(ev_origin) {
                cube_buffer.add_weighted_light(ev_origin.light_emission(), 1.0);
            }
        } else {
            let ev_neighbors =
                FaceMap::from_fn(|face| uc.get_evaluated(cube + face.normal_vector()));
            let direction_weights = directions_to_seek_light(ev_origin, ev_neighbors);

            for (ray_info, relative_cube_sequence) in LightChart::get().rays(maximum_distance) {
                // TODO: Theoretically we should weight light rays by the cosine but that has caused poor behavior in the past.
                let ray_weight_by_faces = ray_info
                    .face_cosines()
                    .zip(direction_weights, |_face, ray_cosine, reflects| {
                        ray_cosine * reflects
                    })
                    .into_values_iter()
                    .sum::<f32>();
                if ray_weight_by_faces <= 0.0 {
                    continue;
                }
                let mut ray_state =
                    LightRayState::new(cube, ray_info.direction.into(), ray_weight_by_faces);

                // Stores the light value that might have been fetched, if it was, from the previous
                // step's cube_ahead, which is the current step's cube_behind.
                let mut light_behind_cache: Option<PackedLight> = None;

                'raycast: for step in relative_cube_sequence {
                    let cube_face = step
                        .relative_cube_face()
                        .translate(cube.lower_bounds().to_vector());

                    cube_buffer.cost += 1;
                    if !self.contents.bounds().contains_cube(cube_face.cube) {
                        // Stop (and display the sky) if we exit the space bounds.
                        break 'raycast;
                    }

                    let mut light_ahead_cache = None;
                    cube_buffer.traverse::<D>(
                        &mut ray_state,
                        &mut info_rays,
                        self,
                        step.relative_cube_face()
                            .translate(cube.lower_bounds().to_vector()),
                        f64::from(step.distance), // TODO: this will be sloppy
                        uc.get_evaluated(cube_face.cube),
                        &mut light_ahead_cache,
                        light_behind_cache,
                    );
                    if ray_state.alpha.partial_cmp(&0.0) != Some(Ordering::Greater) {
                        break;
                    }

                    light_behind_cache = light_ahead_cache;
                }
                // Rays that didn't hit anything close enough will be treated
                // as sky. TODO: We should have a better policy in case of large
                // indoor spaces.
                cube_buffer.end_of_ray(&ray_state, &self.sky);
            }
        }

        let new_light_value = cube_buffer.finish(origin_is_opaque);

        ComputedLight {
            cube,
            light: new_light_value,
            dependencies: cube_buffer.dependencies,
            cost: cube_buffer.cost,
            debug: D::new(cube, new_light_value, info_rays),
        }
    }

    /// Clear and recompute light data and update queue, in a way which gets fast approximate
    /// results suitable for flat landscapes mostly lit from above (the +Y axis).
    ///
    /// Does not send any change notifications.
    ///
    /// TODO: Revisit whether this is a good public API.
    pub(in crate::space) fn fast_evaluate_light(&mut self, uc: UpdateCtx<'_>) {
        self.light_update_queue.clear(); // Going to refill it

        if self.physics == LightPhysics::None {
            return;
        }

        let bounds = self.contents.bounds();
        for x in bounds.x_range() {
            for z in bounds.z_range() {
                let mut covered = false;
                for y in bounds.y_range().rev() {
                    let cube = Cube::new(x, y, z);
                    let index = self.contents.index(cube).unwrap();

                    let this_cube_evaluated = uc.get_evaluated_by_index(index);
                    self.contents.as_linear_mut()[index] =
                        if opaque_for_light_computation(this_cube_evaluated) {
                            covered = true;
                            PackedLight::OPAQUE
                        } else {
                            if this_cube_evaluated.visible_or_animated()
                                || Face6::ALL.into_iter().any(|face| {
                                    uc.get_evaluated(cube + face.normal_vector())
                                        .visible_or_animated()
                                })
                            {
                                // In this case (and only this case), we are guessing rather than being certain,
                                // so we need to schedule a proper update.
                                // (Bypassing `self.light_needs_update()` to skip bounds checks).
                                self.light_update_queue.insert(LightUpdateRequest {
                                    priority: Priority::ESTIMATED,
                                    cube,
                                });

                                if covered {
                                    PackedLight::UNINITIALIZED_AND_BLACK
                                } else {
                                    self.block_sky.in_direction(Face6::PY)
                                }
                            } else {
                                PackedLight::NO_RAYS
                            }
                        };
                }
            }
        }
    }

    #[inline(always)]
    pub fn get(&self, cube: Cube) -> PackedLight {
        match self.physics {
            LightPhysics::None => PackedLight::ONE,
            _ => self
                .contents
                .get(cube)
                .copied()
                .unwrap_or_else(|| self.block_sky.light_outside(self.contents.bounds(), cube)),
        }
    }

    #[cfg(test)]
    pub fn consistency_check(&self) {
        if self.physics == LightPhysics::None {
            assert_eq!(self.contents.volume(), 0);
        }

        // TODO: validate light update queue
        // - consistency with space bounds
        // - contains all cubes with LightStatus::UNINIT
    }
}

/// Argument passed to [`LightStorage`] methods to provide immutable and shareable access to the
/// rest of the space. (Don't try to add any `&mut` references to this!)
#[derive(Clone, Copy, Debug)]
pub(in crate::space) struct UpdateCtx<'a> {
    pub(in crate::space) contents: Vol<&'a [BlockIndex]>,
    pub(in crate::space) palette: &'a Palette,
}

impl<'a> UpdateCtx<'a> {
    fn get_evaluated(&self, cube: Cube) -> &'a EvaluatedBlock {
        if let Some(&block_index) = self.contents.get(cube) {
            self.palette.entry(block_index).evaluated()
        } else {
            block::AIR_EVALUATED_REF
        }
    }

    fn get_evaluated_by_index(&self, cube_index: usize) -> &'a EvaluatedBlock {
        self.palette
            .entry(self.contents.as_linear()[cube_index])
            .evaluated()
    }
}

impl LightPhysics {
    /// Generate the lighting data array that a [`Space`] with this light physics should have.
    ///
    /// `opacity` specifies Whether the blacks in the space are uniformly fully opaque or
    /// uniformly fully transparent, in which case the initialization can be optimal.
    ///
    /// TODO: Also return whether light updates are needed.
    pub(crate) fn initialize_lighting(
        &self,
        bounds: Vol<()>,
        opacity: OpacityCategory,
    ) -> Vol<Box<[PackedLight]>> {
        match self {
            LightPhysics::None => {
                Vol::repeat(GridAab::ORIGIN_EMPTY, PackedLight::UNINITIALIZED_AND_BLACK)
            }
            LightPhysics::Rays { .. } => {
                let value = match opacity {
                    OpacityCategory::Invisible => PackedLight::NO_RAYS,
                    OpacityCategory::Partial => PackedLight::UNINITIALIZED_AND_BLACK,
                    OpacityCategory::Opaque => PackedLight::OPAQUE,
                };
                Vol::repeat(bounds.bounds(), value)
            }
        }
    }
}

/// Given a block and its neighbors, which directions should we cast rays to find light
/// falling on it?
fn directions_to_seek_light(
    origin: &EvaluatedBlock,
    neighborhood: FaceMap<&EvaluatedBlock>,
) -> FaceMap<f32> {
    if origin.visible_or_animated() {
        // Non-opaque blocks should work the same as blocks which have all six adjacent faces present.
        FaceMap::splat(1.0)
    } else {
        FaceMap::from_fn(|face| {
            // We want directions that either face away from visible faces, or towards light sources.
            if neighborhood[face.opposite()].visible_or_animated()
                || neighborhood[face].light_emission() != Rgb::ZERO
            {
                // TODO: Once we have fancier block opacity precomputations, use them to
                // have weights besides 1.0
                1.0f32
            } else {
                0.0
            }
        })
    }
}

/// Accumulation buffer for the light falling on a single cube.
#[derive(Debug)]
struct LightBuffer {
    /// Accumulator of incoming light encountered.
    /// TODO: Make this a vector of `f32` to save NaN checks?
    incoming_light: Rgb,
    /// Number of rays, weighted by the ray angle versus local cube faces.
    total_ray_weight: f32,
    /// Cubes whose lighting value contributed to the `incoming_light` value.
    dependencies: Vec<Cube>,
    /// Approximation of CPU cost of doing the calculation, with one unit defined as
    /// one raycast step.
    cost: usize,
}

/// Companion to [`LightBuffer`] that tracks state for a single ray that makes part of
/// the sum.
struct LightRayState {
    /// Fraction of the light value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as opaque surfaces are encountered.
    alpha: f32,

    /// Weighting factor for how much this ray contributes to the total light.
    /// If zero, this will not be counted as a ray at all.
    ///
    /// Note that this is unrelated to alpha — it is *not* reduced by opacity.
    /// It determines what proportion of the final light value is produced by this ray
    /// relative to other rays.
    ray_weight_by_faces: f32,

    /// The ray we're casting; remembered for debugging and sky light sampling.
    translated_ray: Ray,
}

impl LightRayState {
    /// * `origin_cube`: cube we are actually starting from
    /// * `abstract_ray`: ray as if we were lighting the [0, 0, 0] cube
    /// * `ray_weight_by_faces`: how much influence this ray should have on the
    ///   total illumination
    fn new(origin_cube: Cube, direction: Vector3D<f32, Cube>, ray_weight_by_faces: f32) -> Self {
        let translated_ray = Ray::new(origin_cube.midpoint(), direction.map(f64::from));
        LightRayState {
            alpha: 1.0,
            ray_weight_by_faces,
            translated_ray,
        }
    }
}

impl LightBuffer {
    fn new() -> Self {
        Self {
            incoming_light: Rgb::ZERO,
            total_ray_weight: 0.0,
            dependencies: Vec::new(),
            cost: 0,
        }
    }

    /// Process a ray intersecting a single cube.
    ///
    /// The caller should check `ray_state.alpha` to decide when to stop calling this.
    ///
    /// Note: to avoid redundant lookups as a ray proceeds, `current_light` is used only to fill
    /// `light_ahead_cache` or `light_behind_cache`.
    #[inline]
    #[expect(clippy::too_many_arguments)]
    fn traverse<D>(
        &mut self,
        ray_state: &mut LightRayState,
        info: &mut D::RayInfoBuffer,
        current_light: &LightStorage,
        hit: CubeFace,
        distance: f64,
        ev_hit: &EvaluatedBlock,
        light_ahead_cache: &mut Option<PackedLight>,
        light_behind_cache: Option<PackedLight>,
    ) where
        D: LightComputeOutput,
    {
        if !ev_hit.visible_or_animated() {
            // Completely transparent block is passed through, disregarding its stored light
            // (which it typically will not have).
            return;
        }

        // Compute whether we hit an opaque face which should totally stop propagation.
        //
        // TODO: Also count the opacity of the face we *exited* of the previous block
        // (but should we do that here?).
        //
        // Note that hit_opaque_face is not necessarily true when hit_alpha is 1.0, because
        // the former is strict “watertightness” and the latter is merely an averaged image of
        // the block. (TODO: But maybe we should always use hit_alpha anyway?)
        let face_opacity = ev_hit.opaque();
        let hit_opaque_face: bool = match Face6::try_from(hit.face) {
            Ok(face) => face_opacity[face],
            Err(_) => face_opacity == FaceMap::splat(true),
        };

        if hit_opaque_face && hit.face == Face7::Within {
            // We are inside the block the ray started in. Don't use its existing light value!
            // Just consider it a total absence of light.
            // (TODO: In principle, a block could have light emission inside itself while being
            // fully opaque, and we should be able to support that, but we're not trying for now.)
            //
            // (Note that not reading *transparent* block light is handled separately below.)

            // Setting the weight to 0 cancels its future effect,
            // and there were no past effects.
            ray_state.ray_weight_by_faces = 0.0;
            ray_state.alpha = 0.0;
            return;
        }

        let hit_surface_color: Rgba = ev_hit.face7_color(hit.face).clamp();
        // The alpha of the hit block face is also what fraction of the light ray we assume to hit
        // the block, as opposed to passing through it.
        let hit_alpha: f32 = hit_surface_color.alpha().into_inner();

        // On striking a (semi-)opaque block face, we use the light value from its
        // adjacent cube as the light falling on, thus being reflected by, that face.
        // The ray still might continue through.
        if hit_alpha > 0.0 && hit.face != Face7::Within {
            let light_cube = hit.adjacent();
            let stored_light = light_behind_cache.unwrap_or_else(|| current_light.get(light_cube));

            let light_from_struck_face =
                ev_hit.light_emission() + hit_surface_color.reflect(stored_light.value());

            self.incoming_light +=
                light_from_struck_face * ray_state.alpha * ray_state.ray_weight_by_faces;

            self.cost += 10;
            if self.dependencies.last() != Some(&light_cube) {
                // add dep only if not already present from previous step's ahead
                self.dependencies.push(light_cube);
            }

            // If we hit a truly light-proof face, it's the end of the ray.
            if hit_opaque_face {
                // This terminates the raycast; we don't bounce rays
                // (diffuse reflections, not specular/mirror).
                ray_state.alpha = 0.0;

                // Diagnostics:
                // Iff this is the hit that terminates the ray, record it.
                // TODO: Record transparency too.
                D::push_ray(info, || LightUpdateRayInfo {
                    ray: ray_state.translated_ray.scale_direction(distance),
                    trigger_cube: hit.cube,
                    value_cube: light_cube,
                    value: stored_light,
                    light_from_struck_face,
                });
            } else {
                // Account for surface alpha in the future of this ray's state
                ray_state.alpha *= 1.0 - hit_alpha;
            }
        }

        // Block is partly transparent and light should be picked up from the block's cube itself,
        // but the ray is also not stopped.
        if hit_alpha < 1.0 {
            let light_cube = hit.cube;

            let stored_light = if hit.face == Face7::Within {
                // Don't read the value we're trying to recalculate.
                Rgb::ZERO
            } else {
                light_ahead_cache
                    .get_or_insert_with(|| current_light.get(light_cube))
                    .value()
            };
            // Note that light emission is *not* multiplied by the alpha, because alpha is about
            // reflection/transmission. It's perfectly okay to have a totally transparent (alpha
            // equals zero), yet emissive, block.
            let light_from_traversed_block = ev_hit.light_emission() + stored_light * hit_alpha;
            self.incoming_light +=
                light_from_traversed_block * ray_state.alpha * ray_state.ray_weight_by_faces;

            self.cost += 10;
            self.dependencies.push(light_cube);

            ray_state.alpha *= 1.0 - hit_alpha;
        }
    }

    /// The raycast exited the world or hit an opaque block; finish up by applying
    /// sky and incrementing the count.
    fn end_of_ray(&mut self, ray_state: &LightRayState, sky: &Sky) {
        // TODO: set *info even if we hit the sky

        // Note: this condition is key to allowing some cases to
        // not count this as a successful ray.
        // TODO: clarify signaling flow?
        if ray_state.ray_weight_by_faces > 0. {
            // Note that if ray_state.alpha has reached zero, the sky color has no effect.
            self.add_weighted_light(
                sky.sample(ray_state.translated_ray.direction) * ray_state.alpha,
                ray_state.ray_weight_by_faces,
            );
        }
    }

    /// Add the given color to the sum counting it as having the weight of the given
    /// [`LightRayState`].
    ///
    /// Note that this does not mean this is the sole contribution of that ray;
    /// and it must be called only once per ray (to avoid double-counting weight)
    /// even when the ray passes through transparent blocks that reflect or emit light.
    fn add_weighted_light(&mut self, color: Rgb, weight: f32) {
        self.incoming_light += color * weight;
        self.total_ray_weight += weight;
    }

    /// Return the [`PackedLight`] value accumulated here
    fn finish(&self, origin_is_opaque: bool) -> PackedLight {
        // if total_ray_weight is zero then incoming_light is zero so the result will be zero.
        // We just need to avoid dividing by zero.
        let scale = PositiveSign::<f32>::new_clamped(1.0 / self.total_ray_weight.max(1.0));
        let new_light_value: PackedLight = if self.total_ray_weight > 0.0 {
            PackedLight::some(self.incoming_light * scale)
        } else if origin_is_opaque {
            PackedLight::OPAQUE
        } else {
            PackedLight::NO_RAYS
        };
        new_light_value
    }
}

/// Result of [`Space::compute_lighting()`] — new light for one cube.
/// TODO: better name
#[derive(Clone, Debug)]
#[doc(hidden)] // used for debug rendering
#[expect(unnameable_types)]
pub struct ComputedLight<D> {
    pub cube: Cube,

    pub light: PackedLight,
    /// Cubes which the computed value depends on (imprecisely; empty cubes passed through
    /// are not listed).
    ///
    /// Note: I tried making this allocation reused and it didn't help.
    dependencies: Vec<Cube>,

    cost: usize,

    pub debug: D,
}

/// Performance data for bulk light updates.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct LightUpdatesInfo {
    /// Number of blocks whose light data updates are aggregated in this data.
    pub update_count: usize,
    /// The largest change in light value that occurred.
    pub max_update_difference: u8,
    /// Number of entries in the light update queue.
    pub queue_count: usize,
    /// The largest update priority in the queue (corresponds to the size of
    /// difference that caused the cube to be added).
    pub(crate) max_queue_priority: Priority,
}
impl core::ops::AddAssign<LightUpdatesInfo> for LightUpdatesInfo {
    #[mutants::skip] // hard to test completely
    fn add_assign(&mut self, other: Self) {
        let Self {
            update_count,
            max_update_difference,
            queue_count,
            max_queue_priority,
        } = self;
        *update_count += other.update_count;
        *max_update_difference = (*max_update_difference).max(other.max_update_difference);
        *queue_count += other.queue_count;
        *max_queue_priority = (*max_queue_priority).max(other.max_queue_priority);
    }
}
impl Fmt<StatusText> for LightUpdatesInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        write!(
            fmt,
            "{:4} (max diff {:3}) of {:4} (max pri {:3?})",
            self.update_count,
            self.max_update_difference,
            self.queue_count,
            self.max_queue_priority
        )?;
        Ok(())
    }
}

/// A special definition of opacity for the lighting algorithm:
/// we want to treat opaque light-emitting blocks similarly to transparent blocks
/// *when deciding to compute light for them*, because this produces better results
/// for smooth (interpolated) lighting.
///
/// This function is fairly straightforward; it exists for purposes of *documenting
/// the places that care about this* rather than for code reduction.
pub(crate) fn opaque_for_light_computation(block: &EvaluatedBlock) -> bool {
    block.opaque() == FaceMap::splat(true) && block.light_emission() == Rgb::ZERO
}
