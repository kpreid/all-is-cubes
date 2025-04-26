//! First-run game content.

use alloc::boxed::Box;
use alloc::string::{String, ToString as _};
use alloc::sync::Arc;
use core::error::Error;

use macro_rules_attribute::macro_rules_derive;
use paste::paste;

use all_is_cubes::block::Block;
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::euclid::{Point3D, Size3D};
use all_is_cubes::linking::{BlockProvider, GenError, InGenError};
use all_is_cubes::math::{
    Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridSize, GridSizeCoord, GridVector,
    Rgb, Rgba,
};
use all_is_cubes::save::WhenceUniverse;
use all_is_cubes::space::{LightPhysics, Space};
use all_is_cubes::transaction::Transaction as _;
use all_is_cubes::universe::{Handle, Name, Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{time, transaction};

use crate::fractal::menger_sponge_from_size;
use crate::{LandscapeBlocks, wavy_landscape};
use crate::{atrium::atrium, demo_city, dungeon::demo_dungeon, install_demo_blocks};
use crate::{free_editing_starter_inventory, palette};

/// Generate a `#[test]` function for each element of [`UniverseTemplate`].
/// This macro is used as a derive macro via [`macro_rules_derive`].
macro_rules! generate_template_test {
    (
        $(#[$type_meta:meta])*
        $vis:vis enum $enum_name:ident {
            $(
                $( #[doc = $doc:literal] )*
                $( #[cfg($variant_cfg:meta)] )*
                $variant_name:ident
            ),* $(,)?
        }
    ) => {
        $(
            paste! {
                $( #[cfg($variant_cfg)] )*
                #[cfg(test)]
                #[tokio::test]
                #[allow(non_snake_case)]
                async fn [< template_ $variant_name >] () {
                    tests::check_universe_template($enum_name::$variant_name).await;
                }
            }
        )*
    }
}

/// Selection of initial content for constructing a new [`Universe`].
//
// TODO: Stop using strum, because we will eventually want parameterized templates.
#[derive(
    Clone,
    Debug,
    Eq,
    Hash,
    PartialEq,
    strum::Display,
    strum::EnumString,
    strum::EnumIter,
    strum::IntoStaticStr,
)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
#[macro_rules_derive(generate_template_test!)]
pub enum UniverseTemplate {
    /// Provides an interactive menu of other templates.
    Menu,

    /// New universe with no contents at all.
    Blank,

    /// Always produces an error, for testing error-handling functionality.
    Fail,

    /// Space with assorted “exhibits” demonstrating or testing various features of All is Cubes.
    DemoCity,

    /// Randomly generated connected rooms.
    /// Someday this might have challenges or become a tutorial.
    Dungeon,

    /// Large space with separate floating islands.
    Islands,

    /// A procedural voxel version of the classic [Sponza] Atrium rendering test scene.
    ///
    /// [Sponza]: https://en.wikipedia.org/wiki/Sponza_Palace
    Atrium,

    /// A procedural voxel version of the classic [Cornell Box] rendering test scene.
    ///
    /// [Cornell Box]: https://en.wikipedia.org/wiki/Cornell_box
    CornellBox,

    /// A [Menger sponge] fractal.
    ///
    /// [Menger sponge]: https://en.wikipedia.org/wiki/Menger_sponge
    MengerSponge,

    /// A test scene containing various shapes and colors to exercise the lighting algorithm.
    LightingBench,

    /// Use entirely random choices.
    ///
    /// TODO: This doesn't yet produce anything even visible — we need more sanity constraints.
    #[cfg(feature = "arbitrary")]
    Random,
    // TODO: add an "nothing, you get a blank editor" option once we have enough editing support.
}

impl UniverseTemplate {
    /// Whether the template should be shown to users.
    /// (This does not control )
    pub fn include_in_lists(&self) -> bool {
        use UniverseTemplate::*;
        match self {
            DemoCity | Dungeon | Atrium | Islands | CornellBox | MengerSponge | LightingBench => {
                true
            }

            // Itself a list of templates!
            Menu => false,

            // More testing than interesting demos.
            Blank | Fail => false,

            #[cfg(feature = "arbitrary")]
            Random => false,
        }
    }

    /// Create a new [`Universe`] based on this template's specifications.
    pub async fn build<I: time::Instant>(
        self,
        p: YieldProgress,
        params: TemplateParameters,
    ) -> Result<Universe, GenError> {
        let mut universe = Universe::new();

        // TODO: Later we want a "module loading" system that can lazily bring in content.
        // For now, unconditionally add all these blocks.
        let [demo_blocks_progress, p] = p.split(0.1);
        {
            let mut install_txn = UniverseTransaction::default();
            install_demo_blocks(&mut install_txn, demo_blocks_progress).await?;
            install_txn.execute(&mut universe, &mut transaction::no_outputs)?;
        }
        p.progress(0.).await;

        let default_space_name: Name = "space".into();

        let maybe_space = {
            let params = params.clone();
            let mut p = Some(p);
            use UniverseTemplate::*;
            let maybe_space: Option<Result<Space, InGenError>> = match self {
                Menu => Some(
                    crate::menu::template_menu_space(
                        &mut universe,
                        p.take().unwrap(),
                        Arc::new(|_| {}),
                    )
                    .await,
                ),
                Blank => None,
                Fail => Some(Err(InGenError::Other(
                    "the Fail template always fails to generate".into(),
                ))),
                DemoCity => Some(demo_city::<I>(&mut universe, p.take().unwrap(), params).await),
                Dungeon => Some(demo_dungeon(&mut universe, p.take().unwrap(), params).await),
                Islands => Some(islands(&mut universe, p.take().unwrap(), params).await),
                Atrium => Some(atrium(&mut universe, p.take().unwrap()).await),
                CornellBox => Some(cornell_box(params.size.unwrap_or(GridSize::splat(57)))),
                MengerSponge => Some(
                    menger_sponge_from_size(
                        &mut universe,
                        p.take().unwrap(),
                        params.size.unwrap_or(GridSize::splat(3u32.pow(4))),
                    )
                    .await,
                ),
                LightingBench => Some(
                    all_is_cubes::content::testing::lighting_bench_space(
                        &mut universe,
                        p.take().unwrap(),
                        params.size.unwrap_or(GridSize::new(54, 16, 54)),
                    )
                    .await,
                ),
                #[cfg(feature = "arbitrary")]
                Random => Some(
                    arbitrary_space(&mut universe, p.take().unwrap(), params.seed.unwrap_or(0))
                        .await,
                ),
            };

            if let Some(p) = p {
                p.finish().await;
            }

            maybe_space
        };

        // Insert the space and generate the initial character.
        if let Some(space_result) = maybe_space {
            let space_handle =
                insert_generated_space(&mut universe, default_space_name, space_result)?;

            // TODO: "character" is a special default name used for finding the character the
            // player actually uses, and we should replace that or handle it more formally.
            universe.insert(
                "character".into(),
                Character::spawn_default(universe.read_ticket(), space_handle),
            )?;
        }

        universe.whence = Arc::new(TemplateAndParameters {
            template: self.clone(),
            parameters: params,
        });

        Ok(universe)
    }
}

impl Default for UniverseTemplate {
    fn default() -> Self {
        Self::DemoCity
    }
}

/// TODO: This should be a general Universe tool for "insert a generated value or report an error"
/// but for now was written to help out `UniverseTemplate::build`
fn insert_generated_space(
    universe: &mut Universe,
    name: Name,
    result: Result<Space, InGenError>,
) -> Result<Handle<Space>, GenError> {
    match result {
        Ok(space) => Ok(universe.insert(name, space)?),
        Err(e) => Err(GenError::failure(e, name)),
    }
}

/// Configuration for exactly what a [`UniverseTemplate`] should produce.
///
/// Pass this structure to [`UniverseTemplate::build()`].
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct TemplateParameters {
    /// Seed for any randomization which the template performs.
    /// Not all templates have random elements.
    ///
    /// The seed is optional so that user input processing can distinguish whether the
    /// seed was explicitly specified. If a template receives a seed of `None`, (TODO
    /// define what should happen. Fail? Treat equal to `Some(0)`?)
    //---
    // Design note: u64 was chosen because both `std::hash::Hasher` and `rand::SeedableRng`
    // agree on this many bits for seeds.
    pub seed: Option<u64>,

    /// Dimensions of the primary space of the universe, if there is such a thing.
    ///
    /// If the space cannot be constructed in approximately this size, building the
    /// template should return an error.
    pub size: Option<GridSize>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct TemplateAndParameters {
    template: UniverseTemplate,
    parameters: TemplateParameters,
}

impl WhenceUniverse for TemplateAndParameters {
    fn document_name(&self) -> Option<String> {
        Some(self.template.to_string())
    }

    fn can_load(&self) -> bool {
        false
    }

    fn load(
        &self,
        progress: YieldProgress,
    ) -> futures_core::future::BoxFuture<'static, Result<Universe, Box<dyn Error + Send + Sync>>>
    {
        let ingredients = self.clone();
        Box::pin(async move {
            ingredients
                .template
                // TODO: don't use placeholder time
                .build::<time::NoTime>(progress, ingredients.parameters)
                .await
                .map_err(From::from)
        })
    }

    fn can_save(&self) -> bool {
        false
    }

    fn save(
        &self,
        universe: &Universe,
        progress: YieldProgress,
    ) -> futures_core::future::BoxFuture<'static, Result<(), Box<dyn Error + Send + Sync>>> {
        // Delegate to the same error as () would produce. TODO: Have an error enum instead
        <() as WhenceUniverse>::save(&(), universe, progress)
    }
}

// -- Specific templates below this point ---

async fn islands(
    universe: &mut Universe,
    p: YieldProgress,
    params: TemplateParameters,
) -> Result<Space, InGenError> {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;

    let TemplateParameters { size, seed: _ } = params;
    let size = size.unwrap_or(GridSize::new(1000, 400, 1000));

    // Set up dimensions
    #[expect(clippy::cast_possible_wrap, reason = "big numbers will break anyway")]
    let bounds = GridAab::checked_from_lower_size(
        [
            -((size.width / 2) as i32),
            -((size.height / 2) as i32),
            size.depth as i32,
        ],
        size,
    )
    .map_err(InGenError::other)?; // TODO: add automatic error conversion?

    let mut space = Space::builder(bounds)
        .sky_color(palette::DAY_SKY_COLOR)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn.set_eye_position(bounds.center());
            // TODO: Make this tidier by having a "shrink to centermost point or cube" operation on GridAab
            let cp = bounds.center().map(|c| c as GridCoordinate);
            spawn.set_bounds(GridAab::from_lower_size(
                cp - GridVector::new(30, 30, 30),
                [60, 60, 60],
            ));
            spawn
        })
        .build();

    // Set up grid in which islands are placed
    let island_stride = 50;
    let island_grid = bounds.divide(island_stride);

    for (i, island_pos) in island_grid.interior_iter().enumerate() {
        let cell_bounds = GridAab::from_lower_size(
            (island_pos.lower_bounds().to_vector() * island_stride).to_point(),
            Size3D::splat(island_stride).to_u32(),
        )
        .intersection_cubes(bounds)
        .expect("island outside space bounds");
        // TODO: randomize island location in cell?
        let margin = 10;
        // TODO: non-panicking expand() will be a better solution than this conditional here
        if cell_bounds.size().width >= margin * 2
            && cell_bounds.size().height >= margin + 25
            && cell_bounds.size().depth >= margin * 2
        {
            let occupied_bounds = cell_bounds
                .shrink(FaceMap::splat(10).with(Face6::PY, 25))
                .unwrap();
            wavy_landscape(occupied_bounds, &mut space, &landscape_blocks, 0.5)?;
        }
        p.progress(i as f32 / island_grid.volume_f64() as f32).await;
    }

    Ok(space)
}

#[rustfmt::skip]
fn cornell_box(requested_size: GridSize) -> Result<Space, InGenError> {
    let box_size: GridSizeCoord =
        (requested_size.width
            .min(requested_size.height)
            .min(requested_size.depth))
        .saturating_sub(2)
        .min(64); // TODO: tie this to max light chart size
    let box_size_c: GridCoordinate = box_size as GridCoordinate;

    // Add one block to all sides for wall thickness.
    let bounds = GridAab::from_lower_size(
        [-1, -1, -1],
        GridSize::splat(box_size + 2),
    );
    let mut space = Space::builder(bounds)
        // There shall be no light but that which we make for ourselves!
        .sky_color(Rgb::ZERO)
        .light_physics(LightPhysics::Rays {
            maximum_distance: (box_size * 2).try_into().unwrap_or(u8::MAX),
        })
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn.set_eye_position(Point3D::<FreeCoordinate, _>::new(0.5, 0.5, 1.6)
                * FreeCoordinate::from(box_size));
            spawn
        })
        .build();

    let white: Block = Rgba::new(1.0, 1.0, 1.0, 1.0).into();
    let red: Block = Rgba::new(0.57, 0.025, 0.025, 1.0).into();
    let green: Block = Rgba::new(0.025, 0.236, 0.025, 1.0).into();
    let light: Block = Block::builder()
        .display_name("Light")
        .color(Rgba::new(1.0, 1.0, 1.0, 1.0))
        .light_emission(Rgb::ONE * (1.07 * (box_size as f32).sqrt()))
        .build();

    // Floor.
    space.fill_uniform(GridAab::from_lower_size([0, -1, 0], [box_size, 1, box_size]), &white)?;
    // Ceiling.
    space.fill_uniform(GridAab::from_lower_size([0, box_size_c, 0], [box_size, 1, box_size]), &white)?;
    // Light in ceiling.
    space.fill_uniform(
        GridAab::from_lower_upper([21, 55, 23], [34, 55, 33])
            .multiply(box_size_c).divide(55)
            .abut(Face6::PY, 1).unwrap(),
        &light,
    )?;
    // Back wall.
    space.fill_uniform(GridAab::from_lower_size([0, 0, -1], [box_size, box_size, 1]), &white)?;
    // Right wall (green).
    space.fill_uniform(GridAab::from_lower_size([box_size_c, 0, 0], [1, box_size, box_size]), &green)?;
    // Left wall (red).
    space.fill_uniform(GridAab::from_lower_size([-1, 0, 0], [1, box_size, box_size]), &red)?;

    // Block #1
    space.fill_uniform(GridAab::from_lower_size([29, 0, 36], [16, 16, 15]).multiply(box_size_c).divide(55), &white)?;
    // Block #2
    space.fill_uniform(GridAab::from_lower_size([10, 0, 13], [18, 33, 15]).multiply(box_size_c).divide(55), &white)?;

    // This won't figure out the correct light values, but it will reset everything to
    // uninitialized, which will help the updater get going faster.
    space.fast_evaluate_light();

    Ok(space)
}

#[cfg(feature = "arbitrary")]
async fn arbitrary_space(
    _: &mut Universe,
    mut progress: YieldProgress,
    seed: u64,
) -> Result<Space, InGenError> {
    use all_is_cubes::euclid::Vector3D;
    use arbitrary::{Arbitrary, Error, Unstructured};
    use rand::{RngCore, SeedableRng};

    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);
    let mut bytes = vec![0u8; 16384];
    let mut attempt = 0;
    loop {
        attempt += 1;
        rng.fill_bytes(&mut bytes);
        let r: Result<Space, _> = Arbitrary::arbitrary(&mut Unstructured::new(&bytes));
        match r {
            Ok(mut space) => {
                // Patch spawn position to be reasonable
                let bounds = space.bounds();
                let mut spawn = space.spawn().clone();
                spawn.set_bounds(bounds.expand(FaceMap::splat(20)));
                spawn.set_eye_position(bounds.center());
                space.set_spawn(spawn);

                // Patch physics to be reasonable
                let mut p = space.physics().clone();
                p.gravity = Vector3D::zero(); // won't be a floor
                space.set_physics(p);

                // TODO: These patches are still not enough to get a good result.

                return Ok(space);
            }
            Err(Error::IncorrectFormat) => {
                if attempt >= 1000000 {
                    return Err(InGenError::Other("Out of attempts".into()));
                }
            }
            Err(e) => panic!("{}", e),
        }
        progress = progress.finish_and_cut(0.1).await; // mostly nonsense but we do want to yield
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block;
    use all_is_cubes::util::yield_progress_for_testing;
    use futures_core::future::BoxFuture;

    #[expect(clippy::let_underscore_future)]
    fn _test_build_future_is_send() {
        let _: BoxFuture<'_, _> = Box::pin(UniverseTemplate::Atrium.build::<std::time::Instant>(
            yield_progress_for_testing(),
            TemplateParameters::default(),
        ));
    }

    /// Test one template.
    /// This function is called by the [`generate_template_test!`] macro.
    pub(super) async fn check_universe_template(template: UniverseTemplate) {
        let params = if let UniverseTemplate::Islands = template {
            // Kludge: the islands template is known to be very slow.
            // We should work on making what it does faster, but for now, let's
            // run a much smaller instance of it for the does-it-succeed test.
            TemplateParameters {
                seed: Some(0x7f16dfe65954583e),
                size: Some(GridSize::new(100, 50, 100)),
            }
        } else {
            TemplateParameters {
                seed: Some(0x7f16dfe65954583e),
                size: None,
            }
        };

        let result = template
            .clone()
            .build::<std::time::Instant>(yield_progress_for_testing(), params)
            .await;

        if matches!(template, UniverseTemplate::Fail) {
            // The Fail template _should_ return an error
            result.unwrap_err();
        } else {
            let mut u = result.unwrap();

            if template != UniverseTemplate::Blank {
                let _ = u
                    .get_default_character()
                    .unwrap()
                    .read(u.read_ticket())
                    .unwrap();
            }
            u.step(false, time::DeadlineNt::Asap);

            check_block_spaces(&u);
        }

        // Test that asking for an impossibly huge size does not panic, but returns an error or
        // ignores the size.
        //
        // (This shouldn't ever actually hog memory because it'd be too much to succeed
        // in allocating, regardless of platform, unless the template clamps all but exactly one
        // dimension.)
        // TODO: This test doesn't pass but it should.
        if false {
            println!(
                "too-big result: {:?}",
                template
                    .clone()
                    .build::<std::time::Instant>(
                        yield_progress_for_testing(),
                        TemplateParameters {
                            seed: Some(0),
                            size: Some(GridSize::splat(GridSizeCoord::MAX)),
                        }
                    )
                    .await
            );
        }
    }

    /// Assert that every `Space` used in a `Block` has appropriate properties;
    /// in particular, that it is not unnecessarily having lighting computations.
    fn check_block_spaces(universe: &Universe) {
        // TODO: also check blocks that are found in `Composite` and directly in `Space`, etc.
        // Use case for `VisitHandles` being more general?
        for (block_def_name, block_def_handle) in universe.iter_by_type::<block::BlockDef>() {
            let block_def = &*block_def_handle.read(universe.read_ticket()).unwrap();
            if let block::Primitive::Recur {
                space: space_handle,
                ..
            } = block_def.block().primitive()
            {
                assert_eq!(
                    space_handle
                        .read(universe.read_ticket())
                        .unwrap()
                        .physics()
                        .light,
                    LightPhysics::None,
                    "block {block_def_name} has space {space_name} \
                        whose light physics are not disabled",
                    space_name = space_handle.name()
                );
            }
        }
    }
}
