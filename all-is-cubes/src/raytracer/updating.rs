// TODO: ideally this would live in `all-is-cubes-render`,
// but it depends on `maybe_sync` and the internals of `SpaceRaytracer`.

use alloc::sync::Arc;
use core::fmt;
use core::mem;

use hashbrown::HashSet as HbHashSet;

use crate::block::AIR;
use crate::camera::GraphicsOptions;
use crate::content::palette;
use crate::listen::{self, Listen as _, Listener};
use crate::math::Cube;
use crate::raytracer::{RtBlockData, RtOptionsRef, SpaceRaytracer, TracingBlock, TracingCubeData};
use crate::space::{self, BlockIndex, Space, SpaceChange};
use crate::universe::{Handle, HandleError};

/// Manages a [`SpaceRaytracer`] so that it can be cheaply updated when the [`Space`] is
/// changed.
pub struct UpdatingSpaceRaytracer<D: RtBlockData> {
    space: Handle<Space>,
    graphics_options: listen::DynSource<Arc<GraphicsOptions>>,
    custom_options: listen::DynSource<Arc<D::Options>>,
    state: SpaceRaytracer<D>,
    todo: listen::StoreLock<SrtTodo>,
}

// manual impl avoids `D: Debug` bound
impl<D> fmt::Debug for UpdatingSpaceRaytracer<D>
where
    D: RtBlockData<Options: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UpdatingSpaceRaytracer")
            .field("space", &self.space)
            .field("graphics_options", &self.graphics_options)
            .field("custom_options", &self.custom_options)
            .field("state", &self.state)
            .field("todo", &self.todo)
            .finish()
    }
}

impl<D> UpdatingSpaceRaytracer<D>
where
    D: RtBlockData<Options: Clone + Sync + 'static>,
{
    /// Construct a new [`UpdatingSpaceRaytracer`] which follows the given `space`.
    ///
    /// The space is not accessed (and thus, nothing is updated) until the first call to
    /// [`update()`](Self::update). (This is done so that this constructor cannot fail and
    /// so the space is accessed on a consistent schedule.)
    pub fn new(
        space: Handle<Space>,
        graphics_options: listen::DynSource<Arc<GraphicsOptions>>,
        custom_options: listen::DynSource<Arc<D::Options>>,
    ) -> Self {
        let todo = listen::StoreLock::new(SrtTodo {
            listener: true,
            everything: true,
            blocks: Default::default(),
            cubes: Default::default(),
        });

        // TODO: Placeholder for more detailed graphics options updating
        graphics_options.listen(
            // TODO: this filter should be coalescing instead of having a large buffer
            todo.listener()
                .filter(|&()| Some(SpaceChange::EveryBlock))
                .with_stack_buffer::<1000>(),
        );

        Self {
            state: SpaceRaytracer::new_empty(
                // TODO: checkerboard sky
                space::Sky::Uniform(palette::NO_WORLD_TO_SHOW.to_rgb()),
                (*graphics_options.get()).clone(),
                (*custom_options.get()).clone(),
            ),
            space,
            graphics_options,
            custom_options,
            todo,
        }
    }

    /// Returns the [`Space`] this is synchronized with.
    pub fn space(&self) -> &Handle<Space> {
        &self.space
    }

    /// Returns the [`SpaceRaytracer`] that may be used for tracing.
    /// Its contents are current as of the last [`UpdatingSpaceRaytracer::update()`].
    pub fn get(&self) -> &SpaceRaytracer<D> {
        &self.state
    }

    /// Reads the previously provided [`Space`] and updates the local copy of its contents.
    ///
    /// On success, returns whether any of the scene actually changed.
    ///
    /// Returns an error if reading the [`Space`] fails.
    pub fn update(&mut self) -> Result<bool, HandleError> {
        // Deadlock safety note:
        // If the space is being updated, that will acquire the space's lock and then our
        // todo's lock for notifications. Therefore, to avoid deadlock we would need to
        // acquire the space's lock first. However, current threading policy is “there is
        // no blocking on acquiring Universe-member locks; you're expected to not need it”,
        // so no deadlock can actually occur. If we change this to block on the space lock,
        // we must reorder the actions here (or perhaps acquire the todo lock twice) to
        // avoid deadlock.
        let todo: &mut SrtTodo = &mut self.todo.lock();
        if todo.is_empty() {
            // Nothing to do
            return Ok(false);
        }
        let space = self.space.read()?;

        if mem::take(&mut todo.listener) {
            space.listen(self.todo.listener());
        }

        if mem::take(&mut todo.everything) {
            self.state = SpaceRaytracer::new(
                &space,
                (*self.graphics_options.get()).clone(),
                (*self.custom_options.get()).clone(),
            );
            todo.blocks.clear();
            todo.cubes.clear();

            Ok(true)
        } else {
            let mut anything_changed = false;

            // TODO: need to listen to the options sources for accurate change detection
            let graphics_options = &*self.graphics_options.get();
            let custom_options = &*self.custom_options.get();
            let options = RtOptionsRef {
                graphics_options,
                custom_options,
            };

            let block_data_slice = space.block_data();
            if block_data_slice.len() > self.state.blocks.len() {
                anything_changed = true;
                for block_data in block_data_slice[self.state.blocks.len()..].iter() {
                    self.state
                        .blocks
                        .push(TracingBlock::from_block(options, block_data));
                }
            }
            for block_index in todo.blocks.drain() {
                anything_changed = true;
                let block_index = usize::from(block_index);
                self.state.blocks[block_index] =
                    TracingBlock::from_block(options, &block_data_slice[block_index]);
            }

            for cube in todo.cubes.drain() {
                anything_changed = true;
                // TODO: this does 2 cube index calculations instead of the 1 it needs
                let block_index = space.get_block_index(cube).unwrap_or(0);
                self.state.cubes[cube] = TracingCubeData {
                    block_index,
                    lighting: space.get_lighting(cube),
                    always_invisible: block_data_slice[block_index as usize].block() == &AIR,
                };
            }

            Ok(anything_changed)
        }
    }
}

#[derive(Debug)]
struct SrtTodo {
    /// Listener upon the space is not yet installed.
    listener: bool,

    /// All blocks and cubes must be updated.
    everything: bool,

    // TODO: Benchmark using a BitVec instead.
    blocks: HbHashSet<BlockIndex>,
    cubes: HbHashSet<Cube>,
}

impl SrtTodo {
    #[mutants::skip] // hard to test, and designed to be hard to get wrong
    fn is_empty(&self) -> bool {
        let Self {
            listener,
            everything,
            blocks,
            cubes,
        } = self;
        !listener && !everything && blocks.is_empty() && cubes.is_empty()
    }
}

impl listen::Store<SpaceChange> for SrtTodo {
    fn receive(&mut self, messages: &[SpaceChange]) {
        for message in messages {
            match *message {
                SpaceChange::EveryBlock => {
                    self.everything = true;
                    self.blocks.clear();
                    self.cubes.clear()
                }
                SpaceChange::CubeLight { cube, .. } | SpaceChange::CubeBlock { cube, .. } => {
                    self.cubes.insert(cube);
                }
                SpaceChange::BlockIndex(index) | SpaceChange::BlockEvaluation(index) => {
                    self.blocks.insert(index);
                }
                SpaceChange::Physics => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::camera::{eye_for_look_at, Camera, Viewport};
    use crate::content::make_some_voxel_blocks;
    use crate::raytracer::{CharacterBuf, CharacterRtData};
    use crate::universe::Universe;
    use alloc::string::ToString;
    use euclid::{size2, vec3};
    use manyfmt::{formats::Unquote, Refmt};
    use pretty_assertions::assert_eq;

    struct EquivalenceTester {
        camera: Camera,
        space: Handle<Space>,
        graphics_options: listen::DynSource<Arc<GraphicsOptions>>,
        custom_options: listen::DynSource<Arc<()>>,
        updating: UpdatingSpaceRaytracer<CharacterRtData>,
    }

    impl EquivalenceTester {
        fn new(space: Handle<Space>) -> Self {
            let bounds = space.read().unwrap().bounds();

            // TODO: add tests of changing the options
            let graphics_options: listen::DynSource<_> =
                listen::constant(Arc::new(GraphicsOptions::default()));
            let custom_options: listen::DynSource<_> = listen::constant(Arc::new(()));

            let mut camera = Camera::new(
                (*graphics_options.get()).clone(),
                Viewport {
                    nominal_size: size2(72., 72.),
                    framebuffer_size: size2(72, 36),
                },
            );
            camera.look_at_y_up(eye_for_look_at(bounds, vec3(0., 0., 1.)), bounds.center());

            Self {
                updating: UpdatingSpaceRaytracer::new(
                    space.clone(),
                    graphics_options.clone(),
                    custom_options.clone(),
                ),
                camera,
                space,
                graphics_options,
                custom_options,
            }
        }

        fn update_and_assert(&mut self) -> Result<bool, HandleError> {
            self.camera
                .set_options((*self.graphics_options.get()).clone());
            let changed = self.updating.update()?;
            let image_updating = self
                .updating
                .get()
                .to_text::<CharacterBuf>(&self.camera, "\n")
                .to_string();
            #[expect(clippy::unit_arg)]
            let image_fresh = SpaceRaytracer::<CharacterRtData>::new(
                &self.space.read().unwrap(),
                (*self.graphics_options.get()).clone(),
                *self.custom_options.get(),
            )
            .to_text::<CharacterBuf>(&self.camera, "\n")
            .to_string();

            assert_eq!(image_updating.refmt(&Unquote), image_fresh.refmt(&Unquote));
            print!("{image_updating}");
            Ok(changed)
        }
    }

    #[test]
    fn updating_is_equivalent() {
        let mut universe = Universe::new();
        let mut space = Space::empty_positive(3, 2, 3);

        // Initial state
        let [block1, block2] = make_some_voxel_blocks(&mut universe);
        space.set([0, 0, 0], &block1).unwrap();

        let space = universe.insert_anonymous(space);
        let mut tester = EquivalenceTester::new(space.clone());

        tester.update_and_assert().unwrap();

        // Make some light changes
        space
            .try_modify(|space| space.fast_evaluate_light())
            .unwrap();
        tester.update_and_assert().unwrap();

        // Add a second block
        space
            .try_modify(|space| space.set([1, 0, 0], &block2).unwrap())
            .unwrap();
        tester.update_and_assert().unwrap();

        // Delete existing block
        space
            .try_modify(|space| space.set([0, 0, 0], &AIR).unwrap())
            .unwrap();
        tester.update_and_assert().unwrap();

        // TODO: Also test changing existing block's data
    }

    #[test]
    fn updating_after_space_is_unavailable() {
        let mut universe = Universe::new();
        let mut space = Space::empty_positive(3, 2, 3);

        // Initial state
        let [block1, block2] = make_some_voxel_blocks(&mut universe);
        space.set([0, 0, 0], &block1).unwrap();

        let space = universe.insert_anonymous(space);
        let mut tester = EquivalenceTester::new(space.clone());

        {
            let _obstruction = space.try_borrow_mut().unwrap();

            assert_eq!(
                tester.updating.update().unwrap_err(),
                HandleError::InUse(space.name().clone()),
            );
        }

        // Now after the failure, we should still successfully update.
        tester.update_and_assert().unwrap();

        // And also follow changes correctly.
        space
            .try_modify(|space| space.set([1, 0, 0], &block2).unwrap())
            .unwrap();
        tester.update_and_assert().unwrap();
    }
}
