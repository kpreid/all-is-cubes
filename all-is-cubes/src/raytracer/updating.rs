use super::*;

use std::collections::HashSet;
use std::mem;
use std::sync::{Arc, Mutex, Weak};

use crate::block::AIR;
use crate::listen::{ListenableSource, Listener};
use crate::space::SpaceChange;
use crate::universe::{RefError, URef};

/// Manages a [`SpaceRaytracer`] so that it can be cheaply updated when the [`Space`] is
/// changed.
#[derive(Debug)]
pub struct UpdatingSpaceRaytracer<D: RtBlockData> {
    space: URef<Space>,
    graphics_options: ListenableSource<GraphicsOptions>,
    custom_options: ListenableSource<D::Options>,
    state: SpaceRaytracer<D>,
    todo: Arc<Mutex<SrtTodo>>,
}

impl<D: RtBlockData> UpdatingSpaceRaytracer<D>
where
    D::Options: Clone + Sync + 'static,
{
    // TODO: document args
    pub fn new(
        space: URef<Space>,
        graphics_options: ListenableSource<GraphicsOptions>,
        custom_options: ListenableSource<D::Options>,
    ) -> Self {
        // TODO: don't unconditionally borrow and maybe panic on the first call
        let todo = Arc::default();

        let space_borrowed = space.borrow();
        space_borrowed.listen(TodoListener(Arc::downgrade(&todo)));

        // TODO: Placeholder for more detailed graphics options updating
        graphics_options
            .listen(TodoListener(Arc::downgrade(&todo)).filter(|()| Some(SpaceChange::EveryBlock)));

        Self {
            state: SpaceRaytracer::new(
                &space_borrowed,
                graphics_options.snapshot(),
                custom_options.snapshot(),
            ),
            space,
            graphics_options,
            custom_options,
            todo,
        }
    }

    pub fn get(&self) -> &SpaceRaytracer<D> {
        &self.state
    }

    pub fn update(&mut self) -> Result<(), RefError> {
        // Deadlock safety note:
        // If the space is being updated, that will acquire the space's lock and then our
        // todo's lock for notifications. Therefore, to avoid deadlock we would need to
        // acquire the space's lock first. However, current threading policy is “there is
        // no blocking on acquiring Universe-member locks; you're expected to not need it”,
        // so no deadlock can actually occur. If we change this to block on the space lock,
        // we must reorder the actions here (or perhaps acquire the todo lock twice) to
        // avoid deadlock.
        let mut todo = self.todo.lock().unwrap();
        if !todo.everything && todo.blocks.is_empty() && todo.cubes.is_empty() {
            // Nothing to do
            return Ok(());
        }
        let space = self.space.try_borrow()?;

        if mem::take(&mut todo.everything) {
            self.state = SpaceRaytracer::new(
                &space,
                self.graphics_options.snapshot(),
                self.custom_options.snapshot(),
            );
            todo.blocks.clear();
            todo.cubes.clear();
        } else {
            let graphics_options = &*self.graphics_options.get();
            let custom_options = &*self.custom_options.get();
            let options = RtOptionsRef {
                graphics_options,
                custom_options,
            };

            let block_data_slice = space.block_data();
            if block_data_slice.len() > self.state.blocks.len() {
                for block_data in block_data_slice[self.state.blocks.len()..].iter() {
                    self.state
                        .blocks
                        .push(TracingBlock::from_block(options, block_data));
                }
            }
            for block_index in todo.blocks.drain() {
                // TODO: handle extending the vector
                let block_index = usize::from(block_index);
                self.state.blocks[block_index] =
                    TracingBlock::from_block(options, &block_data_slice[block_index]);
            }

            for cube in todo.cubes.drain() {
                // TODO: this does 2 cube index calculations instead of the 1 it needs
                let block_index = space.get_block_index(cube).unwrap_or(0);
                self.state.cubes[cube] = TracingCubeData {
                    block_index,
                    lighting: space.get_lighting(cube),
                    always_invisible: block_data_slice[block_index as usize].block() == &AIR,
                };
            }
        }

        Ok(())
    }
}

#[derive(Debug, Default)]
struct SrtTodo {
    everything: bool,
    // TODO: Benchmark using a BitVec instead.
    blocks: HashSet<BlockIndex>,
    cubes: HashSet<GridPoint>,
}

/// [`Listener`] adapter for [`SpaceRendererTodo`].
#[derive(Clone, Debug)]
struct TodoListener(Weak<Mutex<SrtTodo>>);

impl Listener<SpaceChange> for TodoListener {
    fn receive(&self, message: SpaceChange) {
        if let Some(mutex) = self.0.upgrade() {
            if let Ok(mut todo) = mutex.lock() {
                match message {
                    SpaceChange::EveryBlock => {
                        todo.everything = true;
                        todo.blocks.clear();
                        todo.cubes.clear()
                    }
                    SpaceChange::Lighting(p) | SpaceChange::Block(p) => {
                        todo.cubes.insert(p);
                    }
                    SpaceChange::Number(index) | SpaceChange::BlockValue(index) => {
                        todo.blocks.insert(index);
                    }
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::AIR;
    use crate::camera::{eye_for_look_at, Viewport};
    use crate::content::make_some_voxel_blocks;
    use crate::universe::Universe;
    use crate::util::{CustomFormat, Unquote};
    use cgmath::{Decomposed, Transform as _};
    use pretty_assertions::assert_eq;

    struct EquivalenceTester {
        camera: Camera,
        space: URef<Space>,
        graphics_options: ListenableSource<GraphicsOptions>,
        custom_options: ListenableSource<()>,
        updating: UpdatingSpaceRaytracer<CharacterRtData>,
    }

    impl EquivalenceTester {
        fn new(space: URef<Space>) -> Self {
            let grid = space.borrow().grid();

            // TODO: add tests of changing the options
            let graphics_options = ListenableSource::constant(GraphicsOptions::default());
            let custom_options = ListenableSource::constant(());

            let mut camera = Camera::new(
                graphics_options.snapshot(),
                Viewport {
                    nominal_size: Vector2::new(72., 72.),
                    framebuffer_size: Vector2::new(72, 36),
                },
            );
            camera.set_view_transform(
                Decomposed::look_at_rh(
                    eye_for_look_at(grid, Vector3::unit_z()),
                    grid.center(),
                    Vector3::new(0., 1., 0.),
                )
                .inverse_transform()
                .unwrap(),
            );

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

        fn update_and_assert(&mut self) -> Result<(), RefError> {
            self.camera.set_options(self.graphics_options.snapshot());
            self.updating.update()?;
            let image_updating = self
                .updating
                .get()
                .trace_scene_to_string::<CharacterBuf>(&self.camera, "\n");
            #[allow(clippy::unit_arg)]
            let image_fresh = SpaceRaytracer::<CharacterRtData>::new(
                &self.space.borrow(),
                self.graphics_options.snapshot(),
                self.custom_options.snapshot(),
            )
            .trace_scene_to_string::<CharacterBuf>(&self.camera, "\n");

            assert_eq!(
                image_updating.custom_format(Unquote),
                image_fresh.custom_format(Unquote)
            );
            print!("{}", image_updating);
            Ok(())
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
}
