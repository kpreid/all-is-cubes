//! Export to the glTF 3D file format.
//!
//! To use this, create a [`GltfWriter`].
//!
//! TODO: example code here
//!
//! TODO: This is not a clean, well-abstracted library API yet.

use std::collections::{BTreeMap, BTreeSet};
use std::io;
use std::time::Duration;

use all_is_cubes::math::GridCoordinate;
pub use gltf_json as json;
use gltf_json::validation::Checked::Valid;
use gltf_json::Index;

use all_is_cubes::camera::{Camera, Flaws, ViewTransform};
use all_is_cubes::cgmath::{One as _, Vector3};
use all_is_cubes_mesh::SpaceMesh;

mod buffer;
use buffer::create_buffer_and_accessor;
pub use buffer::GltfDataDestination;
mod animation;
use animation::FrameState;
mod mesh;
use mesh::{add_mesh, Materials};
mod glue;
use glue::{convert_quaternion, empty_node, push_and_return_index};
mod texture;
pub use texture::{GltfTextureAllocator, GltfTextureRef};
mod vertex;
pub use vertex::GltfVertex;

/// Handles the construction of [`gltf_json::Root`] and the writing of supporting files
/// for a single glTF asset.
///
/// Life cycle:
/// 1. Create this (providing a [`GltfDataDestination`] for buffers and textures).
/// 2. Call methods to add entities that will be exported.
/// 3. Call [`GltfWriter::into_root()`] to obtain the main
/// [`gltf_json::Root`] value which should be written to the `.gltf` file.
///
/// TODO: Split this struct into "root and buffers" (knows glTF generically) and
/// "scene and animation" (knows how we intend to use it). This will simplify some borrows.
#[derive(Debug)]
pub struct GltfWriter {
    /// Contains all the glTF entities written so far.
    /// Each operation that adds data appends to the vectors of entities inside this.
    /// Entities must not be deleted or reordered, to ensure [`Index`]es stay valid.
    root: gltf_json::Root,

    /// Where to write the buffers and textures.
    buffer_dest: GltfDataDestination,

    /// Testure allocator configured to write to this destination.
    texture_allocator: GltfTextureAllocator,

    /// Materials the meshes need.
    materials: Materials,

    /// glTF camera entity, if created yet.
    /// Its settings are taken from the first [`Camera`] encountered.
    camera: Option<Index<gltf_json::Camera>>,

    /// The state of the world in each frame of the animation.
    frame_states: Vec<FrameState>,

    /// Every mesh index appearing anywhere in `frame_states`.
    /// Using BTreeSet for stable ordering.
    any_time_visible_mesh_nodes: BTreeSet<Index<gltf_json::Node>>,

    /// All flaws encountered so far.
    flaws: Flaws,
}

impl GltfWriter {
    /// `buffer_dest`: Where to write auxiliary data (vertex buffers, textures).
    pub fn new(buffer_dest: GltfDataDestination) -> Self {
        let mut root = gltf_json::Root {
            asset: gltf_json::Asset {
                generator: Some(String::from("all-is-cubes")),
                ..gltf_json::Asset::default()
            },
            extensions_used: ["KHR_materials_transmission", "KHR_materials_volume"]
                .map(String::from)
                .to_vec(),
            ..gltf_json::Root::default()
        };

        Self {
            materials: Materials::new(&mut root.materials),
            texture_allocator: GltfTextureAllocator::new(buffer_dest.clone()),

            root,
            buffer_dest,
            camera: None,
            frame_states: Vec::new(),
            any_time_visible_mesh_nodes: BTreeSet::new(),
            flaws: Flaws::empty(),
        }
    }

    /// Returns a [`TextureAllocator`](all_is_cubes_mesh::TextureAllocator) that writes
    /// textures into this glTF asset
    pub fn texture_allocator(&self) -> GltfTextureAllocator {
        self.texture_allocator.clone()
    }

    /// Add one frame of an animated scene.
    ///
    /// `our_camera` should be the current camera state (its `view_transform`s in
    /// successive frames will be converted into an animation).
    ///
    /// `visible_nodes` is a list of every node that should be visible in the current scene,
    /// which should have been produced by previous calls to [`GltfWriter::add_mesh()`].
    ///
    /// Returns flaws which come from
    ///
    /// TODO: This is not a clean API yet; it was designed around the needs of
    /// `all-is-cubes-desktop`'s recording mode.
    pub fn add_frame(
        &mut self,
        our_camera: Option<&Camera>,
        visible_nodes: &[Index<gltf_json::Node>],
    ) -> Flaws {
        // Create camera if and only if one was given and we didn't have one.
        if self.camera.is_none() {
            if let Some(our_camera) = our_camera.as_ref() {
                self.camera = Some(push_and_return_index(
                    &mut self.root.cameras,
                    convert_camera(None, our_camera),
                ));
            }
        }

        self.frame_states.push(FrameState {
            visible_nodes: visible_nodes.to_vec(),
            camera_transform: our_camera
                .map_or_else(ViewTransform::one, |camera| camera.get_view_transform()),
        });
        self.any_time_visible_mesh_nodes
            .extend(visible_nodes.iter());

        // TODO: report only flaws from this frame
        self.flaws
    }

    /// Add one [`SpaceMesh`], with a containing node, and return its index.
    ///
    /// The mesh's texture allocator must be [`self.texture_allocator()`].
    pub fn add_mesh(
        &mut self,
        name: String,
        mesh: &SpaceMesh<GltfVertex, GltfTextureRef>,
        translation: Vector3<GridCoordinate>,
    ) -> Index<gltf_json::Node> {
        // TODO: Deduplicate meshes so that we don't have to store the same data twice if
        // a world change is undone, or in a cyclic animation (or if two chunks have the
        // same contents — once we make chunks in relative coordinates).
        let mesh_index = add_mesh(self, name.clone(), mesh);

        push_and_return_index(
            &mut self.root.nodes,
            gltf_json::Node {
                mesh: Some(mesh_index),
                translation: Some(translation.map(|c| c as f32).into()),
                ..empty_node(Some(name))
            },
        )
    }

    /// Finish all scene preparation and return the [`gltf_json::Root`] which is to be
    /// written to a JSON file.
    pub fn into_root(mut self, frame_pace: Duration) -> io::Result<gltf_json::Root> {
        let mut scene_nodes: Vec<Index<gltf_json::Node>> = Vec::new();

        // If we have a camera entity, create a node for it.
        if let Some(camera_index) = self.camera {
            let mut camera_node = gltf_json::Node {
                camera: Some(camera_index),
                ..empty_node(None)
            };
            if let Some(initial_state) = self.frame_states.get(0) {
                let t = initial_state.camera_transform;
                camera_node.translation = Some(t.disp.map(|c| c as f32).into());
                camera_node.rotation = Some(convert_quaternion(t.rot));
                camera_node.scale = Some([t.scale as f32; 3]);
            }
            let camera_node_index = push_and_return_index(&mut self.root.nodes, camera_node);
            scene_nodes.push(camera_node_index);

            // Generate camera animation
            if self.frame_states.len() > 1 {
                animation::add_camera_animation(&mut self, camera_node_index, frame_pace)?;
            }
        }

        // Attach *all* visible nodes to the scene.
        scene_nodes.extend(self.any_time_visible_mesh_nodes.iter());

        // Add world mesh animations.
        if self.frame_states.len() > 1 {
            // Timeline represented as BTreeMap<node, Vec<(frame number, visibility)>>.
            // The initial state is "visible", so any nonanimated mesh needs no entry.
            let mut timelines: BTreeMap<Index<gltf_json::Node>, Vec<(usize, bool)>> =
                BTreeMap::new();
            for (frame_number, state) in self.frame_states.iter().enumerate() {
                for &node_index in &state.visible_nodes {
                    let timeline = timelines.entry(node_index).or_default();
                    if !timeline.last().map_or(true, |&(_, vis)| vis) {
                        // Node needs to be made visible.
                        timeline.push((frame_number, true));
                    }
                }
                // Remove invisible meshes (including ones we haven't seen at all yet)
                for &node_index in self.any_time_visible_mesh_nodes.iter() {
                    if state.visible_nodes.contains(&node_index) {
                        // TODO: do a map lookup instead of linear scan?
                        continue;
                    }

                    use std::collections::btree_map::Entry;
                    match timelines.entry(node_index) {
                        Entry::Occupied(mut e) => {
                            let timeline = e.get_mut();
                            if timeline.last().map_or(true, |&(_, vis)| vis) {
                                // Node needs to be made invisible.
                                timeline.push((frame_number, false));
                            }
                        }
                        Entry::Vacant(e) => {
                            // Node needs to be made invisible *from the start*.
                            // This should always happen on the first frame
                            assert_eq!(frame_number, 0, "Neglected initial invisibility");
                            e.insert(vec![(frame_number, false)]);
                        }
                    }
                }
            }

            let mut animation_channels = Vec::new();
            let mut animation_samplers = Vec::new();

            for (node_index, timeline) in timelines {
                if timeline.is_empty() {
                    continue;
                }
                let time_accessor = create_buffer_and_accessor(
                    &mut self.root,
                    &mut self.buffer_dest,
                    format!("node {node_index} animation time"),
                    &format!("node-{node_index}-time"),
                    timeline
                        .iter()
                        .map(|&(t, _vis)| [frame_pace.as_secs_f32() * t as f32]),
                )?;
                let scale_accessor = create_buffer_and_accessor(
                    &mut self.root,
                    &mut self.buffer_dest,
                    format!("node {node_index} visibility"),
                    &format!("node-{node_index}-vis"),
                    timeline
                        .iter()
                        .map(|&(_t, vis)| [f32::from(u8::from(vis)); 3]),
                )?;
                animation_channels.push(gltf_json::animation::Channel {
                    sampler: push_and_return_index(
                        &mut animation_samplers,
                        gltf_json::animation::Sampler {
                            input: time_accessor,
                            interpolation: Valid(gltf_json::animation::Interpolation::Step),
                            output: scale_accessor,
                            extensions: Default::default(),
                            extras: Default::default(),
                        },
                    ),
                    target: gltf_json::animation::Target {
                        node: node_index,
                        path: Valid(gltf_json::animation::Property::Scale),
                        extensions: Default::default(),
                        extras: Default::default(),
                    },
                    extensions: Default::default(),
                    extras: Default::default(),
                });
            }

            // Generate animation. Spec requires animation to be nonempty.
            if !animation_channels.is_empty() {
                push_and_return_index(
                    &mut self.root.animations,
                    gltf_json::Animation {
                        name: Some("world changes".into()),
                        channels: animation_channels,
                        samplers: animation_samplers,
                        extensions: Default::default(),
                        extras: Default::default(),
                    },
                );
            }
        }

        self.root.scenes.push(gltf_json::Scene {
            name: Some("recording".into()),
            nodes: scene_nodes,
            extras: Default::default(),
            extensions: None,
        });

        Ok(self.root)
    }
}

/// Construct gltf camera entity.
/// Note that this is not complete since it does not contain the viewpoint; a node is also needed.
fn convert_camera(name: Option<String>, camera: &Camera) -> gltf_json::Camera {
    gltf_json::Camera {
        name,
        type_: Valid(gltf_json::camera::Type::Perspective),
        orthographic: None,
        perspective: Some(gltf_json::camera::Perspective {
            aspect_ratio: Some(camera.viewport().nominal_aspect_ratio() as f32),
            yfov: camera.options().fov_y.into_inner() as f32 * (std::f32::consts::PI / 180.),
            zfar: Some(camera.options().view_distance.into_inner() as f32),
            znear: 1. / 32., // TODO: expose this from `Camera`
            extensions: Default::default(),
            extras: Default::default(),
        }),
        extensions: Default::default(),
        extras: Default::default(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::{Block, Resolution, AIR};
    use all_is_cubes::camera::GraphicsOptions;
    use all_is_cubes::cgmath::Zero as _;
    use all_is_cubes::content::make_some_blocks;
    use all_is_cubes::space::Space;
    use all_is_cubes::universe::Universe;
    use all_is_cubes_mesh::{block_meshes_for_space, MeshOptions, SpaceMesh};
    use gltf_json::validation::Validate;

    /// Test helper to insert one mesh+node
    pub(crate) fn gltf_mesh(
        space: &Space,
        writer: &mut GltfWriter,
    ) -> (
        SpaceMesh<GltfVertex, GltfTextureRef>,
        Index<gltf_json::Node>,
    ) {
        let options = &MeshOptions::new(&GraphicsOptions::default());
        let blocks = block_meshes_for_space(space, &writer.texture_allocator(), options);
        let mesh: SpaceMesh<GltfVertex, GltfTextureRef> =
            SpaceMesh::new(space, space.bounds(), options, &*blocks);

        let index = writer.add_mesh("mesh".into(), &mesh, Vector3::zero());

        (mesh, index)
    }

    #[test]
    fn gltf_smoke_test() {
        // Construct recursive block.
        let resolution = Resolution::R4;
        let mut u = Universe::new();
        let mut blocks = Vec::from(make_some_blocks::<2>());
        blocks.push(AIR);
        let recursive_block = Block::builder()
            .voxels_fn(&mut u, resolution, |p| {
                &blocks[(p.x as usize).rem_euclid(blocks.len())]
            })
            .unwrap()
            .build();
        let mut outer_space = Space::empty_positive(1, 1, 1);
        outer_space.set((0, 0, 0), &recursive_block).unwrap();

        let mut writer = GltfWriter::new(GltfDataDestination::null());
        let (_, mesh_index) = gltf_mesh(&outer_space, &mut writer);
        writer.add_frame(None, &[mesh_index]);

        let root = writer.into_root(Duration::ZERO).unwrap();

        println!(
            "{}",
            serde_json::to_string_pretty(&root).expect("serialization failed")
        );

        // TODO: better way to call validate()?
        root.validate(&root, gltf_json::Path::new, &mut |pf, error| {
            panic!("{path} {error}", path = pf())
        });
    }
}
