//! Export to the glTF 3D file format.
//!
//! To use this, create a [`GltfWriter`].
//!
//! TODO: example code here
//!
//! TODO: This is not a clean, well-abstracted library API yet.

#![expect(clippy::module_name_repetitions)] // TODO: review all the naming in this module

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::PathBuf;
use std::time::Duration;
use std::{fmt, fs, io, mem};

// Special alias to reduce rebase conflicts — TODO: replace this with direct use of the new name
use ::gltf as gltf_json;
pub use ::gltf as json;
pub use ::gltf as gltf_lib;
use futures_core::future::BoxFuture;
use gltf_json::Index;

use all_is_cubes::block;
use all_is_cubes::universe::{Name, ReadTicket};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_mesh::{BlockMesh, MeshOptions, MeshTypes, SpaceMesh, block_meshes_for_space};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{Camera, GraphicsOptions, ViewTransform};

use crate::{ExportError, ExportSet, Format};

// -------------------------------------------------------------------------------------------------

mod buffer;
pub use buffer::GltfDataDestination;
use buffer::create_buffer_and_accessor;
mod animation;
use animation::FrameState;
mod mesh;
use mesh::MaterialKey;
mod glue;
use glue::convert_quaternion;
mod texture;
pub use texture::{GltfAtlasPoint, GltfTextureAllocator, GltfTexturePlane, GltfTile};
mod vertex;
pub use vertex::GltfVertex;

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// [`MeshTypes`] implementation for glTF output.
#[derive(Debug)]
#[expect(clippy::exhaustive_enums)]
pub enum GltfMt {}
impl MeshTypes for GltfMt {
    type Vertex = GltfVertex;
    type Alloc = GltfTextureAllocator;
    type Tile = GltfTile;
}

/// "This mesh with this translation." A value type that specifies that, in some frame
/// of the output, the particular mesh should be visible at a particular location.
///
/// These are then converted into [`gltf::Node`]s with animations controlling when they
/// are visible.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[expect(clippy::exhaustive_structs)]
pub struct MeshInstance {
    /// The mesh to display.
    pub mesh: Index<gltf::Mesh>,
    /// Translation applied to this instance of the mesh, in integer (whole block) amounts only.
    pub translation: [i32; 3],
}

/// Handles the construction of [`gltf::Root`] and the writing of supporting files
/// for a single glTF asset.
///
/// Life cycle:
/// 1. Create this (providing a [`GltfDataDestination`] for buffers and textures).
/// 2. Call methods to add entities that will be exported.
/// 3. Call [`GltfWriter::into_root()`] to obtain the main
///    [`gltf_json::Root`] value which should be written to the `.gltf` file.
//---
// TODO: Split this struct into "root and buffers" (knows glTF generically) and
// "scene and animation" (knows how we intend to use it). This will simplify some borrows.
#[derive(Debug)]
pub struct GltfWriter {
    /// Contains all the glTF entities written so far.
    /// Each operation that adds data appends to the vectors of entities inside this.
    /// Entities must not be deleted or reordered, to ensure [`Index`]es stay valid.
    root: gltf::Root,

    /// Where to write the buffers and textures.
    buffer_dest: GltfDataDestination,

    /// Texture allocator configured to write to `buffer_dest` too.
    texture_allocator: GltfTextureAllocator,

    /// When we create meshes that require textures (rather than vertex colors),
    /// we can’t know what texture coordinates their vertices should have,
    /// until we know the sizes of all tiles in the atlas.
    ///
    /// Such mesh data lives here, with special coordinates that identify the wanted tile,
    /// until the atlas is built. Their glTF buffers and accessors are present in `root` but have
    /// placeholder data.
    ///
    /// If a mesh uses vertex colors, it is not inserted here.
    meshes_awaiting_texture_coordinates: Vec<mesh::MeshAwaitingTextureCoordinates>,

    /// Interned materials.
    materials: HashMap<MaterialKey, Index<gltf_json::Material>>,

    /// glTF camera entity, if created yet.
    /// Its settings are taken from the first [`Camera`] encountered.
    camera: Option<Index<gltf::Camera>>,

    /// The state of the world in each frame of an animated scene.
    /// If its length is greater than 1, [`GltfWriter::into_root()`] will turn it into
    /// a glTF animation that hides and shows mesh instances.
    frame_states: Vec<FrameState>,

    /// Every mesh appearing anywhere in `frame_states`.
    /// Using [`BTreeSet`] for stable ordering.
    any_time_visible_mesh_instances: BTreeSet<MeshInstance>,

    /// All flaws encountered so far.
    flaws: Flaws,
}

impl GltfWriter {
    /// `buffer_dest`: Where to write auxiliary data (vertex buffers, textures).
    pub fn new(buffer_dest: GltfDataDestination) -> Self {
        let root = gltf_json::Root {
            asset: gltf_json::Asset {
                generator: Some(String::from("all-is-cubes")),
                ..gltf::Asset::default()
            },
            extensions_used: ["KHR_materials_transmission", "KHR_materials_volume"]
                .map(String::from)
                .to_vec(),
            ..gltf::Root::default()
        };

        Self {
            texture_allocator: GltfTextureAllocator::new(buffer_dest.clone()),

            root,
            buffer_dest,
            meshes_awaiting_texture_coordinates: Vec::new(),
            materials: HashMap::new(),
            camera: None,
            frame_states: Vec::new(),
            any_time_visible_mesh_instances: BTreeSet::new(),
            flaws: Flaws::empty(),
        }
    }

    /// Returns a texture [`Allocator`](all_is_cubes_mesh::texture::Allocator) that writes
    /// textures into this glTF asset
    pub fn texture_allocator(&self) -> GltfTextureAllocator {
        self.texture_allocator.clone()
    }

    /// Add one frame of an animated scene.
    ///
    /// `our_camera` should be the current camera state (its `view_transform`s in
    /// successive frames will be converted into an animation).
    ///
    /// `visible_meshes` is a list of [`MeshInstance`]s that should be visible in the
    /// current frame; the meshes should have been produced by previous calls to
    /// [`GltfWriter::add_mesh()`].
    ///
    /// Returns flaws which come from \[TODO: explain\].
    ///
    /// TODO: This is not a clean API yet; it was designed around the needs of
    /// `all-is-cubes-desktop`'s recording mode.
    pub fn add_frame(
        &mut self,
        our_camera: Option<&Camera>,
        visible_meshes: &[MeshInstance],
    ) -> Flaws {
        // Create camera if and only if one was given and we didn't have one.
        if self.camera.is_none()
            && let Some(our_camera) = our_camera.as_ref()
        {
            self.camera = Some(self.root.push(convert_camera(None, our_camera)));
        }

        self.frame_states.push(FrameState {
            visible_mesh_instances: visible_meshes.to_vec(),
            camera_transform: our_camera
                .map_or_else(ViewTransform::identity, |camera| camera.view_transform()),
        });
        self.any_time_visible_mesh_instances.extend(visible_meshes.iter());

        // TODO: report only flaws from this frame
        self.flaws
    }

    /// Add one [`SpaceMesh`] to the output.
    ///
    /// The mesh's texture allocator must be [`self.texture_allocator()`][Self::texture_allocator].
    pub fn add_mesh<M>(
        &mut self,
        name: &dyn fmt::Display,
        mesh: &SpaceMesh<M>,
    ) -> Option<Index<gltf::Mesh>>
    where
        M: MeshTypes<Vertex = GltfVertex, Alloc = GltfTextureAllocator>,
    {
        // TODO: Deduplicate meshes so that we don't have to store the same data twice if
        // a world change is undone, or in a cyclic animation (or if two chunks have the
        // same contents — once we make chunks in relative coordinates).
        mesh::add_mesh(self, name, mesh)
    }

    /// Finish all scene preparation and return the [`gltf::Root`] which is to be
    /// written to a JSON file.
    ///
    /// # Errors
    ///
    /// Returns an error if writing any of the buffer or image data fails.
    pub fn into_root(mut self, frame_pace: Duration) -> io::Result<gltf::Root> {
        if !self.texture_allocator.is_empty() {
            let (block_texture_index, uv_map) =
                texture::insert_block_texture_atlas(&mut self.root, &self.texture_allocator)?;

            debug_assert_eq!(
                block_texture_index,
                Index::new(0),
                "if we have multiple textures, then materials need work"
            );

            for mesh in mem::take(&mut self.meshes_awaiting_texture_coordinates) {
                mesh.finish(&uv_map, &mut self.root, &self.buffer_dest)?;
            }
        }

        let mut scene_nodes: Vec<Index<gltf::Node>> = Vec::new();

        // If we have a camera entity, create a node for it.
        if let Some(camera_index) = self.camera {
            let mut camera_node = gltf::Node {
                camera: Some(camera_index),
                ..Default::default()
            };
            if let Some(initial_state) = self.frame_states.first() {
                let t = initial_state.camera_transform;
                camera_node.translation = Some(t.translation.map(|c| c as f32).into());
                camera_node.rotation = Some(convert_quaternion(t.rotation));
            }
            let camera_node_index = self.root.push(camera_node);
            scene_nodes.push(camera_node_index);

            // Generate camera animation
            if self.frame_states.len() > 1 {
                animation::add_camera_animation(&mut self, camera_node_index, frame_pace)?;
            }
        }

        // For each needed mesh instance, create a node with that translation and that mesh.
        let mut instance_nodes: BTreeMap<MeshInstance, Index<gltf::Node>> = BTreeMap::new();
        for &instance in self.any_time_visible_mesh_instances.iter() {
            let MeshInstance { mesh, translation } = instance;
            let node_index = self.root.push(gltf::Node {
                mesh: Some(mesh),
                translation: Some(translation.map(|c| c as f32)),
                // TODO: give this node a name if we can figure out what a good, cheap one is
                ..Default::default()
            });
            instance_nodes.insert(instance, node_index);
            scene_nodes.push(node_index);
        }

        // Add world mesh animations.
        if self.frame_states.len() > 1 {
            // Timeline represented as BTreeMap<node, Vec<(frame number, visibility)>>.
            // The initial state is "visible", so any nonanimated mesh needs no entry.
            let mut timelines: BTreeMap<MeshInstance, Vec<(usize, bool)>> = BTreeMap::new();
            for (frame_number, state) in self.frame_states.iter().enumerate() {
                for &instance in &state.visible_mesh_instances {
                    let timeline = timelines.entry(instance).or_default();
                    if !timeline.last().is_none_or(|&(_, vis)| vis) {
                        // Node needs to be made visible.
                        timeline.push((frame_number, true));
                    }
                }
                // Remove invisible instances (including ones we haven't seen at all yet)
                for &instance in self.any_time_visible_mesh_instances.iter() {
                    if state.visible_mesh_instances.contains(&instance) {
                        // TODO: do a map lookup instead of linear scan?
                        continue;
                    }

                    use std::collections::btree_map::Entry;
                    match timelines.entry(instance) {
                        Entry::Occupied(mut e) => {
                            let timeline = e.get_mut();
                            if timeline.last().is_none_or(|&(_, vis)| vis) {
                                // Node needs to be made invisible.
                                timeline.push((frame_number, false));
                            }
                        }
                        #[expect(clippy::missing_panics_doc, reason = "sanity check")]
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

            for (instance, timeline) in timelines {
                if timeline.is_empty() {
                    continue;
                }
                let node_index = instance_nodes[&instance];
                let time_accessor = create_buffer_and_accessor(
                    &mut self.root,
                    &self.buffer_dest,
                    format!("node {node_index} animation time"),
                    &format!("node-{node_index}-time"),
                    timeline.iter().map(|&(t, _vis)| [frame_pace.as_secs_f32() * t as f32]),
                )?;
                let scale_accessor = create_buffer_and_accessor(
                    &mut self.root,
                    &self.buffer_dest,
                    format!("node {node_index} visibility"),
                    &format!("node-{node_index}-vis"),
                    timeline.iter().map(|&(_t, vis)| [f32::from(u8::from(vis)); 3]),
                )?;
                animation_channels.push(gltf::animation::Channel {
                    sampler: Index::push(
                        &mut animation_samplers,
                        gltf::animation::Sampler {
                            input: time_accessor,
                            interpolation: gltf::animation::Interpolation::Step,
                            output: scale_accessor,
                            unrecognized_extensions: Default::default(),
                            extras: Default::default(),
                        },
                    ),
                    target: gltf::animation::Target {
                        node: Some(node_index),
                        path: gltf::animation::Property::Scale,
                        pointer: None,
                        unrecognized_extensions: Default::default(),
                        extras: Default::default(),
                    },
                    unrecognized_extensions: Default::default(),
                    extras: Default::default(),
                });
            }

            // Generate animation. Spec requires animation to be nonempty.
            if !animation_channels.is_empty() {
                self.root.push(gltf::Animation {
                    name: Some("world changes".into()),
                    channels: animation_channels,
                    samplers: animation_samplers,
                    unrecognized_extensions: Default::default(),
                    extras: Default::default(),
                });
            }
        }

        if !scene_nodes.is_empty() {
            self.root.scenes.push(gltf::Scene {
                name: Some("recording".into()),
                nodes: scene_nodes,
                extras: Default::default(),
                unrecognized_extensions: Default::default(),
            });
        }

        Ok(self.root)
    }

    fn intern_material(&mut self, key: MaterialKey) -> Index<gltf_json::Material> {
        use std::collections::hash_map::Entry;
        match self.materials.entry(key) {
            Entry::Occupied(oe) => *oe.get(),
            Entry::Vacant(ve) => *ve.insert(Index::push(
                &mut self.root.materials,
                key.to_material_definition(),
            )),
        }
    }
}

// The funny return type is to work with [`crate::export_to_path`].
pub(crate) fn export_gltf(
    progress: YieldProgress,
    read_ticket: ReadTicket<'_>,
    mut source: ExportSet,
    destination: PathBuf,
) -> Result<BoxFuture<'static, Result<(), ExportError>>, ExportError> {
    let block_defs = source.contents.extract_type::<block::BlockDef>();
    let spaces = source.contents.extract_type::<all_is_cubes::space::Space>();
    source.reject_unsupported(Format::Gltf)?;

    let mut writer = GltfWriter::new(GltfDataDestination::new(Some(destination.clone()), 2000));
    let mesh_options = MeshOptions::new(&GraphicsOptions::default());

    // Fetch data from `source` synchronously.
    let block_evaluations: Vec<(Name, block::EvaluatedBlock)> = block_defs
        .into_iter()
        .map(|block_def_handle| -> Result<_, ExportError> {
            let block_def = block_def_handle.read(read_ticket)?;
            let name = block_def_handle.name();
            let evaluation = block_def.evaluate().map_err(|eve| ExportError::NotRepresentable {
                format: Format::Gltf,
                name: Some(name.clone()),
                reason: format!("block evaluation failed: {eve}"),
            })?;
            Ok((name, evaluation))
        })
        .collect::<Result<_, ExportError>>()?;
    let space_meshes: Vec<(Name, SpaceMesh<GltfMt>, [f32; 3])> = spaces
        .into_iter()
        .map(|space_handle| -> Result<_, ExportError> {
            let name = space_handle.name();
            let space = &space_handle.read(read_ticket)?;
            let block_meshes =
                block_meshes_for_space::<GltfMt>(space, &writer.texture_allocator(), &mesh_options);
            let mesh: SpaceMesh<GltfMt> =
                SpaceMesh::new(space, space.bounds(), &mesh_options, &block_meshes[..]);
            let translation: [f32; 3] = space.bounds().lower_bounds().to_f32().into();

            Ok((name, mesh, translation))
        })
        .collect::<Result<_, ExportError>>()?;

    Ok(Box::pin(async move {
        let [block_def_progress, space_progress] = progress.split(0.5); // TODO: ratio

        // TODO: deduplicate these two extremely similar loops

        {
            let mut block_nodes: Vec<Index<gltf_json::Node>> =
                Vec::with_capacity(block_evaluations.len());
            for (index, (mut p, (name, evaluation))) in block_def_progress
                .split_evenly(block_evaluations.len())
                .zip(block_evaluations)
                .enumerate()
            {
                p.set_label(&name);
                p.progress(0.01).await;
                {
                    let mesh = SpaceMesh::<GltfMt>::from(&BlockMesh::new(
                        &evaluation,
                        &writer.texture_allocator(),
                        &mesh_options,
                    ));

                    let mesh_index = writer.add_mesh(&name, &mesh);
                    // TODO: if the mesh is empty/None, should we include the node anyway or not?
                    let mesh_node = writer.root.push(gltf_json::Node {
                        name: Some(name.to_string()),
                        mesh: mesh_index,
                        // translate each block so they are spaced out evenly for viewing
                        translation: Some([index as f32 * 2.0, 0.0, 0.0]),
                        ..Default::default()
                    });

                    block_nodes.push(mesh_node);
                }

                p.finish().await;
            }

            // A scene that can be used to view all blocks.
            // (Using multiple scenes would be less opinionated, but in my experience, glTF viewers
            // often do not support viewing multiple scenes at all.)
            if !block_nodes.is_empty() {
                writer.root.scenes.push(json::Scene {
                    name: Some("block preview scene".into()),
                    nodes: block_nodes,
                    unrecognized_extensions: Default::default(),
                    extras: Default::default(),
                });
            }
        }

        for (mut p, (name, mesh, translation)) in
            space_progress.split_evenly(space_meshes.len()).zip(space_meshes)
        {
            p.set_label(&name);
            p.progress(0.01).await;
            {
                let mesh_index = writer.add_mesh(&name, &mesh);
                let mesh_node = writer.root.push(gltf_json::Node {
                    name: Some(name.to_string()),
                    mesh: mesh_index,
                    // SpaceMesh translates everything so the lower bounds of the requested region
                    // are at [0, 0, 0], so we must undo that.
                    translation: Some(translation),
                    ..Default::default()
                });

                writer.root.scenes.push(json::Scene {
                    name: Some(format!("{name} space scene")),
                    nodes: vec![mesh_node],
                    unrecognized_extensions: Default::default(),
                    extras: Default::default(),
                });
            }

            p.finish().await;
        }

        {
            let file = fs::File::create(destination)?;
            serde_json::to_writer_pretty(&file, &writer.into_root(Duration::from_secs(1))?) // TODO: non-pretty option
                .map_err(|_| -> ExportError { todo!("serialization error conversion") })?;
            file.sync_all()?;
        }

        Ok(())
    }))
}

/// Construct gltf camera entity.
/// Note that this is not complete since it does not contain the viewpoint; a node is also needed.
fn convert_camera(name: Option<String>, camera: &Camera) -> gltf::Camera {
    gltf::Camera {
        name,
        projection: gltf::camera::Projection::Perspective {
            perspective: gltf::camera::Perspective {
                aspect_ratio: Some(camera.viewport().nominal_aspect_ratio() as f32),
                yfov: camera.options().fov_y.into_inner() as f32 * (std::f32::consts::PI / 180.),
                zfar: Some(camera.options().view_distance.into_inner() as f32),
                znear: camera.near_plane_distance().into_inner() as f32,
                unrecognized_extensions: Default::default(),
                extras: Default::default(),
            },
        },
        unrecognized_extensions: Default::default(),
        extras: Default::default(),
    }
}
