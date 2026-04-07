use std::collections::BTreeMap;
use std::fmt;
use std::io;
use std::mem::{offset_of, size_of};

use gltf_json::Index;
use gltf_json::buffer::Stride;
use gltf_json::validation::Checked::Valid;
use gltf_json::validation::USize64;

use all_is_cubes::math::range_len;
use all_is_cubes_mesh::{IndexSlice, MeshTypes, SpaceMesh};

use crate::gltf::glue::create_accessor;
use crate::gltf::{GltfTextureAllocator, GltfVertex, GltfWriter};

// -------------------------------------------------------------------------------------------------

/// Create [`gltf_json::Mesh`] and all its parts (accessors, buffers) from a [`SpaceMesh`].
///
/// If the mesh requires textures, it will contain partially placeholder data until the
/// texture atlas is finalized.
///
/// If the input is empty, does nothing and returns `None`.
pub(crate) fn add_mesh<M>(
    writer: &mut GltfWriter,
    name: &dyn fmt::Display,
    mesh: &SpaceMesh<M>,
) -> Option<Index<gltf_json::Mesh>>
where
    // TODO: This generic bound (rather than `SpaceMesh<GltfMt>`) is a workaround to allow
    // `all-is-cubes-port` to define its own `DynamicMeshTypes`. This is a sign that maybe
    // `DynamicMeshTypes` is designed wrong.
    M: MeshTypes<Vertex = GltfVertex, Alloc = GltfTextureAllocator>,
{
    if mesh.is_empty() {
        return None;
    }

    let needs_texture = mesh.vertices().0.iter().any(GltfVertex::needs_uv_mapping);
    let index_type = match mesh.indices() {
        IndexSlice::U16(_) => gltf_json::accessor::ComponentType::U16,
        IndexSlice::U32(_) => gltf_json::accessor::ComponentType::U32,
    };
    let vertices_byte_len = bytemuck::cast_slice::<GltfVertex, u8>(mesh.vertices().0).len();
    let indices_byte_len = mesh.indices().as_bytes().len();
    let buffer_object_name = format!("{name} data");
    let buffer_file_suffix = format!("mesh-{i}", i = writer.root.buffers.len());

    // Push the vertex & index buffer early.
    // If we are doing texture coordinates, we need to fill this in later with texture coordinates
    // from the texture atlas, but have the object index now.
    let buffer_index = writer.root.push(gltf_json::Buffer {
        byte_length: USize64::from(vertices_byte_len.strict_add(indices_byte_len)),
        name: Some(buffer_object_name.clone()),
        uri: None, // to be filled in later
        extensions: Default::default(),
        extras: Default::default(),
    });

    let vertex_buffer_view = writer.root.push(gltf_json::buffer::View {
        buffer: buffer_index,
        byte_length: USize64::from(vertices_byte_len),
        byte_offset: None,
        byte_stride: Some(Stride(size_of::<GltfVertex>())),
        name: Some(format!("{name} vertex")),
        target: Some(Valid(gltf_json::buffer::Target::ArrayBuffer)),
        extensions: Default::default(),
        extras: Default::default(),
    });
    let index_buffer_view = writer.root.push(gltf_json::buffer::View {
        buffer: buffer_index,
        byte_length: USize64::from(indices_byte_len),
        // Indexes are packed into the same buffer, so they start at the end of the vertex bytes
        byte_offset: Some(USize64::from(vertices_byte_len)),
        byte_stride: None,
        name: Some(format!("{name} index")),
        // ElementArrayBuffer means index buffer
        target: Some(Valid(gltf_json::buffer::Target::ElementArrayBuffer)),
        extensions: Default::default(),
        extras: Default::default(),
    });

    let mut attributes = BTreeMap::from([
        (
            Valid(gltf_json::mesh::Semantic::Positions),
            writer.root.push(create_accessor(
                format!("{name} position"),
                vertex_buffer_view,
                offset_of!(GltfVertex, position),
                mesh.vertices().0.iter().map(|v| v.position.map(f32::from)),
            )),
        ),
        (
            Valid(gltf_json::mesh::Semantic::Colors(0)),
            writer.root.push(create_accessor(
                format!("{name} base color"),
                vertex_buffer_view,
                offset_of!(GltfVertex, base_color),
                mesh.vertices().0.iter().map(|v| {
                    if v.needs_uv_mapping() {
                        // All textured vertices have a vertex color of 1,1,1,1
                        [1.0; 4]
                    } else {
                        v.base_color.map(f32::from)
                    }
                }),
            )),
        ),
    ]);

    if needs_texture {
        let texcoord_accessor_index = writer.root.push(create_accessor(
            format!("{name} base color texcoords"),
            vertex_buffer_view,
            offset_of!(GltfVertex, base_color_tc),
            mesh.vertices().0.iter().map(|v| v.base_color_tc.map(f32::from)),
        ));
        attributes.insert(
            Valid(gltf_json::mesh::Semantic::TexCoords(0)),
            texcoord_accessor_index,
        );

        let vertices: Vec<GltfVertex> = mesh.vertices().0.to_vec();
        let index_bytes: Vec<u8> = match mesh.indices() {
            IndexSlice::U16(slice) => slice.iter().copied().flat_map(u16::to_le_bytes).collect(),
            IndexSlice::U32(slice) => slice.iter().copied().flat_map(u32::to_le_bytes).collect(),
        };

        // If it contains texture coordinates, we need to save it for later processing.
        // The buffer data won't be actually written until then.
        writer.meshes_awaiting_texture_coordinates.push(MeshAwaitingTextureCoordinates {
            vertices,
            index_bytes,
            buffer_index,
            buffer_object_name,
            buffer_file_suffix,
            texcoord_accessor_index,
        });
    } else {
        // If the mesh contains only vertex colors, we can write the buffer immediately.

        let vertex_bytes = bytemuck::must_cast_slice::<GltfVertex, u8>(mesh.vertices().0);
        // TODO: use the given name (sanitized) in the file name
        writer.root.buffers[buffer_index.value()] = writer
            .buffer_dest
            .write(buffer_object_name, &buffer_file_suffix, "glbin", |w| {
                w.write_all(vertex_bytes)?;
                // Convert index bytes to little-endian
                match mesh.indices() {
                    IndexSlice::U16(slice) => {
                        for index in slice {
                            w.write_all(&index.to_le_bytes())?;
                        }
                    }
                    IndexSlice::U32(slice) => {
                        for index in slice {
                            w.write_all(&index.to_le_bytes())?;
                        }
                    }
                }
                Ok(())
            })
            .expect("buffer write error"); // TODO: error propagation
    }

    writer.flaws |= mesh.flaws();

    // Kludge: this only works because we have at most one texture so we know its index.
    // If we have multiple textures (e.g. for emission) we will need to fix up
    // texture indices or texture objects.
    let use_texture = needs_texture.then_some(Index::new(0));

    let mesh_object = gltf_json::Mesh {
        name: Some(format!("{name} mesh")),
        primitives: [
            (
                mesh.opaque_range(),
                MaterialKey {
                    transparent: false,
                    use_texture,
                },
                format!("{name} opaque index"),
            ),
            (
                mesh.transparent_range(all_is_cubes_mesh::DepthOrdering::ALL_UNSORTED),
                MaterialKey {
                    transparent: true,
                    use_texture,
                },
                format!("{name} transparent index"),
            ),
        ]
        .into_iter()
        .filter_map(|(index_range, material_key, name)| {
            if index_range.is_empty() {
                return None;
            }

            let material_index = writer.intern_material(material_key);

            Some(gltf_json::mesh::Primitive {
                attributes: attributes.clone(),
                indices: Some(Index::push(
                    &mut writer.root.accessors,
                    gltf_json::Accessor {
                        buffer_view: Some(index_buffer_view),
                        byte_offset: Some(USize64::from(index_range.start * index_type.size())),
                        count: USize64::from(range_len(&index_range)),
                        component_type: Valid(gltf_json::accessor::GenericComponentType(
                            index_type,
                        )),
                        extensions: Default::default(),
                        extras: Default::default(),
                        type_: Valid(gltf_json::accessor::Type::Scalar),
                        min: None,
                        max: None,
                        name: Some(name),
                        normalized: false,
                        sparse: None,
                    },
                )),
                mode: Valid(gltf_json::mesh::Mode::Triangles),
                material: Some(material_index),
                targets: None,
                extensions: Default::default(),
                extras: Default::default(),
            })
        })
        .collect(),
        weights: None,
        extensions: Default::default(),
        extras: Default::default(),
    };
    let mesh_index = Index::push(&mut writer.root.meshes, mesh_object);

    Some(mesh_index)
}

// -------------------------------------------------------------------------------------------------

/// Vertex data for a glTF mesh that does not have texture coordinates filled in yet.
///
/// When we create meshes that require textures (rather than vertex colors),
/// we can’t know what texture coordinates their vertices should have,
/// until we know the sizes of all tiles in the atlas.
#[derive(Debug)]
pub(in crate::gltf) struct MeshAwaitingTextureCoordinates {
    /// Vertex data containing coordinates to be patched up, then written to the glTF asset.
    vertices: Vec<GltfVertex>,

    /// Index data to be written to the glTF asset.
    ///
    /// We don’t need to patch this, but we do want to write it into the same buffer, so we need
    /// to hold onto it until then.
    index_bytes: Vec<u8>,

    /// glTF buffer entry that needs its URI updated after writing.
    buffer_index: Index<gltf_json::Buffer>,

    // Name passed to [`GltfDataDestination::write`].
    buffer_object_name: String,
    buffer_file_suffix: String,

    /// Accessor for the texture coordinates, whose `min` & `max` must be updated.
    texcoord_accessor_index: Index<gltf_json::Accessor>,
}

impl MeshAwaitingTextureCoordinates {
    /// * Transform the texture coordinates according to [`UvMap`],
    /// * write the new texture coordinates into the vertex data,
    /// * write the vertex and index data to `buffer_dest`, and
    /// * patch the buffer object and texture coordinates accessor object in `root`.
    pub fn finish(
        mut self,
        uv_map: &crate::gltf::texture::UvMap,
        root: &mut gltf_json::Root,
        buffer_dest: &crate::gltf::GltfDataDestination,
    ) -> io::Result<()> {
        // Rewrite texture coordinates according to `uv_map`.
        for vertex in &mut self.vertices {
            vertex.update_texture_coordinates(uv_map);
        }

        // Update the texcoord accessor's min/max to reflect the actual UV values.
        let [min, max] = super::glue::accessor_minmax(
            self.vertices.iter().map(|v| v.base_color_tc.map(f32::from)),
        );
        let accessor = &mut root.accessors[self.texcoord_accessor_index.value()];
        accessor.min = min;
        accessor.max = max;

        // Write the buffer data (vertices followed by indices) and replace the placeholder
        // buffer object
        let buffer = buffer_dest.write(
            self.buffer_object_name,
            &self.buffer_file_suffix,
            "glbin",
            |w| {
                w.write_all(bytemuck::must_cast_slice::<GltfVertex, u8>(&self.vertices))?;
                w.write_all(&self.index_bytes)?;
                Ok(())
            },
        )?;
        root.buffers[self.buffer_index.value()] = buffer;

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------

/// Identifier of one of the materials that we may or may not use in a glTF export.
///
/// Each such material is only reified as a glTF object if it is needed.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(in crate::gltf) struct MaterialKey {
    pub transparent: bool,
    pub use_texture: Option<Index<gltf_json::Texture>>,
}

impl MaterialKey {
    pub fn to_material_definition(self) -> gltf_json::Material {
        let Self {
            transparent,
            use_texture,
        } = self;

        let name = format!(
            "all-is-cubes {tr} {tex}",
            tr = if transparent { "transparent" } else { "opaque" },
            tex = if use_texture.is_some() {
                "textured"
            } else {
                "untextured"
            }
        );

        let pbr_metallic_roughness = gltf_json::material::PbrMetallicRoughness {
            // Per glTF 2.0 § 3.9.2, the base_color_factor will be
            // multiplied by the vertex color.
            base_color_factor: gltf_json::material::PbrBaseColorFactor([1.0, 1.0, 1.0, 1.0]),
            base_color_texture: use_texture.map(|index| gltf_json::texture::Info {
                index,
                tex_coord: 0,
                extensions: Default::default(),
                extras: Default::default(),
            }),
            metallic_factor: gltf_json::material::StrengthFactor(0.0),
            roughness_factor: gltf_json::material::StrengthFactor(1.0),
            ..<_>::default()
        };

        if !transparent {
            gltf_json::Material {
                name: Some(name),
                alpha_mode: Valid(gltf_json::material::AlphaMode::Opaque),
                double_sided: false,
                pbr_metallic_roughness,
                ..gltf_json::Material::default()
            }
        } else {
            gltf_json::Material {
                name: Some(name),
                alpha_mode: Valid(gltf_json::material::AlphaMode::Blend),
                double_sided: false,
                pbr_metallic_roughness,
                extensions: Some(gltf_json::extensions::material::Material {
                    volume: None,
                    // TODO: Reenable this when attenuation_distance serialization bug is fixed.
                    // https://github.com/gltf-rs/gltf/issues/364
                    // Some(gltf_json::extensions::material::Volume {
                    //     thickness_factor: gltf_json::extensions::material::ThicknessFactor(1.0),
                    //     thickness_texture: None,
                    //     attenuation_distance:
                    //         gltf_json::extensions::material::AttenuationDistance::default(),
                    //     attenuation_color:
                    //         gltf_json::extensions::material::AttenuationColor::default(),
                    //     extras: Void::default(),
                    // }),
                    transmission: Some(gltf_json::extensions::material::Transmission {
                        transmission_factor:
                            gltf_json::extensions::material::TransmissionFactor::default(),
                        transmission_texture: None,
                        extras: Default::default(), // feature-variable type
                    }),
                    ..Default::default() // feature-variable additional fields
                }),
                ..gltf_json::Material::default()
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gltf::{GltfDataDestination, tests::gltf_mesh};
    use all_is_cubes::block;
    use all_is_cubes::math::GridAab;
    use all_is_cubes::space::Space;
    use gltf_json::texture::MinFilter;
    use std::time::Duration;

    #[test]
    fn no_extra_indices_when_transparent() {
        let space = Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(block::from_color!(0., 0., 0., 0.5))
            .build();

        let mut writer = GltfWriter::new(GltfDataDestination::null(), MinFilter::Nearest);
        let (_, mesh_index) = gltf_mesh(&space, &mut writer);
        let mesh_index = mesh_index.unwrap();
        let root = writer.into_root(Duration::ZERO).unwrap();

        let mesh = root.get(mesh_index).unwrap();
        let index_accessor_index = mesh.primitives[0].indices.unwrap();
        let vertex_accessor_index = *mesh.primitives[0].attributes.values().next().unwrap();
        let vertex_accessor = root.get(vertex_accessor_index).unwrap();
        let index_accessor = root.get(index_accessor_index).unwrap();
        let vertex_buffer_view = root.get(vertex_accessor.buffer_view.unwrap()).unwrap();
        let index_buffer_view = root.get(index_accessor.buffer_view.unwrap()).unwrap();
        let vertex_buffer = root.get(vertex_buffer_view.buffer).unwrap();
        let index_buffer = root.get(index_buffer_view.buffer).unwrap();

        let index_size = index_accessor.component_type.unwrap().0.size();
        dbg!(
            vertex_accessor.count,
            index_accessor.count,
            &vertex_buffer,
            &index_buffer,
            size_of::<GltfVertex>(),
            4 * 6 * size_of::<GltfVertex>(),
            6 * 6 * index_size,
        );
        // Six faces each with four vertices and six indices. No extras.
        assert_eq!(vertex_accessor.count, USize64(4 * 6), "vertex count");
        assert_eq!(index_accessor.count, USize64(6 * 6), "index count");
        // Buffer size should be exactly as big as needed to hold both
        assert_eq!(
            vertex_buffer_view.buffer.value(),
            index_buffer_view.buffer.value()
        );
        assert_eq!(
            vertex_buffer.byte_length.0 as usize,
            6 * 6 * index_size + 4 * 6 * size_of::<GltfVertex>(),
            "buffer size"
        );
    }

    /// [`SpaceMesh`]es are allowed to be empty. glTF meshes are not.
    #[test]
    fn empty_mesh() {
        let space = Space::empty_positive(1, 1, 1);

        let mut writer = GltfWriter::new(GltfDataDestination::null(), MinFilter::Nearest);
        let (_, mesh_index) = gltf_mesh(&space, &mut writer);

        assert!(mesh_index.is_none());
    }
}
