use std::collections::BTreeMap;
use std::fmt;
use std::mem::{offset_of, size_of};

use gltf_json::Index;
use gltf_json::buffer::Stride;
use gltf_json::validation::Checked::Valid;
use gltf_json::validation::USize64;

use all_is_cubes_mesh::{IndexSlice, MeshTypes, SpaceMesh};

use crate::gltf::glue::create_accessor;
use crate::gltf::{GltfTextureAllocator, GltfVertex, GltfWriter};

/// Create [`gltf_json::Mesh`] and all its parts (accessors, buffers) from a [`SpaceMesh`].
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

    let vertex_bytes = bytemuck::must_cast_slice::<GltfVertex, u8>(mesh.vertices().0);
    let index_type = match mesh.indices() {
        IndexSlice::U16(_) => gltf_json::accessor::ComponentType::U16,
        IndexSlice::U32(_) => gltf_json::accessor::ComponentType::U32,
    };

    // TODO: use the given name (sanitized) in the file name
    let buffer_entity = writer
        .buffer_dest
        .write(
            format!("{name} data"),
            &format!("mesh-{i}", i = writer.root.buffers.len()),
            "glbin",
            |w| {
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
            },
        )
        .expect("buffer write error");
    let buffer_index = writer.root.push(buffer_entity);
    let vertex_buffer_view = writer.root.push(gltf_json::buffer::View {
        buffer: buffer_index,
        byte_length: USize64::from(vertex_bytes.len()),
        byte_offset: None,
        byte_stride: Some(Stride(size_of::<GltfVertex>())),
        name: Some(format!("{name} vertex")),
        target: Some(Valid(gltf_json::buffer::Target::ArrayBuffer)),
        extensions: Default::default(),
        extras: Default::default(),
    });
    let index_buffer_view = writer.root.push(gltf_json::buffer::View {
        buffer: buffer_index,
        byte_length: USize64::from(mesh.indices().as_bytes().len()),
        // Indexes are packed into the same buffer, so they start at the end of the vertex bytes
        byte_offset: Some(USize64::from(vertex_bytes.len())),
        byte_stride: None,
        name: Some(format!("{name} index")),
        // ElementArrayBuffer means index buffer
        target: Some(Valid(gltf_json::buffer::Target::ElementArrayBuffer)),
        extensions: Default::default(),
        extras: Default::default(),
    });

    let vertex_colored_attributes = BTreeMap::from([
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
                mesh.vertices()
                    .0
                    .iter()
                    .map(|v| v.base_color.map(f32::from)),
            )),
        ),
        (
            Valid(gltf_json::mesh::Semantic::TexCoords(0)),
            writer.root.push(create_accessor(
                format!("{name} base color texcoords"),
                vertex_buffer_view,
                offset_of!(GltfVertex, base_color_tc),
                mesh.vertices()
                    .0
                    .iter()
                    .map(|v| v.base_color_tc.map(f32::from)),
            )),
        ),
    ]);

    writer.flaws |= mesh.flaws();

    let mesh_index = Index::push(
        &mut writer.root.meshes,
        gltf_json::Mesh {
            name: Some(format!("{name} mesh")),
            primitives: [
                (
                    mesh.opaque_range(),
                    writer.materials.opaque_vertex_colored,
                    format!("{name} opaque index"),
                ),
                (
                    mesh.transparent_range(all_is_cubes_mesh::DepthOrdering::Any),
                    writer.materials.transparent_vertex_colored,
                    format!("{name} transparent index"),
                ),
            ]
            .into_iter()
            .filter_map(|(index_range, material, name)| {
                if !index_range.is_empty() {
                    Some(gltf_json::mesh::Primitive {
                        attributes: vertex_colored_attributes.clone(),
                        indices: Some(Index::push(
                            &mut writer.root.accessors,
                            gltf_json::Accessor {
                                buffer_view: Some(index_buffer_view),
                                byte_offset: Some(USize64::from(
                                    index_range.start * index_type.size(),
                                )),
                                count: USize64::from(index_range.len()),
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
                        material: Some(material),
                        targets: None,
                        extensions: Default::default(),
                        extras: Default::default(),
                    })
                } else {
                    None
                }
            })
            .collect(),
            weights: None,
            extensions: Default::default(),
            extras: Default::default(),
        },
    );

    Some(mesh_index)
}

/// Collection of materials used in the glTF.
///
/// TODO: Each should be optional and created only if required.
#[derive(Debug)]
pub(crate) struct Materials {
    pub opaque_vertex_colored: Index<gltf_json::Material>,
    pub transparent_vertex_colored: Index<gltf_json::Material>,
}

impl Materials {
    pub fn new(materials_json: &mut Vec<gltf_json::Material>) -> Self {
        let pbr_metallic_roughness = gltf_json::material::PbrMetallicRoughness {
            // Per glTF 2.0 § 3.9.2, the base_color_factor will be
            // multiplied by the vertex color.
            base_color_factor: gltf_json::material::PbrBaseColorFactor([1.0, 1.0, 1.0, 1.0]),
            base_color_texture: None,
            metallic_factor: gltf_json::material::StrengthFactor(0.0),
            roughness_factor: gltf_json::material::StrengthFactor(1.0),
            ..<_>::default()
        };
        Self {
            opaque_vertex_colored: {
                let value = gltf_json::Material {
                    name: Some("aic-vertex-opaque".into()),
                    alpha_mode: Valid(gltf_json::material::AlphaMode::Opaque),
                    double_sided: false,
                    pbr_metallic_roughness: pbr_metallic_roughness.clone(),
                    ..gltf_json::Material::default()
                };
                Index::push(materials_json, value)
            },
            transparent_vertex_colored: {
                let value = gltf_json::Material {
                    name: Some("aic-vertex-transparent".into()),
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
                };
                Index::push(materials_json, value)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gltf::{GltfDataDestination, tests::gltf_mesh};
    use all_is_cubes::block;
    use all_is_cubes::math::GridAab;
    use all_is_cubes::space::Space;
    use std::time::Duration;

    #[test]
    fn no_extra_indices_when_transparent() {
        let space = Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(block::from_color!(0., 0., 0., 0.5))
            .build();

        let mut writer = GltfWriter::new(GltfDataDestination::null());
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

        let mut writer = GltfWriter::new(GltfDataDestination::null());
        let (_, mesh_index) = gltf_mesh(&space, &mut writer);

        assert!(mesh_index.is_none());
    }
}
