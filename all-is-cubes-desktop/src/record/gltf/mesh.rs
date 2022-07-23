use std::collections::HashMap;
use std::mem::size_of;

use bytemuck::offset_of;
use gltf_json::extras::Void;
use gltf_json::validation::Checked::Valid;
use gltf_json::Index;

use all_is_cubes::mesh::SpaceMesh;

use crate::record::gltf::glue::{create_accessor, push_and_return_index, u32size};
use crate::record::gltf::{GltfTextureRef, GltfVertex, GltfWriter};

pub(crate) fn add_mesh(
    writer: &mut GltfWriter,
    name: String,
    mesh: &SpaceMesh<GltfVertex, GltfTextureRef>,
) -> Index<gltf_json::Mesh> {
    let vertex_bytes = bytemuck::cast_slice::<GltfVertex, u8>(mesh.vertices());
    let index_bytes = bytemuck::cast_slice::<u32, u8>(mesh.indices());
    let index_type = Valid(gltf_json::accessor::GenericComponentType(
        gltf_json::accessor::ComponentType::U32,
    ));

    // TODO: use the given name (sanitized) in the file name
    let buffer_entity = writer
        .buffer_dest
        .write(
            format!("{name} data"),
            &format!("mesh-{i}", i = writer.root.buffers.len()),
            |w| {
                w.write_all(vertex_bytes)?;
                for index in mesh.indices() {
                    w.write_all(&index.to_le_bytes())?;
                }
                Ok(())
            },
        )
        .expect("buffer write error");
    let buffer_index = push_and_return_index(&mut writer.root.buffers, buffer_entity);
    let vertex_buffer_view = push_and_return_index(
        &mut writer.root.buffer_views,
        gltf_json::buffer::View {
            buffer: buffer_index,
            byte_length: u32size(vertex_bytes.len()),
            byte_offset: None,
            byte_stride: Some(u32size(size_of::<GltfVertex>())),
            name: Some(format!("{name} vertex")),
            target: Some(Valid(gltf_json::buffer::Target::ArrayBuffer)),
            extensions: Default::default(),
            extras: Default::default(),
        },
    );
    let index_buffer_view = push_and_return_index(
        &mut writer.root.buffer_views,
        gltf_json::buffer::View {
            buffer: buffer_index,
            byte_length: u32size(index_bytes.len()),
            // Indexes are packed into the same buffer, so they start at the end of the vertex bytes
            byte_offset: Some(u32size(vertex_bytes.len())),
            byte_stride: None,
            name: Some(format!("{name} index")),
            // ElementArrayBuffer means index buffer
            target: Some(Valid(gltf_json::buffer::Target::ElementArrayBuffer)),
            extensions: Default::default(),
            extras: Default::default(),
        },
    );

    let vertex_colored_attributes = HashMap::from([
        (
            Valid(gltf_json::mesh::Semantic::Positions),
            push_and_return_index(
                &mut writer.root.accessors,
                create_accessor(
                    format!("{name} position"),
                    vertex_buffer_view,
                    offset_of!(GltfVertex::DUMMY, GltfVertex, position),
                    mesh.vertices().iter().map(|v| v.position.map(f32::from)),
                ),
            ),
        ),
        (
            Valid(gltf_json::mesh::Semantic::Colors(0)),
            push_and_return_index(
                &mut writer.root.accessors,
                create_accessor(
                    format!("{name} color"),
                    vertex_buffer_view,
                    offset_of!(GltfVertex::DUMMY, GltfVertex, color_or_texture),
                    mesh.vertices()
                        .iter()
                        .map(|v| v.color_or_texture.map(f32::from)),
                ),
            ),
        ),
    ]);

    push_and_return_index(
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
                    mesh.transparent_range(all_is_cubes::mesh::DepthOrdering::Any),
                    writer.materials.transparent_vertex_colored,
                    format!("{name} transparent index"),
                ),
            ]
            .into_iter()
            .filter_map(|(index_range, material, name)| {
                if !index_range.is_empty() {
                    Some(gltf_json::mesh::Primitive {
                        attributes: vertex_colored_attributes.clone(),
                        indices: Some(push_and_return_index(
                            &mut writer.root.accessors,
                            gltf_json::Accessor {
                                buffer_view: Some(index_buffer_view),
                                byte_offset: u32size(index_range.start * size_of::<u32>()),
                                count: u32size(index_range.len()),
                                component_type: index_type,
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
    )
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
            opaque_vertex_colored: push_and_return_index(
                materials_json,
                gltf_json::Material {
                    name: Some("aic-vertex-opaque".into()),
                    alpha_mode: Valid(gltf_json::material::AlphaMode::Opaque),
                    double_sided: false,
                    pbr_metallic_roughness: pbr_metallic_roughness.clone(),
                    ..gltf_json::Material::default()
                },
            ),
            transparent_vertex_colored: push_and_return_index(
                materials_json,
                gltf_json::Material {
                    name: Some("aic-vertex-transparent".into()),
                    alpha_mode: Valid(gltf_json::material::AlphaMode::Blend),
                    double_sided: false,
                    pbr_metallic_roughness,
                    extensions: Some(gltf_json::extensions::material::Material {
                        volume: Some(gltf_json::extensions::material::Volume {
                            thickness_factor: gltf_json::extensions::material::ThicknessFactor(1.0),
                            thickness_texture: None,
                            attenuation_distance:
                                gltf_json::extensions::material::AttenuationDistance::default(),
                            attenuation_color:
                                gltf_json::extensions::material::AttenuationColor::default(),
                            extras: Void::default(),
                        }),
                        transmission: Some(gltf_json::extensions::material::Transmission {
                            transmission_factor:
                                gltf_json::extensions::material::TransmissionFactor::default(),
                            transmission_texture: None,
                            extras: Void::default(),
                        }),
                    }),
                    ..gltf_json::Material::default()
                },
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use all_is_cubes::block::Block;
    use all_is_cubes::math::Rgba;
    use all_is_cubes::space::Space;

    use crate::record::gltf::GltfDataDestination;

    use super::super::tests::gltf_mesh;
    use super::*;

    #[test]
    fn no_extra_indices_when_transparent() {
        let mut space = Space::empty_positive(1, 1, 1);
        space
            .set((0, 0, 0), &Block::from(Rgba::new(0., 0., 0., 0.5)))
            .unwrap();

        let mut writer = GltfWriter::new(GltfDataDestination::null());
        let (_, _, mesh_index) = gltf_mesh(&space, &mut writer);
        let root = writer.into_root(Duration::ZERO).unwrap();

        let mesh = &root.meshes[mesh_index.value()];
        let index_accessor_index = mesh.primitives[0].indices.unwrap();
        let vertex_accessor_index = mesh.primitives[0].attributes.values().next().unwrap();
        let vertex_accessor = &root.accessors[vertex_accessor_index.value()];
        let index_accessor = &root.accessors[index_accessor_index.value()];
        let vertex_buffer_view = &root.buffer_views[vertex_accessor.buffer_view.unwrap().value()];
        let index_buffer_view = &root.buffer_views[index_accessor.buffer_view.unwrap().value()];
        let vertex_buffer = &root.buffers[vertex_buffer_view.buffer.value()];
        let index_buffer = &root.buffers[index_buffer_view.buffer.value()];

        dbg!(
            vertex_accessor.count,
            index_accessor.count,
            &vertex_buffer,
            &index_buffer,
            size_of::<GltfVertex>(),
            4 * 6 * size_of::<GltfVertex>(),
            6 * 6 * size_of::<u32>(),
        );
        // Six faces each with four vertices and six indices. No extras.
        assert_eq!(vertex_accessor.count, 4 * 6, "vertex count");
        assert_eq!(index_accessor.count, 6 * 6, "index count");
        // Buffer size should be exactly as big as needed to hold both
        assert_eq!(
            vertex_buffer_view.buffer.value(),
            index_buffer_view.buffer.value()
        );
        assert_eq!(
            vertex_buffer.byte_length as usize,
            6 * 6 * size_of::<u32>() + 4 * 6 * size_of::<GltfVertex>(),
            "buffer size"
        );
    }
}
