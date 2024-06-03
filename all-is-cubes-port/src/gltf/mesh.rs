use std::collections::BTreeMap;
use std::fmt;
use std::mem::{offset_of, size_of};

use gltf::validation::USize64;
use gltf::Index;

use all_is_cubes_mesh::{IndexSlice, MeshTypes, SpaceMesh};

use crate::gltf::glue::create_accessor;
use crate::gltf::{GltfTextureAllocator, GltfVertex, GltfWriter};

/// Create [`gltf::Mesh`] and all its parts (accessors, buffers) from a [`SpaceMesh`].
///
/// If the input is empty, does nothing and returns `None`.
pub(crate) fn add_mesh<M>(
    writer: &mut GltfWriter,
    name: &dyn fmt::Display,
    mesh: &SpaceMesh<M>,
) -> Option<Index<gltf::Mesh>>
where
    // TODO: This generic bound (rather than `SpaceMesh<GltfMt>`) is a workaround to allow
    // `all-is-cubes-port` to define its own `DynamicMeshTypes`. This is a sign that maybe
    // `DynamicMeshTypes` is designed wrong.
    M: MeshTypes<Vertex = GltfVertex, Alloc = GltfTextureAllocator>,
{
    if mesh.is_empty() {
        return None;
    }

    let vertex_bytes = bytemuck::must_cast_slice::<GltfVertex, u8>(mesh.vertices());
    let index_type = match mesh.indices() {
        IndexSlice::U16(_) => gltf::accessor::ComponentType::U16,
        IndexSlice::U32(_) => gltf::accessor::ComponentType::U32,
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
    let vertex_buffer_view = writer.root.push(gltf::buffer::View {
        buffer: buffer_index,
        length: USize64::from(vertex_bytes.len()),
        offset: USize64(0),
        stride: Some(size_of::<GltfVertex>()),
        name: Some(format!("{name} vertex")),
        target: Some(gltf::buffer::Target::ArrayBuffer),
        unrecognized_extensions: Default::default(),
        extras: Default::default(),
    });
    let index_buffer_view = writer.root.push(gltf::buffer::View {
        buffer: buffer_index,
        length: USize64::from(mesh.indices().as_bytes().len()),
        // Indexes are packed into the same buffer, so they start at the end of the vertex bytes
        offset: USize64::from(vertex_bytes.len()),
        stride: None,
        name: Some(format!("{name} index")),
        // ElementArrayBuffer means index buffer
        target: Some(gltf::buffer::Target::ElementArrayBuffer),
        unrecognized_extensions: Default::default(),
        extras: Default::default(),
    });

    let vertex_colored_attributes = BTreeMap::from([
        (
            gltf::mesh::Semantic::Positions,
            writer.root.push(create_accessor(
                format!("{name} position"),
                vertex_buffer_view,
                offset_of!(GltfVertex, position),
                mesh.vertices().iter().map(|v| v.position.map(f32::from)),
            )),
        ),
        (
            gltf::mesh::Semantic::Colors(0),
            writer.root.push(create_accessor(
                format!("{name} base color"),
                vertex_buffer_view,
                offset_of!(GltfVertex, base_color),
                mesh.vertices().iter().map(|v| v.base_color.map(f32::from)),
            )),
        ),
        (
            gltf::mesh::Semantic::TexCoords(0),
            writer.root.push(create_accessor(
                format!("{name} base color texcoords"),
                vertex_buffer_view,
                offset_of!(GltfVertex, base_color_tc),
                mesh.vertices()
                    .iter()
                    .map(|v| v.base_color_tc.map(f32::from)),
            )),
        ),
    ]);

    writer.flaws |= mesh.flaws();

    let mesh_index = Index::push(
        &mut writer.root.meshes,
        gltf::Mesh {
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
                    Some(gltf::mesh::Primitive {
                        attributes: vertex_colored_attributes.clone(),
                        indices: Some(Index::push(
                            &mut writer.root.accessors,
                            gltf::Accessor {
                                buffer_view: Some(index_buffer_view),
                                byte_offset: Some(USize64::from(
                                    index_range.start * index_type.size(),
                                )),
                                count: USize64::from(index_range.len()),
                                component_type: index_type,
                                unrecognized_extensions: Default::default(),
                                extras: Default::default(),
                                attribute_type: gltf::accessor::AttributeType::Scalar,
                                min: None,
                                max: None,
                                name: Some(name),
                                normalized: false,
                                sparse: None,
                            },
                        )),
                        mode: gltf::mesh::Mode::Triangles,
                        material: Some(material),
                        targets: vec![],
                        variants: None,
                        unrecognized_extensions: Default::default(),
                        extras: Default::default(),
                    })
                } else {
                    None
                }
            })
            .collect(),
            weights: vec![],
            unrecognized_extensions: Default::default(),
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
    pub opaque_vertex_colored: Index<gltf::Material>,
    pub transparent_vertex_colored: Index<gltf::Material>,
}

impl Materials {
    pub fn new(materials_json: &mut Vec<gltf::Material>) -> Self {
        let pbr_metallic_roughness = gltf::material::PbrMetallicRoughness {
            // Per glTF 2.0 § 3.9.2, the base_color_factor will be
            // multiplied by the vertex color.
            base_color_factor: [1.0, 1.0, 1.0, 1.0],
            base_color_texture: None,
            metallic_factor: 0.0,
            roughness_factor: 1.0,
            ..<_>::default()
        };
        Self {
            opaque_vertex_colored: {
                let value = gltf::Material {
                    name: Some("aic-vertex-opaque".into()),
                    alpha_mode: gltf::material::AlphaMode::Opaque,
                    double_sided: false,
                    pbr_metallic_roughness: Some(pbr_metallic_roughness.clone()),
                    ..gltf::Material::default()
                };
                Index::push(materials_json, value)
            },
            transparent_vertex_colored: {
                let value = gltf::Material {
                    name: Some("aic-vertex-transparent".into()),
                    alpha_mode: gltf::material::AlphaMode::Blend,
                    double_sided: false,
                    pbr_metallic_roughness: Some(pbr_metallic_roughness),
                    volume: Some(gltf::material::khr_materials_volume::Volume {
                        thickness_factor: 1.0, // one cube size
                        ..Default::default()
                    }),
                    // TODO: this should have a transmission texture, once we support textures
                    transmission: Some(
                        gltf::material::khr_materials_transmission::Transmission::default(),
                    ),
                    ..gltf::Material::default()
                };
                Index::push(materials_json, value)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gltf::{tests::gltf_mesh, GltfDataDestination};
    use all_is_cubes::color_block;
    use all_is_cubes::math::GridAab;
    use all_is_cubes::space::Space;
    use std::time::Duration;

    #[test]
    fn no_extra_indices_when_transparent() {
        let space = Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(color_block!(0., 0., 0., 0.5))
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

        let index_size = index_accessor.component_type.size();
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
            vertex_buffer.length.0 as usize,
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
