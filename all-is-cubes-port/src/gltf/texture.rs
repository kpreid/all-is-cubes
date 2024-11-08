//! [`GltfTextureAllocator`], produces glTF-compatible textures for blocks.

use std::io;

use gltf_json::validation::Checked::Valid;

use all_is_cubes::block::Evoxel;
use all_is_cubes::euclid::Point2D;
use all_is_cubes::math::{Axis, GridAab, GridRotation, Vol};
use all_is_cubes_mesh::texture::{self, TilePoint};

use super::GltfDataDestination;

/// [`texture::Allocator`] implementation for glTF exports.
///
/// You may use this with [`SpaceMesh`] to create textured meshes that can be exported.
///
/// This allocator does not perform deallocation, on the assumption that as the glTF asset
/// is constructed, the meshes going into it are created and dropped rather than kept to
/// the end.
///
/// TODO: This allocator does not actually produce a usable texture atlas yet.
///
/// [`SpaceMesh`]: all_is_cubes_mesh::SpaceMesh
#[derive(Clone, Debug)]
pub struct GltfTextureAllocator {
    destination: GltfDataDestination,
    gatherer: internal::Gatherer,
    enable: bool,
}

impl GltfTextureAllocator {
    /// Public access is via [`GltfWriter::texture_allocator()`].
    ///
    /// `enable_wip` enables the still-incomplete partial work for testing.
    /// If false, all allocations fail.
    pub(crate) fn new(destination: GltfDataDestination, enable_wip: bool) -> Self {
        Self {
            destination,
            gatherer: internal::Gatherer::default(),
            enable: enable_wip,
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.gatherer.is_empty()
    }

    pub(crate) fn write_png_atlas(&self) -> Result<gltf_json::Buffer, io::Error> {
        let image: image::RgbaImage = self.gatherer.build_atlas();
        let buffer = self
            .destination
            .write(String::from("texture"), "texture", "png", |w| {
                // `image` wants `Write + Seek` but `w` is not currently `Seek`
                let mut tmp = io::Cursor::new(Vec::new());
                image
                    .write_to(&mut tmp, image::ImageFormat::Png)
                    .expect("failed to write image to in-memory buffer");
                w.write_all(tmp.into_inner().as_slice())?;
                Ok(())
            })?;
        Ok(buffer)
    }
}

impl texture::Allocator for GltfTextureAllocator {
    type Tile = GltfTile;
    type Point = GltfAtlasPoint;

    fn allocate(&self, bounds: GridAab, mut channels: texture::Channels) -> Option<GltfTile> {
        assert!(!bounds.is_empty());

        if self.enable {
            // TODO: implement more channels
            if true {
                channels = texture::Channels::Reflectance;
            }

            Some(GltfTile {
                bounds,
                channels,
                texels: internal::TexelsCell::default(),
                gatherer: self.gatherer.clone(),
            })
        } else {
            None
        }
    }
}

/// [`texture::Tile`] produced by [`GltfTextureAllocator`].
///
/// You should not generally need to refer to this type.
//
// Implementation notes: Since glTF does not support 3D textures, we must slice the provided
// texels into 2D sections. Therefore, this is just a container for the texels, not a handle
// to the actual atlas.
#[derive(Clone, Debug, PartialEq)]
pub struct GltfTile {
    bounds: GridAab,
    channels: texture::Channels,
    gatherer: internal::Gatherer,
    texels: internal::TexelsCell,
}

impl texture::Tile for GltfTile {
    type Point = GltfAtlasPoint;
    type Plane = GltfTexturePlane;
    const REUSABLE: bool = false;

    fn write(&mut self, data: Vol<&[Evoxel]>) {
        assert_eq!(data.bounds(), self.bounds());

        let mut buffer = vec![[0, 0, 0, 0]; self.bounds().volume().unwrap()];
        texture::copy_voxels_into_xmaj_texture(data, &mut buffer, None);

        // OK to panic on failure because if we do, the caller ignored Self::REUSABLE.
        self.texels
            .set(buffer)
            .expect("cannot overwrite glTF textures")
    }

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn channels(&self) -> texture::Channels {
        self.channels
    }

    #[track_caller]
    fn slice(&self, sliced_bounds: GridAab) -> Self::Plane {
        let axis = texture::validate_slice(self.bounds, sliced_bounds);

        let rotation = match axis {
            // TODO: decide which exact rotations to use
            Axis::X => GridRotation::RZYx, // X is flat, so rotate about Y
            Axis::Y => GridRotation::RXZy, // Y is flat, so rotate about X
            Axis::Z => GridRotation::IDENTITY,
        };

        let plane_id = self.gatherer.insert(internal::AtlasEntry {
            source_texels: self.texels.clone(),
            source_bounds: self.bounds,
            sliced_bounds,
            rotation,
        });

        GltfTexturePlane {
            plane_id,
            bounds: sliced_bounds,
            rotation,
        }
    }
}

/// [`texture::Plane`] produced by [`GltfTextureAllocator`].
///
/// You should not generally need to refer to this type.
#[derive(Clone, Debug, PartialEq)]
#[expect(clippy::derive_partial_eq_without_eq)]
pub struct GltfTexturePlane {
    plane_id: u64,
    bounds: GridAab,
    rotation: GridRotation,
}

impl texture::Plane for GltfTexturePlane {
    type Point = GltfAtlasPoint;

    fn grid_to_texcoord(&self, tc_in_tile: TilePoint) -> Self::Point {
        // TODO: precompute more of this
        let rot_tc: TilePoint = self
            .rotation
            .to_rotation_matrix()
            .to_free()
            .with_source::<texture::TexelUnit>() // TODO: avoid these unit changes by being able to get Transform3D straight from GridRotation
            .with_destination::<texture::TexelUnit>()
            .transform_point3d(tc_in_tile.map(f64::from))
            .unwrap()
            .map(|c| c as f32);
        let rot_bounds = self.bounds.transform(self.rotation.into()).unwrap();

        // Translate to get coordinates within the rotated-to-XY plane.
        let point_within = rot_tc - rot_bounds.lower_bounds().to_f32().cast_unit();
        debug_assert!(
            point_within.z >= 0.0 && point_within.z <= 1.0,
            "{tc_in_tile:?} -> {point_within:?}"
        );
        let point_within = point_within.to_2d().to_point();

        GltfAtlasPoint {
            plane_id: self.plane_id,
            point_within,
        }
    }
}

/// Type of points produced by [`GltfTextureAllocator`], identifying a [`GltfTexturePlane`]
/// and position within the plane.
///
/// These are not directly usable as texture coordinates; they must first be rewritten into
/// coordinates within a generated texture atlas. TODO: Document how to do that.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GltfAtlasPoint {
    /// Unique ID of the plane.
    pub(crate) plane_id: u64,
    /// Point within the plane, still with 1 unit = 1 texel coordinates.
    pub(crate) point_within: Point2D<f32, texture::TexelUnit>,
}

/// Generate the atlas texture and necessary glTF entities.
pub(super) fn insert_block_texture_atlas(
    root: &mut gltf_json::Root,
    allocator: &GltfTextureAllocator,
) -> Result<gltf_json::Index<gltf_json::Texture>, io::Error> {
    let block_texture_buffer = allocator.write_png_atlas()?;
    let block_texture_len = block_texture_buffer.byte_length;
    let block_texture_buffer = root.push(block_texture_buffer);
    let block_texture_buffer_view = root.push(gltf_json::buffer::View {
        buffer: block_texture_buffer,
        byte_length: block_texture_len,
        byte_offset: None,
        byte_stride: None,
        name: Some("block texture".into()),
        target: None,
        extensions: None,
        extras: Default::default(),
    });
    let block_texture_sampler = root.push(gltf_json::texture::Sampler {
        mag_filter: Some(Valid(gltf_json::texture::MagFilter::Nearest)),
        min_filter: Some(Valid(gltf_json::texture::MinFilter::Linear)),
        name: Some("block texture".into()),
        wrap_s: Valid(gltf_json::texture::WrappingMode::ClampToEdge),
        wrap_t: Valid(gltf_json::texture::WrappingMode::ClampToEdge),
        extensions: None,
        extras: Default::default(),
    });
    let block_texture_image = root.push(gltf_json::Image {
        buffer_view: Some(block_texture_buffer_view),
        mime_type: Some(gltf_json::image::MimeType("image/png".into())),
        name: Some("block texture".into()),
        uri: None,
        extensions: None,
        extras: Default::default(),
    });
    let block_texture = root.push(gltf_json::Texture {
        name: None,
        sampler: Some(block_texture_sampler),
        source: block_texture_image,
        extensions: None,
        extras: Default::default(),
    });
    Ok(block_texture)
}

mod internal {
    use all_is_cubes::euclid::Point3D;
    use all_is_cubes::math::Cube;

    use super::*;
    use std::collections::BTreeMap;
    use std::mem;
    use std::sync::{Arc, Mutex, OnceLock};

    /// Texels are written here through tiles and read through planes.
    pub(super) type TexelsCell = Arc<OnceLock<Vec<[u8; 4]>>>;

    /// Interior-mutable accumulator of textures to put in the atlas.
    #[derive(Clone, Debug, Default)]
    pub(super) struct Gatherer(Arc<Mutex<Vec<AtlasEntry>>>);

    impl Gatherer {
        pub(crate) fn is_empty(&self) -> bool {
            self.0.lock().expect("mutex in atlas gatherer").is_empty()
        }

        pub fn insert(&self, entry: AtlasEntry) -> u64 {
            let mut data = self.0.lock().expect("mutex in atlas gatherer");
            let plane_id = u64::try_from(data.len()).unwrap();
            data.push(entry);
            plane_id
        }

        pub(crate) fn build_atlas(&self) -> image::RgbaImage {
            use rectangle_pack as rp;

            let entries: Vec<AtlasEntry> =
                mem::take(&mut self.0.lock().expect("mutex in atlas gatherer"));
            // TODO: exit early if vec is empty

            // TODO: add anti-bleed borders to atlas
            let mut rects_to_place: rp::GroupedRectsToPlace<usize, ()> =
                rp::GroupedRectsToPlace::new();
            for (
                i,
                &AtlasEntry {
                    source_texels: _,
                    source_bounds: _,
                    sliced_bounds,
                    rotation,
                },
            ) in entries.iter().enumerate()
            {
                let size = sliced_bounds
                    .transform(rotation.into())
                    .unwrap(/* cannot overflow? */)
                    .size();
                assert_eq!(
                    size.depth, 1,
                    "failed to rotate slice {sliced_bounds:?} into the XY plane with {rotation:?}: {size:?}"
                );
                rects_to_place.push_rect(
                    i,
                    None,
                    rp::RectToInsert::new(size.width, size.height, size.depth),
                );
            }

            let mut texture_size = 1;
            let placements = loop {
                // "Bins" correspond to multiple textures. We will use one bin,
                // because our goal is to fit everything into one texture rather than
                // requiring multiple meshes.
                let mut bins =
                    BTreeMap::from([((), rp::TargetBin::new(texture_size, texture_size, 1))]);

                match rp::pack_rects(
                    &rects_to_place,
                    &mut bins,
                    &rp::volume_heuristic,
                    &rp::contains_smallest_box,
                ) {
                    Ok(placements) => {
                        break placements;
                    }
                    Err(rp::RectanglePackError::NotEnoughBinSpace) => {
                        texture_size = texture_size.checked_mul(2).unwrap();
                    }
                }
            };

            let mut atlas_image = image::RgbaImage::new(texture_size, texture_size);

            for (&index, &((), slice_location_in_atlas)) in placements.packed_locations() {
                let entry = &entries[index];

                let rotated_slice_bounds = entry.rotated_slice_bounds();
                let rotated_size = rotated_slice_bounds.size();
                let unrotate = entry.rotation.inverse();
                let texels_size = entry.source_bounds.size();
                let texels = entry
                    .source_texels
                    .get()
                    .expect("image texels not set -- TODO propagate error");

                // Copy slice from 3D `texels` into 2D atlas image.
                // TODO: Something is wrong in this code causing skewed outputs; not yet diagnosed.
                for y in 0..rotated_size.height {
                    for x in 0..rotated_size.width {
                        // Zero-offset position in the rotated-to-flat slice.
                        let pixel_position = Point3D::new(x, y, 0).cast::<i32>();
                        // Position in the rotated-to-flat slice's coordinates.
                        let position_in_rotated_slice = Cube::from(
                            pixel_position + rotated_slice_bounds.lower_bounds().to_vector(),
                        );
                        // TODO: this single cube is a kludge to simplify off-by-1 problems with rotation
                        // We should have transform helpers instead of computing twice as many coordinates.
                        let cube_in_rotated_slice = position_in_rotated_slice.grid_aab();

                        // Position in the original requested slice's coordinates.
                        let unrotated = cube_in_rotated_slice.transform(unrotate.into()).unwrap();
                        assert!(
                            entry.sliced_bounds.contains_box(unrotated),
                            "{pixel_position:?} in {rotated_size:?} -> {unrotated:?} in {sliced_bounds:?}",
                            sliced_bounds = entry.sliced_bounds
                        );

                        // Position within tile bounds.
                        let position_in_texels = (unrotated.lower_bounds()
                            - entry.source_bounds.lower_bounds())
                        .to_u32();
                        // Index into the `texels` array at that position.
                        let index_in_texels = (position_in_texels.x
                            + texels_size.width * position_in_texels.y)
                            + texels_size.height * position_in_texels.z;

                        let texel = texels[usize::try_from(index_in_texels).unwrap()];
                        atlas_image.put_pixel(
                            slice_location_in_atlas.x() + x,
                            slice_location_in_atlas.y() + y,
                            image::Rgba(texel),
                        );
                    }
                }
            }

            atlas_image
        }
    }

    impl PartialEq for Gatherer {
        fn eq(&self, other: &Self) -> bool {
            Arc::ptr_eq(&self.0, &other.0)
        }
    }

    /// Details of one piece of texture that must be included in the generated texture atlas.
    #[derive(Debug)]
    pub(super) struct AtlasEntry {
        /// Texels to extract.
        pub(super) source_texels: TexelsCell,
        /// Bounds of the 3D region containing `source_texels`.
        pub(super) source_bounds: GridAab,
        /// Bounds of the 2D region sliced out of `source_bounds`. Must have at least one axis of size 1.
        pub(super) sliced_bounds: GridAab,
        /// Rotation that will rotate `sliced_bounds` to be flat in the atlas.
        pub(super) rotation: GridRotation,
    }

    impl AtlasEntry {
        pub fn rotated_slice_bounds(&self) -> GridAab {
            self
                .sliced_bounds
                .transform(self.rotation.into())
                .unwrap(/* cannot overflow because block sizes are limited */)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block;
    use all_is_cubes::math::Rgba;
    use all_is_cubes_mesh::texture::{Allocator, Channels, Tile};
    use std::fs;

    /// TODO: this is just a smoke-test; add more rigorous tests.
    #[test]
    fn allocator_creates_file() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("filetest.gltf");

        let allocator =
            GltfTextureAllocator::new(GltfDataDestination::new(Some(file_base_path), 0), true);
        let mut tile = allocator
            .allocate(GridAab::ORIGIN_CUBE, Channels::Reflectance)
            .expect("allocation");
        tile.write(
            block::Evoxels::from_one(Evoxel::from_color(Rgba::from_srgb8([1, 2, 3, 4])))
                .as_vol_ref(),
        );
        drop(tile);

        allocator.write_png_atlas().unwrap();

        assert_eq!(
            fs::read_dir(temp_dir.path())
                .unwrap()
                .map(|e| e.unwrap().file_name().into_string().unwrap())
                .collect::<Vec<String>>(),
            vec!["filetest-texture.png"],
        );
    }
}
