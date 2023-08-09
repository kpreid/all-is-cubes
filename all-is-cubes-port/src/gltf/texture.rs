//! [`GltfTextureAllocator`], produces glTF-compatible textures for blocks.

use std::io;

use all_is_cubes::cgmath::{ElementWise, Point3, Vector3};
use all_is_cubes::math::GridAab;
use all_is_cubes_mesh::texture;

use super::GltfDataDestination;

pub(crate) type TexPoint = Vector3<f32>;

/// [`texture::Allocator`] implementation for glTF exports.
///
/// You may use this with [`SpaceMesh`] to create textured meshes that can be exported.
///
/// TODO: This doesn't actually work yet.
///
/// [`SpaceMesh`]: all_is_cubes_mesh::SpaceMesh
#[derive(Clone, Debug)]
pub struct GltfTextureAllocator {
    destination: GltfDataDestination,
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
            enable: enable_wip,
        }
    }
}

impl texture::Allocator for GltfTextureAllocator {
    type Tile = GltfTextureRef;
    type Point = TexPoint;

    fn allocate(&self, bounds: GridAab) -> Option<GltfTextureRef> {
        if self.enable {
            Some(GltfTextureRef {
                bounds,
                destination: self.destination.clone(),
            })
        } else {
            None
        }
    }
}

/// [`texture::Tile`] produced by [`GltfTextureAllocator`].
///
/// You should not generally need to refer to this type.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GltfTextureRef {
    bounds: GridAab,
    destination: GltfDataDestination,
}

impl texture::Tile for GltfTextureRef {
    type Point = TexPoint;
    type Plane = GltfTexturePlane;

    fn write(&mut self, data: &[texture::Texel]) {
        assert_eq!(data.len(), self.bounds.volume());

        // TODO: don't allow more than 1 write per tile (TextureAllocator API change)

        // TODO: This code is totally wrong for the end goal; we need to construct a
        // texture atlas so that a SpaceMesh can have just one texture for all its blocks.
        // Its purpose in existing is merely to be a step along the road; the image writing
        // code will be moved to post-processing.

        // TODO: instead of writing all the 3d data as one blob, we need to dynamically
        // generate the wanted 2d slices of it.
        let buffer = self
            .destination
            .write(String::from("texture"), "texture", |w| {
                // `image` wants `Write + Seek` but `w` is not currently `Seek`
                let mut tmp = io::Cursor::new(Vec::new());

                image::write_buffer_with_format(
                    &mut tmp,
                    bytemuck::cast_slice::<[u8; 4], u8>(data),
                    self.bounds.size().x as u32,
                    self.bounds.size().y as u32 * self.bounds.size().z as u32,
                    image::ColorType::Rgba8,
                    image::ImageOutputFormat::Png,
                )
                .expect("failed to write image to in-memory buffer");

                w.write_all(tmp.into_inner().as_slice())?;
                Ok(())
            })
            .expect("TODO: propagate IO errors to later instead of panicking");

        dbg!(buffer);
    }

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn slice(&self, bounds: GridAab) -> Self::Plane {
        assert!(self.bounds.contains_box(bounds));
        GltfTexturePlane { bounds }
    }
}

/// [`Plane`] produced by [`GltfTextureAllocator`].
///
/// You should not generally need to refer to this type.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GltfTexturePlane {
    bounds: GridAab,
}

impl texture::Plane for GltfTexturePlane {
    type Point = TexPoint;

    fn grid_to_texcoord(&self, in_tile_grid: Point3<f32>) -> Self::Point {
        // TODO: these coordinates will, no matter what, need to be adjusted to be
        // within the atlas once we know what the atlas contents are. At this point,
        // we need to include information about which tile is being used, so that we can
        // transform them into the atlas position as a post-processing of the vertices.
        // (So, `TexPoint` will become some sort of enum-ish type.)
        let relative = in_tile_grid - self.bounds.lower_bounds().cast().unwrap();
        relative.div_element_wise(self.bounds.size().cast().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes_mesh::texture::{Allocator, Tile};
    use std::fs;

    #[test]
    fn allocator_creates_file() {
        let temp_dir = tempfile::tempdir().unwrap();
        let mut file_base_path = temp_dir.path().to_owned();
        file_base_path.push("filetest.gltf");

        let allocator =
            GltfTextureAllocator::new(GltfDataDestination::new(Some(file_base_path), 0), true);
        let mut tile = allocator
            .allocate(GridAab::ORIGIN_CUBE)
            .expect("allocation");
        tile.write(&[[0, 1, 2, 3]]);
        drop(tile);
        drop(allocator);
        assert_eq!(
            fs::read_dir(temp_dir.path())
                .unwrap()
                .map(|e| e.unwrap().file_name().into_string().unwrap())
                .collect::<Vec<String>>(),
            vec!["filetest-texture.glbin"], // TODO: should be .png if it's a file
        );
    }
}
