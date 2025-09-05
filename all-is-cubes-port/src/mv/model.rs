use all_is_cubes::block::Block;
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::{Point3D, vec3};
use all_is_cubes::math::{Cube, GridAab, Rgb};
use all_is_cubes::space::{LightPhysics, Space};
use all_is_cubes::universe::{Handle, ReadTicket};
use all_is_cubes::util::{ConciseDebug, Refmt};

use crate::mv::DotVoxConversionError;
use crate::mv::coord::{aic_to_mv_coordinate_transform, mv_to_aic_coordinate_transform};
use crate::mv::palette::block_to_dot_vox_palette_entry;
use crate::{ExportError, Format};

// -------------------------------------------------------------------------------------------------

/// TODO: Document and allow control over the metadata choices like spawn and physics,
/// and the choice of coordinate transform.
#[cfg(feature = "import")]
pub(crate) fn to_space(
    palette_blocks: &[Block],
    model: &dot_vox::Model,
) -> Result<Space, DotVoxConversionError> {
    let transform = mv_to_aic_coordinate_transform(model.size);
    let bounds =
        GridAab::from_lower_size([0, 0, 0], vec3(model.size.x, model.size.y, model.size.z))
            .transform(transform)
            .expect("TODO: return error");

    let mut space = Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::looking_at_space(bounds, vec3(-1., 1., 1.));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .light_physics(LightPhysics::Rays {
            maximum_distance: u8::try_from(bounds.y_range().len()).unwrap_or(u8::MAX),
        })
        .sky_color(Rgb::ONE)
        .build();

    space.mutate(ReadTicket::stub(), |m| {
        for v in model.voxels.iter() {
            let converted_cube: Cube = Cube::from(Point3D::new(v.x, v.y, v.z).map(i32::from));
            let transformed_cube = transform.transform_cube(converted_cube);

            let block = palette_blocks.get(v.i as usize).ok_or_else(|| {
                DotVoxConversionError::PaletteTooShort {
                    len: palette_blocks.len(),
                    index: v.i,
                }
            })?;

            m.set(transformed_cube, block)
                .map_err(DotVoxConversionError::SetCube)?;
        }
        Ok::<(), DotVoxConversionError>(())
    })?;

    Ok(space)
}

#[cfg(feature = "export")]
pub(crate) fn from_space(
    read_ticket: ReadTicket<'_>,
    space_handle: &Handle<Space>,
    palette: &mut Vec<dot_vox::Color>,
) -> Result<dot_vox::Model, ExportError> {
    let space = space_handle.read(read_ticket)?;
    let bounds = space.bounds();
    if bounds.size().width > 256 || bounds.size().height > 256 || bounds.size().depth > 256 {
        return Err(ExportError::NotRepresentable {
            format: Format::DotVox,
            name: Some(space_handle.name()),
            reason: format!(
                "space of size {} is too large to export to .vox; must be 256 or less in each axis",
                bounds.size().refmt(&ConciseDebug)
            ),
        });
    }

    let transform = aic_to_mv_coordinate_transform(bounds);
    let block_index_to_palette_index: Vec<Option<u8>> = space
        .block_data()
        .iter()
        .map(|data| {
            if let Some(entry) = block_to_dot_vox_palette_entry(data.evaluated())
                && let Ok(index) = u8::try_from(palette.len())
            {
                palette.push(entry);
                Some(index)
            } else {
                None
            }
        })
        .collect();

    let mut voxels: Vec<dot_vox::Voxel> = Vec::new();
    for cube in bounds.interior_iter() {
        if let Some(i) =
            block_index_to_palette_index[usize::from(space.get_block_index(cube).unwrap())]
        {
            let transformed_cube = transform.transform_cube(cube);
            voxels.push(dot_vox::Voxel {
                // We previously checked that the size is not too big.
                x: transformed_cube.x as u8,
                y: transformed_cube.y as u8,
                z: transformed_cube.z as u8,
                i,
            });
        } else {
            // Else the cube is empty space and should not be exported explicitly.
        }
    }

    Ok(dot_vox::Model {
        size: {
            let size = transform.rotation.transform_size(space.bounds().size());
            dot_vox::Size {
                x: size.width,
                y: size.height,
                z: size.depth,
            }
        },
        voxels,
    })
}
