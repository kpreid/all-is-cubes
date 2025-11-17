use std::sync::Arc;

use all_is_cubes::block::{self, Block, Resolution};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::vec3;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridPoint, GridVector, Gridgid};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, ReadTicket};
use all_is_cubes::util::YieldProgress;

use crate::mv;
use crate::mv::coord::MV_TO_AIC_ROTATION;

#[cfg(feature = "import")]
pub(crate) async fn scene_to_space(
    mut progress: YieldProgress,
    data: Arc<dot_vox::DotVoxData>,
    read_ticket: ReadTicket<'_>,
    imported_models: Vec<Handle<Space>>,
) -> Result<Space, mv::DotVoxConversionError> {
    progress.set_label("Processing scene graph");
    progress.progress(0.0).await;

    let mut leaves: Vec<SceneElement<'_>> = Vec::new();
    walk_scene_graph(&data, 0, Gridgid::IDENTITY, None, &mut leaves)?;

    let scene_voxel_bounding_box: GridAab = leaves
        .iter()
        .filter_map(SceneElement::bounding_box_in_aic_scene_coordinates)
        .reduce(GridAab::union_cubes)
        .unwrap_or(GridAab::ORIGIN_EMPTY);

    // TODO: optimize the scale and a global translation so that the scene content lines up
    // well with the cube grid.
    let scale_to_blocks = Resolution::R16;
    let scene_block_bounding_box = scene_voxel_bounding_box.divide(scale_to_blocks.into());

    let mut space = Space::builder(scene_block_bounding_box)
        .spawn({
            let mut spawn = Spawn::looking_at_space(scene_block_bounding_box, vec3(-1., 1., 1.));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .try_build()
        .map_err(|e| mv::DotVoxConversionError::Unexpected(InGenError::from(e)))?;

    for (leaf_index, (leaf, mut leaf_progress)) in
        leaves.iter().zip(progress.split_evenly(leaves.len())).enumerate()
    {
        leaf_progress.set_label(format!("Placing model #{leaf_index}/{n}", n = leaves.len()));
        leaf_progress.progress(0.0).await;

        let (transform_in_blocks, remainder_of_transform) = leaf.div_rem_transform(scale_to_blocks);

        let model_uid = usize::try_from(leaf.shape_model.model_id).unwrap();
        space
            .mutate(read_ticket, |m| -> Result<(), InGenError> {
                for (rel_cube, block_for_this_model) in model_space_to_blocks(
                    leaf.model,
                    imported_models[model_uid].clone(),
                    block::BlockAttributes::default(),
                    scale_to_blocks,
                    remainder_of_transform,
                ) {
                    let abs_cube = transform_in_blocks.transform_cube(rel_cube);
                    let existing_block = m[abs_cube].clone();
                    let new_block = block::Composite::new(
                        block_for_this_model.rotate(transform_in_blocks.rotation),
                        block::CompositeOperator::Over,
                    )
                    .compose_or_replace(existing_block);
                    m.set(abs_cube, new_block)?;
                }
                Ok(())
            })
            .map_err(mv::DotVoxConversionError::Unexpected)?
    }

    Ok(space)
}

/// Given a [`dot_vox::Model`] and its imported [`Space`], construct the blocks which will place it
/// in the space at the given `resolution`.
///
/// `voxel_offset` translates the model in units of 1 voxel within the produced blocks.
//---
// TODO: Does this function belong in model.rs instead of scene.rs?
// It is about models but it is closely tied to scene building.
#[cfg(feature = "import")]
pub(crate) fn model_space_to_blocks(
    model: &dot_vox::Model,
    space: Handle<Space>,
    attributes: block::BlockAttributes,
    resolution: Resolution,
    voxel_offset: GridVector,
) -> impl Iterator<Item = (Cube, Block)> {
    // the Space should have this same size; recomputing it just saves us a ReadTicket
    let model_size = mv::coord::mv_to_aic_size(model.size);

    let model_origin_translation = model_origin_translation(model);
    let bounding_box_in_blocks =
        GridAab::from_lower_size(GridPoint::zero() + model_origin_translation, model_size)
            .translate(voxel_offset)
            .divide(resolution.into());

    bounding_box_in_blocks.interior_iter().map(move |cube| {
        (
            cube,
            Block::from_primitive(block::Primitive::Recur {
                space: space.clone(),
                offset: cube.lower_bounds() * GridCoordinate::from(resolution)
                    - voxel_offset
                    - model_origin_translation,
                resolution,
            })
            .with_modifier(attributes.clone()),
        )
    })
}

/// Walk the scene graph and compute the transform for each leaf [`dot_vox::ShapeModel`].
#[cfg(feature = "import")]
fn walk_scene_graph<'data>(
    data: &'data dot_vox::DotVoxData,
    scene_index: u32,
    parent_transform: Gridgid,
    parent_node_list: Option<&ParentList<'_>>,
    output: &mut Vec<SceneElement<'data>>,
) -> Result<(), mv::DotVoxConversionError> {
    let parent_node_list = Some(&ParentList::cycle_and_depth_check(
        parent_node_list,
        scene_index,
    )?);

    match data
        .scenes
        .get(usize::try_from(scene_index).unwrap())
        .ok_or_else(|| mv::DotVoxConversionError::MissingSceneNode(scene_index))?
    {
        &dot_vox::SceneNode::Transform {
            ref attributes,
            ref frames,
            child,
            layer_id: _,
        } => {
            mv::error::warn_extra_attributes(
                format_args!("transform node #{scene_index}"),
                attributes,
                &["_name"],
            );

            // TODO: cleanup how we access the frame repeatedly
            if let Some(frame) = frames.first() {
                mv::error::warn_extra_attributes(
                    format_args!("first frame of transform node #{scene_index}"),
                    &frame.attributes,
                    &["_t", "_r"],
                );
            }

            let translation = if let Some(frame) = frames.first()
                && let Some(t_string) = frame.attributes.get("_t")
            {
                GridVector::from(
                    <[i32; 3]>::try_from(
                        t_string
                            .split(' ')
                            .map(|s| s.parse::<i32>())
                            .collect::<Result<Vec<i32>, _>>()
                            .unwrap(),
                    )
                    .unwrap(),
                )
            } else {
                GridVector::zero()
            };

            let rotation = if let Some(frame) = frames.first()
                && let Some(r_string) = frame.attributes.get("_r")
            {
                // TODO: Rotation::from_byte() may panic on invalid data. Send a patch to fix that.
                dot_vox::Rotation::from_byte(r_string.parse().unwrap())
            } else {
                dot_vox::Rotation::IDENTITY
            };
            walk_scene_graph(
                data,
                child,
                parent_transform
                    * Gridgid::from_translation(translation)
                    * Gridgid::from_rotation_about_origin(mv::coord::mv_to_aic_rotation(rotation)),
                parent_node_list,
                output,
            )?;
        }
        dot_vox::SceneNode::Group {
            attributes,
            children,
        } => {
            mv::error::warn_extra_attributes(
                format_args!("group node #{scene_index}"),
                attributes,
                &[],
            );
            for &child in children {
                walk_scene_graph(data, child, parent_transform, parent_node_list, output)?;
            }
        }
        dot_vox::SceneNode::Shape { attributes, models } => {
            mv::error::warn_extra_attributes(
                format_args!("shape node #{scene_index}"),
                attributes,
                &[],
            );
            for (sm_index, shape_model) in models.iter().enumerate() {
                mv::error::warn_extra_attributes(
                    format_args!("shape model #{sm_index} in node #{scene_index}"),
                    &shape_model.attributes,
                    &[],
                );
                output.push(SceneElement {
                    transform: parent_transform,
                    model: data
                        .models
                        .get(usize::try_from(shape_model.model_id).unwrap())
                        .ok_or_else(|| {
                            mv::DotVoxConversionError::MissingModel(shape_model.model_id)
                        })?,
                    shape_model,
                })
            }
        }
    }
    Ok(())
}

/// Used for cycle detection when walking the scene graph.
#[cfg(feature = "import")]
struct ParentList<'a> {
    index: u32,
    parent: Option<&'a ParentList<'a>>,
}

#[cfg(feature = "import")]
impl<'a> ParentList<'a> {
    fn cycle_and_depth_check(
        list: Option<&'a Self>,
        index: u32,
    ) -> Result<Self, mv::DotVoxConversionError> {
        if let Some(list) = list {
            list.check_inner(index, 100)?;
        }
        Ok(ParentList {
            index,
            parent: list,
        })
    }

    fn check_inner(&self, index: u32, max_depth: u32) -> Result<(), mv::DotVoxConversionError> {
        if self.index == index {
            Err(mv::DotVoxConversionError::SceneGraphCycle(index))
        } else if let Some(parent) = self.parent {
            parent.check_inner(
                index,
                max_depth
                    .checked_sub(1)
                    .ok_or_else(|| mv::DotVoxConversionError::SceneGraphRecursion)?,
            )
        } else {
            Ok(())
        }
    }
}

/// Results of processing the [`dot_vox::Scene`] graph.
/// Each specifies one model to put into the scene, which to us is a block or multi-block structure.
#[cfg(feature = "import")]
struct SceneElement<'data> {
    /// This transform is *not* mapped to the All is Cubes coordinate system,
    /// but only flattens the data in the file.
    transform: Gridgid,
    shape_model: &'data dot_vox::ShapeModel,
    model: &'data dot_vox::Model,
}

#[cfg(feature = "import")]
impl SceneElement<'_> {
    fn transform_in_aic_scene_coordinates(&self) -> Gridgid {
        Gridgid::from(MV_TO_AIC_ROTATION)
            * self.transform
            * Gridgid::from(MV_TO_AIC_ROTATION.inverse())
    }

    pub fn bounding_box_in_aic_scene_coordinates(&self) -> Option<GridAab> {
        let size = mv::coord::mv_to_aic_size(self.model.size);
        GridAab::from_lower_size([0, 0, 0], [size.width, size.height, size.depth])
            .translate(model_origin_translation(self.model))
            .transform(self.transform_in_aic_scene_coordinates())
    }

    /// Computes the translation which should be applied to a model multiblock,
    /// and the translation which should be applied to the voxels within the block,
    /// to produce the properly placed model as All is Cubes blocks.
    pub fn div_rem_transform(&self, scale: Resolution) -> (Gridgid, GridVector) {
        let transform_in_aic_scene_coordinates = self.transform_in_aic_scene_coordinates();
        let scale = GridCoordinate::from(scale);

        let transform_in_blocks = Gridgid {
            translation: transform_in_aic_scene_coordinates.translation / scale,
            rotation: transform_in_aic_scene_coordinates.rotation,
        };

        let remainder = transform_in_aic_scene_coordinates.translation
            - transform_in_blocks.translation * scale;

        (transform_in_blocks, remainder)
    }
}

/// Translation of the voxels of this model (presuming they are positioned in the positive
/// octant) to what should be considered the origin for purposes of a scene node’s transform.
#[cfg(feature = "import")]
fn model_origin_translation(model: &dot_vox::Model) -> GridVector {
    let size = mv::coord::mv_to_aic_size(model.size);
    // TODO: this translation to center-point was determined *empirically* to be the right
    // interpretation of .vox transforms, and probably has incorrect rounding.
    // In addition, not all scenes import correctly at all. Mysteries all around.
    vec3(
        -size.width.cast_signed() / 2,
        -size.height.cast_signed() / 2,
        -size.depth.cast_signed() / 2,
    )
}
