//! Common imports used via `use super::prelude::*` in sibling modules.

pub(super) use alloc::boxed::Box;
pub(super) use alloc::sync::Arc;
pub(super) use alloc::vec::Vec;
pub(super) use core::convert::{TryFrom as _, identity};
pub(super) use core::f64::consts::PI;
pub(super) use core::num::NonZero;

pub(super) use exhaust::Exhaust as _;
pub(super) use rand::SeedableRng as _;

pub(super) use all_is_cubes::arcstr::{self, literal};
pub(super) use all_is_cubes::block::{
    self, AIR, Block, BlockCollision, Composite, CompositeOperator, Move,
    Resolution::{self, *},
    RotationPlacementRule, Zoom, space_to_blocks, text,
};
pub(super) use all_is_cubes::drawing::VoxelBrush;
pub(super) use all_is_cubes::drawing::embedded_graphics::{
    geometry::Point,
    prelude::Size,
    primitives::{PrimitiveStyle, Rectangle, StyledDrawable},
};
pub(super) use all_is_cubes::euclid::{
    Point3D, Rotation2D, Size3D, Vector2D, Vector3D, point3, size3, vec3,
};
pub(super) use all_is_cubes::linking::{BlockProvider, InGenError};
pub(super) use all_is_cubes::listen;
pub(super) use all_is_cubes::math::{
    Cube, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridRotation,
    GridSize, GridVector, Gridgid, PositiveSign, Rgb, Rgba, ps32, rgb_const, rgba_const, zo32,
};
pub(super) use all_is_cubes::op::Operation;
pub(super) use all_is_cubes::space::{self, Space, SpacePhysics, SpaceTransaction};
pub(super) use all_is_cubes::time;
pub(super) use all_is_cubes::transaction::{self, Transaction as _};
pub(super) use all_is_cubes::{color_block, include_image};

pub(super) use crate::alg::{self, four_walls, stack};
pub(super) use crate::city::exhibit::{Context, Exhibit, ExhibitTransaction, Placement, exhibit};
pub(super) use crate::{
    BoxPart, BoxStyle, DemoBlocks, Fire, LandscapeBlocks, make_some_blocks,
    make_some_voxel_blocks_txn, palette, tree,
};
