//! Data types which represent game state in formats explicitly designed for
//! serialization, and versioned to ensure ability to deserialize older data.
//!
//! As a general rule, all types in this file should avoid referring to types outside
//! this file, except where specifically intended. This ensures that changes to internal
//! representations will not accidentally leak to the serialization/save-game format via
//! `#[derive(Serialize, Deserialize)]`.
//!
//! An additional purpose of keeping all such types in this file is so that they can be
//! reviewed together to comprehend the formats.
//!
//! General properties of the serialization schema:
//!
//! * 3D vectors/points are represented as 3-element arrays
//!   (and not, say, as structures with named fields).
//! * [`Cow`] is sometimes used to avoid unnecessary clones during serialization.

use alloc::borrow::Cow;
use alloc::vec::Vec;
use core::num::NonZeroU16;

use arcstr::ArcStr;
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};

use crate::block::Block;
use crate::math::{Aab, Face6, GridAab, GridCoordinate, GridRotation};
use crate::save::compress::{GzSerde, Leu16};
use crate::universe::URef;
use crate::{behavior, block, character, inv, space, universe};

/// Placeholder type for when we want to serialize the *contents* of a `URef`,
/// without cloning or referencing those contents immediately.
pub(crate) struct SerializeRef<T>(pub(crate) URef<T>);

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `behavior` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum BehaviorSetSer<A> {
    BehaviorSetV1 {
        behaviors: Vec<BehaviorSetEntryV1Ser<A>>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct BehaviorSetEntryV1Ser<A> {
    pub behavior: BehaviorV1Ser,
    pub attachment: A,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum BehaviorV1Ser {
    // TODO: This is empty because we don't actually have any specifically serializable
    // behaviors yet.
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `block` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum BlockSer<'a> {
    BlockV1 {
        primitive: PrimitiveSer<'a>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        modifiers: Vec<ModifierSer>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum PrimitiveSer<'a> {
    AirV1,
    AtomV1 {
        color: RgbaSer,
        #[serde(default, skip_serializing_if = "is_default")]
        light_emission: RgbSer,
        #[serde(flatten)]
        attributes: BlockAttributesV1Ser<'a>,
        #[serde(default, skip_serializing_if = "is_default")]
        collision: BlockCollisionSer,
    },
    RecurV1 {
        #[serde(flatten)]
        attributes: BlockAttributesV1Ser<'a>,
        space: URef<space::Space>,
        #[serde(default, skip_serializing_if = "is_default")]
        offset: [i32; 3],
        resolution: block::Resolution,
    },
    IndirectV1 {
        definition: URef<block::BlockDef>,
    },
    TextPrimitiveV1 {
        text: TextSer,
        offset: [GridCoordinate; 3],
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct BlockAttributesV1Ser<'a> {
    #[serde(default, skip_serializing_if = "str::is_empty")]
    pub display_name: ArcStr,
    #[serde(default = "return_true", skip_serializing_if = "is_true")]
    pub selectable: bool,
    #[serde(default, skip_serializing_if = "is_default")]
    pub rotation_rule: RotationPlacementRuleSer,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tick_action: Option<TickActionSer<'a>>,
    #[serde(default, skip_serializing_if = "is_default")]
    pub animation_hint: AnimationHintSer,
}
fn return_true() -> bool {
    true
}
fn is_true(value: &bool) -> bool {
    *value
}
fn is_default<T: Default + PartialEq>(value: &T) -> bool {
    *value == T::default()
}

#[derive(Debug, Default, PartialEq, Deserialize, Serialize)]
pub(crate) enum BlockCollisionSer {
    #[default]
    HardV1,
    NoneV1,
}

#[derive(Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum RotationPlacementRuleSer {
    #[default]
    NeverV1,
    AttachV1 {
        by: Face6,
    },
}

/// Unversioned because it's versioned by the parent struct
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct TickActionSer<'a> {
    pub operation: Cow<'a, crate::op::Operation>,
    pub period: NonZeroU16,
}
#[derive(Copy, Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum AnimationHintSer {
    AnimationHintV1 {
        redefinition: AnimationChangeV1Ser,
        replacement: AnimationChangeV1Ser,
    },
}
impl Default for AnimationHintSer {
    fn default() -> Self {
        Self::AnimationHintV1 {
            redefinition: AnimationChangeV1Ser::None,
            replacement: AnimationChangeV1Ser::None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Deserialize, Serialize)]
pub(crate) enum AnimationChangeV1Ser {
    None,
    ColorSameCategory,
    Shape,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum ModifierSer {
    QuoteV1 {
        suppress_ambient: bool,
    },
    RotateV1 {
        rotation: GridRotation,
    },
    CompositeV1 {
        source: Block,
        operator: block::CompositeOperator,
        reverse: bool,
        disassemblable: bool,
    },
    ZoomV1 {
        scale: block::Resolution,
        offset: [u8; 3],
    },
    MoveV1 {
        direction: Face6,
        distance: u16,
        velocity: i16,
    },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum TextSer {
    TextV1 {
        string: ArcStr,
        font: FontSer,
        foreground: Block,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        outline: Option<Block>,
        resolution: block::Resolution,
        layout_bounds: GridAab,
        positioning: PositioningSerV1,
        #[serde(default, skip_serializing_if = "is_default")]
        debug: bool,
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) enum FontSer {
    System16V1,
    #[doc(hidden)] // experimental, and not intended to be actually serialized
    UnstableSmallerBodyTextV1,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct PositioningSerV1 {
    pub(crate) x: PositioningXSer,
    pub(crate) line_y: PositioningYSer,
    pub(crate) z: PositioningZSer,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) enum PositioningXSer {
    LeftV1,
    CenterV1,
    RightV1,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) enum PositioningYSer {
    BodyTopV1,
    BodyMiddleV1,
    BaselineV1,
    BodyBottomV1,
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) enum PositioningZSer {
    BackV1,
    FrontV1,
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `character` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum CharacterSer<'a> {
    CharacterV1 {
        space: URef<space::Space>,
        position: [f64; 3],
        velocity: [f64; 3],
        collision_box: Aab,
        flying: bool,
        noclip: bool,
        yaw: f64,
        pitch: f64,
        inventory: Cow<'a, inv::Inventory>,
        selected_slots: [usize; 3],
        #[serde(default, skip_serializing_if = "behavior::BehaviorSet::is_empty")]
        behaviors: Cow<'a, behavior::BehaviorSet<character::Character>>,
    },
}

#[derive(Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum SpawnSer {
    SpawnV1 {
        bounds: GridAab,
        eye_position: Option<[NotNan<f64>; 3]>,
        look_direction: [NotNan<f64>; 3],
        inventory: Vec<Option<InvStackSer>>,
    },
}
//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `inv` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum InventorySer {
    InventoryV1 { slots: Vec<Option<InvStackSer>> },
}

/// Schema for a nonempty [`inv::Slot`].
/// Not tagged since it will only appear inside an [`InventorySer`].
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct InvStackSer {
    pub(crate) count: NonZeroU16,
    pub(crate) item: inv::Tool,
}

/// Schema for [`inv::Tool`].
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum ToolSer {
    ActivateV1 {},
    RemoveBlockV1 {
        keep: bool,
    },
    BlockV1 {
        block: Block,
    },
    InfiniteBlocksV1 {
        block: Block,
    },
    CopyFromSpaceV1 {},
    EditBlockV1 {},
    PushPullV1 {},
    JetpackV1 {
        active: bool,
    },
    CustomV1 {
        op: crate::op::Operation,
        icon: Block,
    },
    ExternalActionV1 {
        icon: Block,
    },
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `math` module

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct AabSer {
    // This one isn't an enum because I expect we'll not need to change it
    pub(crate) lower: [f64; 3],
    pub(crate) upper: [f64; 3],
}

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct GridAabSer {
    // This one isn't an enum because I expect we'll not need to change it
    pub(crate) lower: [GridCoordinate; 3],
    pub(crate) upper: [GridCoordinate; 3],
}

type RgbSer = [NotNan<f32>; 3];

type RgbaSer = [NotNan<f32>; 4];

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `op` module

#[derive(Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum OperationSer {
    BecomeV1 { block: Block },
    PaintV1 { blocks: Vec<([i32; 3], Block)> },
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `space` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum SpaceSer<'a> {
    SpaceV1 {
        bounds: GridAab,
        physics: SpacePhysicsSerV1,
        blocks: Vec<Block>,
        contents: GzSerde<'a, Leu16>,
        light: Option<GzSerde<'a, LightSerV1>>,
        #[serde(default, skip_serializing_if = "behavior::BehaviorSet::is_empty")]
        behaviors: Cow<'a, behavior::BehaviorSet<space::Space>>,
        spawn: Cow<'a, character::Spawn>,
    },
}

/// Schema for serializing `PackedLight`.
///
/// Note: This is used inside `GzSerde`, so it must be endiannness-independent.
/// It accomplishes this by having only `u8`-sized fields.
#[derive(Clone, Copy, Debug, bytemuck::NoUninit, bytemuck::CheckedBitPattern)]
#[repr(C)]
pub(crate) struct LightSerV1 {
    pub value: [u8; 3],
    pub status: LightStatusSerV1,
}

#[derive(Clone, Copy, Debug, bytemuck::NoUninit, bytemuck::CheckedBitPattern)]
#[repr(u8)]
pub(crate) enum LightStatusSerV1 {
    Uninitialized = 0,
    NoRays = 1,
    Opaque = 2,
    Visible = 3,
}

/// Currently identical to `PackedLight`.
#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct SpacePhysicsSerV1 {
    pub gravity: [NotNan<f64>; 3],
    pub sky: SkySer,
    pub light: LightPhysicsSerV1,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum SkySer {
    UniformV1 { color: RgbSer },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum LightPhysicsSerV1 {
    NoneV1,
    RaysV1 { maximum_distance: u16 },
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `universe` module

/// Schema for `Universe` serialization and deserialization.
/// The type parameters allow for the different data types wanted in the serialization
/// case vs. the deserialization case.
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum UniverseSchema<C, S> {
    UniverseV1 {
        /// Note: We are currently targeting JSON output, which cannot use non-string keys.
        /// Therefore, this is not expressed as a map.
        members: Vec<MemberEntrySer<MemberSchema<C, S>>>,
    },
}
pub(crate) type UniverseSer =
    UniverseSchema<SerializeRef<character::Character>, SerializeRef<space::Space>>;
pub(crate) type UniverseDe = UniverseSchema<character::Character, space::Space>;

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct MemberEntrySer<T> {
    pub name: universe::Name,
    #[serde(flatten)]
    pub value: T,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "member_type")]
pub(crate) enum MemberSchema<C, S> {
    Block { value: Block },
    Character { value: C },
    Space { value: S },
}
pub(crate) type MemberSer =
    MemberSchema<SerializeRef<character::Character>, SerializeRef<space::Space>>;
pub(crate) type MemberDe = MemberSchema<character::Character, space::Space>;

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum URefSer {
    URefV1 {
        #[serde(flatten)]
        name: universe::Name,
    },
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub(crate) enum NameSer {
    Specific(ArcStr),
    Anonym(usize),
}
