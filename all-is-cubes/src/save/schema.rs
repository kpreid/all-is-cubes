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

#![allow(
    unexpected_cfgs,
    reason = "https://github.com/Lokathor/bytemuck/issues/286"
)]

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::num::NonZeroU16;

use arcstr::ArcStr;
use serde::{Deserialize, Serialize};

use crate::block::Block;
use crate::math::{
    Aab, Face6, GridAab, GridCoordinate, GridRotation, NotNan, PositiveSign, ZeroOne,
};
use crate::save::compress::{GzSerde, Leu16};
use crate::time::Schedule;
use crate::universe::Handle;
use crate::{behavior, block, character, inv, op, sound, space, tag, universe};

/// Placeholder type for when we want to serialize the *contents* of a `Handle`,
/// without cloning or referencing those contents immediately.
pub(crate) struct SerializeHandle<'t, T>(pub(crate) universe::ReadTicket<'t>, pub(crate) Handle<T>);

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
        primitive: PrimitiveSer,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        modifiers: Vec<ModifierSer<'a>>,
    },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum PrimitiveSer {
    AirV1,
    AtomV1 {
        color: RgbaSer,
        #[serde(default, skip_serializing_if = "is_default")]
        light_emission: RgbSer,
        /// Note: Attributes stored on the primitive are no longer used, and supported only for deserialization.
        #[serde(flatten)]
        attributes: BlockAttributesV1Ser,
        #[serde(default, skip_serializing_if = "is_default")]
        collision: BlockCollisionSer,
    },
    RecurV1 {
        /// Note: Attributes stored on the primitive are no longer used, and supported only for deserialization.
        #[serde(flatten)]
        attributes: BlockAttributesV1Ser,
        space: Handle<space::Space>,
        #[serde(default, skip_serializing_if = "is_default")]
        offset: [i32; 3],
        resolution: block::Resolution,
    },
    IndirectV1 {
        definition: Handle<block::BlockDef>,
    },
    TextPrimitiveV1 {
        text: TextSer,
        offset: [GridCoordinate; 3],
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct BlockAttributesV1Ser {
    #[serde(default, skip_serializing_if = "str::is_empty")]
    pub display_name: ArcStr,
    #[serde(default = "return_true", skip_serializing_if = "is_true")]
    pub selectable: bool,
    #[serde(default, skip_serializing_if = "is_default")]
    pub inventory: InvInBlockSer,
    #[serde(default, skip_serializing_if = "is_default")]
    pub rotation_rule: RotationPlacementRuleSer,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub placement_action: Option<PlacementActionSer>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tick_action: Option<TickActionSer>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub activation_action: Option<op::Operation>,
    #[serde(default, skip_serializing_if = "is_default")]
    pub animation_hint: AnimationHintSer,
}
fn return_true() -> bool {
    true
}
#[expect(clippy::trivially_copy_pass_by_ref)]
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

#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum RotationPlacementRuleSer {
    #[default]
    NeverV1,
    AttachV1 {
        by: Face6,
    },
}

/// Unversioned because it's versioned by the parent struct
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct TickActionSer {
    pub operation: op::Operation,
    pub schedule: Schedule,
}
/// Unversioned because it's versioned by the parent struct
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct PlacementActionSer {
    pub operation: op::Operation,
    pub in_front: bool,
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

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum ModifierSer<'a> {
    AttributesV1 {
        #[serde(flatten)]
        attributes: BlockAttributesV1Ser,
    },
    TagV1 {
        tag: tag::Tag,
    },
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
    /// This is called "`BlockInventory`" rather than "`Inventory`" so that the
    /// variant tags are unique across the entire schema, which might be useful
    /// for future compatibility or other applications accessing the data.
    BlockInventoryV1 {
        inventory: Cow<'a, inv::Inventory>,
    },
    #[serde(untagged)]
    Move(MoveSer),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum MoveSer {
    MoveV1 {
        direction: Face6,
        distance: u16,
        velocity: i16,
        schedule: Schedule,
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
    LogoV1,
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
        space: Handle<space::Space>,
        body: Cow<'a, crate::physics::Body>,
        inventory: Cow<'a, inv::Inventory>,
        selected_slots: [inv::Ix; 3],
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
    RemoveBlockV1 { keep: bool },
    BlockV1 { block: Block },
    InfiniteBlocksV1 { block: Block },
    CopyFromSpaceV1 {},
    EditBlockV1 {},
    PushPullV1 {},
    JetpackV1 { active: bool },
    CustomV1 { op: op::Operation, icon: Block },
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(tag = "type")]
pub(crate) enum InvInBlockSer {
    InvInBlockV1 {
        size: inv::Ix,
        icon_scale: block::Resolution,
        icon_resolution: block::Resolution,
        icon_rows: Vec<IconRowSerV1>,
    },
}
impl Default for InvInBlockSer {
    fn default() -> Self {
        (&inv::InvInBlock::EMPTY).into()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
pub(crate) struct IconRowSerV1 {
    pub(crate) first_slot: inv::Ix,
    pub(crate) count: inv::Ix,
    pub(crate) origin: [i32; 3],
    pub(crate) stride: [i32; 3],
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `math` module

type RgbSer = [PositiveSign<f32>; 3];

type RgbaSer = (
    PositiveSign<f32>,
    PositiveSign<f32>,
    PositiveSign<f32>,
    ZeroOne<f32>,
);

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `op` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum OperationSer<'a> {
    AltV1 {
        ops: Cow<'a, [op::Operation]>,
    },
    BecomeV1 {
        block: Block,
    },
    DestroyToV1 {
        block: Block,
    },
    ReplaceV1 {
        old: Block,
        new: Block,
        conserved: bool,
        optional: bool,
    },
    AddModifiersV1 {
        modifiers: Cow<'a, [ModifierSer<'a>]>,
    },
    StartMoveV1 {
        modifier: MoveSer,
    },
    MoveInventoryV1 {
        transfer_into_adjacent: Option<Face6>,
    },
    TakeInventoryV1 {
        destroy_if_empty: bool,
    },
    NeighborsV1 {
        neighbors: Cow<'a, [([i32; 3], op::Operation)]>,
    },
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `physics` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum BodySer {
    BodyV1 {
        // TODO: NotNan is not strict enough. If an infinity sneaks in, this will fail to deserialize.
        position: [NotNan<f64>; 3],
        velocity: [NotNan<f64>; 3],
        collision_box: Aab,
        occupying: Aab,
        flying: bool,
        noclip: bool,
        yaw: f64,
        pitch: f64,
    },
}
//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `sound` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum SoundDefSer {
    SynthesizedSoundV1 {
        duration: ZeroOne<f32>,
        frequency: PositiveSign<f32>,
        amplitude: ZeroOne<f32>,
    },
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
    OctantsV1 { colors: [RgbSer; 8] },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum LightPhysicsSerV1 {
    NoneV1,
    RaysV1 { maximum_distance: u8 },
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `tag` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum TagSer {
    TagHandleV1 { handle: Handle<tag::TagDef> },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum TagDefSer {
    TagDefV1 {},
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `time` module

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum ScheduleSer {
    ScheduleV1 { period: NonZeroU16 },
}

//------------------------------------------------------------------------------------------------//
// Schema corresponding to the `universe` module

/// Schema for `Universe` serialization and deserialization.
/// The type parameters allow for the different data types wanted in the serialization
/// case vs. the deserialization case.
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum UniverseSchema<C, So, Sp, T> {
    UniverseV1 {
        /// Note: We are currently targeting JSON output, which cannot use non-string keys.
        /// Therefore, this is not expressed as a map.
        members: Vec<MemberEntrySer<MemberSchema<C, So, Sp, T>>>,
    },
}
pub(crate) type UniverseSer<'t> = UniverseSchema<
    SerializeHandle<'t, character::Character>,
    SerializeHandle<'t, sound::SoundDef>,
    SerializeHandle<'t, space::Space>,
    SerializeHandle<'t, tag::TagDef>,
>;
pub(crate) type UniverseDe = UniverseSchema<
    Box<character::Character>,
    Box<sound::SoundDef>,
    Box<space::Space>,
    Box<tag::TagDef>,
>;

#[derive(Debug, Deserialize, Serialize)]
pub(crate) struct MemberEntrySer<T> {
    pub name: universe::Name,
    #[serde(flatten)]
    pub value: T,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "member_type")]
pub(crate) enum MemberSchema<C, So, Sp, T> {
    // Cheap-to-clone members can have their values directly here;
    // expensive or unclonable ones need to be serialized by handle.
    // TODO: Should TagDef be really counted as expensive?
    Block { value: Block },
    Character { value: C },
    Sound { value: So },
    Space { value: Sp },
    Tag { value: T },
}
pub(crate) type MemberSer<'t> = MemberSchema<
    SerializeHandle<'t, character::Character>,
    SerializeHandle<'t, sound::SoundDef>,
    SerializeHandle<'t, space::Space>,
    SerializeHandle<'t, tag::TagDef>,
>;
pub(crate) type MemberDe = MemberSchema<
    // the Boxes are not strictly necessary but are expected by other code
    Box<character::Character>,
    Box<sound::SoundDef>,
    Box<space::Space>,
    Box<tag::TagDef>,
>;

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type")]
pub(crate) enum HandleSer {
    HandleV1 {
        #[serde(flatten)]
        name: universe::Name,
    },
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub(crate) enum NameSer {
    Specific(ArcStr),
    Anonym(usize),
}
