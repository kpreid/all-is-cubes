//! Text-based raytracing output.

#![cfg_attr(not(feature = "std"), allow(unused_imports))]

use alloc::string::String;
use core::fmt;

use arcstr::{ArcStr, Substr, literal};
use euclid::size2;
use unicode_segmentation::UnicodeSegmentation;

use crate::camera::{Camera, GraphicsOptions, Viewport, eye_for_look_at};
use crate::math::FreeVector;
use crate::raytracer::{Accumulate, Exception, RtBlockData, RtOptionsRef, SpaceRaytracer};
use crate::space::{Space, SpaceBlockData};

/// If you are using [`CharacterBuf`], use this [`RtBlockData`] implementation.
// TODO: better name
#[derive(Clone, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
// TODO: field would ideally be private
pub struct CharacterRtData(pub Substr);

impl RtBlockData for CharacterRtData {
    type Options = ();

    // TODO: not using `arcstr::literal_substr!()` because it is broken by `feature(new_range)`.
    // <https://github.com/rust-lang/rust/issues/148342>

    fn from_block(_: RtOptionsRef<'_, Self::Options>, s: &SpaceBlockData) -> Self {
        // TODO: allow customizing the fallback character
        let name: &ArcStr = &s.evaluated().attributes().display_name;
        let char = match name.graphemes(true).next() {
            Some(grapheme) => name.substr_from(grapheme),
            None => literal!("#").substr(..),
        };
        Self(char)
    }

    fn exception(exception: Exception, _: RtOptionsRef<'_, Self::Options>) -> Self {
        match exception {
            Exception::Sky => Self(literal!(" ").substr(..)),
            Exception::Incomplete => Self(literal!("X").substr(..)),
            _ => Self(literal!(" ").substr(..)), // TODO: what is the best paint behavior for text?
        }
    }
}

/// Implements [`Accumulate`] for text output: captures the first characters of block names
/// rather than colors.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CharacterBuf(State);
#[derive(Clone, Debug, Default, PartialEq)]
enum State {
    #[default]
    Empty,
    EnteredSpace,
    Hit(Substr),
}

impl CharacterBuf {
    /// Update the state to include the given character if none is already present,
    /// as if [`Accumulate::add()`] was called.
    pub fn add_character_hit(&mut self, character: &Substr) {
        match self.0 {
            State::Hit(_) => {}
            State::Empty | State::EnteredSpace => {
                self.0 = State::Hit(character.clone());
            }
        }
    }
}

impl Accumulate for CharacterBuf {
    type BlockData = CharacterRtData;

    #[inline]
    fn opaque(&self) -> bool {
        match self.0 {
            State::Hit(_) => true,
            State::Empty | State::EnteredSpace => false,
        }
    }

    #[inline]
    fn add(&mut self, hit: super::Hit<'_, Self::BlockData>) {
        match (hit.exception, &self.0) {
            (Some(Exception::EnterSpace), State::Empty | State::EnteredSpace) => {
                self.0 = State::EnteredSpace
            }
            (Some(Exception::Sky), _) => {}
            (_, _) => self.add_character_hit(&hit.block.0),
        }
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        // TODO: pick the mode instead of the first
        items
            .into_iter()
            .reduce(|cb1, cb2| {
                Self(match (cb1.0, cb2.0) {
                    (State::Hit(c), _) | (_, State::Hit(c)) => State::Hit(c),
                    (State::EnteredSpace, State::EnteredSpace) => State::EnteredSpace,
                    _ => State::Empty,
                })
            })
            .unwrap()
    }
}

impl From<CharacterBuf> for Substr {
    #[inline]
    fn from(buf: CharacterBuf) -> Substr {
        match buf.0 {
            State::Empty => literal!(".").substr(..),
            State::EnteredSpace => literal!(" ").substr(..),
            State::Hit(character) => character,
        }
    }
}
impl From<CharacterBuf> for String {
    #[inline]
    fn from(buf: CharacterBuf) -> String {
        Substr::from(buf).to_string()
    }
}

/// Print an image of the given space as “ASCII art”.
///
/// Intended for use in tests, to visualize the results in case of failure.
/// Accordingly, it always writes to the same destination as [`print!`](std::print) (which is
/// redirected when tests are run).
///
/// `direction` specifies the direction from which the camera will be looking towards
/// the center of the space. The text output will be 80 columns wide.
#[cfg(any(feature = "std", test))]
pub fn print_space(space: &Space, direction: impl Into<FreeVector>) {
    std::print!(
        "{}",
        PrintSpace {
            space,
            direction: direction.into()
        }
    );
}

/// Wrapper struct for the implementation of [`print_space()`].
/// and which allows testing its output.
#[cfg(any(feature = "std", test))]
struct PrintSpace<'a> {
    space: &'a Space,
    direction: FreeVector,
}

#[cfg(any(feature = "std", test))]
impl fmt::Display for PrintSpace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: optimize height (and thus aspect ratio) for the shape of the space

        let mut camera = Camera::new(
            GraphicsOptions::default(),
            Viewport {
                nominal_size: size2(40., 40.),
                framebuffer_size: size2(80, 40),
            },
        );
        camera.look_at_y_up(
            eye_for_look_at(self.space.bounds(), self.direction),
            self.space.bounds().center(),
        );

        SpaceRaytracer::<CharacterRtData>::new(self.space, GraphicsOptions::default(), ())
            .to_text::<CharacterBuf>(&camera, "\n")
            .fmt(f)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{self, Block, Resolution::R4};
    use crate::content::make_some_blocks;
    use crate::math::{GridAab, Rgba};
    use crate::universe::Universe;
    use euclid::vec3;
    use std::string::ToString;

    #[test]
    fn print_space_test() {
        let space = Space::builder(GridAab::from_lower_size(
            [0, 0, 0],
            euclid::Size3D::new(3, 1, 1).cast(),
        ))
        .build_and_mutate(|m| {
            let [b0, b1, b2] = make_some_blocks();
            m.set([0, 0, 0], &b0).unwrap();
            m.set([1, 0, 0], &b1).unwrap();
            m.set([2, 0, 0], &b2).unwrap();
            Ok(())
        })
        .unwrap();

        let output = PrintSpace {
            space: &space,
            direction: vec3(1., 1., 1.),
        }
        .to_string();
        print!("{output}");
        pretty_assertions::assert_eq!(
            output,
            "\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ...........................0000000000...........................................\n\
            .......................0000000000000001111......................................\n\
            ........................000000001111111111111111................................\n\
            ........................00000011111111111111111112222...........................\n\
            .........................0000011111111111111122222222222222.....................\n\
            .........................000001111111111112222222222222222222222................\n\
            ...........................000011111111122222222222222222222222222..............\n\
            .............................001111111112222222222222222222222222...............\n\
            ...............................111111111222222222222222222222222................\n\
            ..................................11111122222222222222222222222.................\n\
            ....................................11112222222222222222222222..................\n\
            .......................................1222222222222222222222...................\n\
            .........................................2222222222222222222....................\n\
            ............................................222222222222222.....................\n\
            ..............................................22222222222.......................\n\
            ................................................22222222........................\n\
            ...................................................2222.........................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
        "
        );
    }

    /// Check that blocks with small spaces are handled without out-of-bounds errors
    #[test]
    fn partial_voxels() {
        let resolution = R4;
        let mut universe = Universe::new();
        let block_space = Space::builder(GridAab::from_lower_size([0, 0, 0], [4, 2, 4]))
            .filled_with(block::from_color!(Rgba::WHITE))
            .build();
        let space_handle = universe.insert_anonymous(block_space);
        let partial_block = Block::builder()
            .voxels_handle(resolution, space_handle.clone())
            .display_name("P")
            .build();

        let [b0] = make_some_blocks();
        let space = Space::builder(GridAab::from_lower_size(
            [0, 0, 0],
            euclid::Size3D::new(2, 1, 1).cast(),
        ))
        .read_ticket(universe.read_ticket())
        .build_and_mutate(|m| {
            m.set([0, 0, 0], &b0).unwrap();
            m.set([1, 0, 0], &partial_block).unwrap();
            Ok(())
        })
        .unwrap();

        let output = PrintSpace {
            space: &space,
            direction: vec3(1., 1., 1.),
        }
        .to_string();
        print!("{output}");
        pretty_assertions::assert_eq!(
            output,
            "\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ...............................000000...........................................\n\
                ......................0000000000000000000000....................................\n\
                ...................0000000000000000000000000000    .............................\n\
                ....................000000000000000000000000000           ......................\n\
                ....................000000000000000000000000000                  ...............\n\
                .....................0000000000000000000000000                       ...........\n\
                ......................00000000000000000000000PP                      ...........\n\
                ......................00000000000000000000PPPPPPPPPP                ............\n\
                .......................000000000000000PPPPPPPPPPPPPPPPPP           .............\n\
                .......................000000000000PPPPPPPPPPPPPPPPPPPPPPPPPP     ..............\n\
                .........................0000000PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP...............\n\
                ............................0000PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP................\n\
                ..............................00PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP.................\n\
                ................................0PPPPPPPPPPPPPPPPPPPPPPPPPPPPP..................\n\
                ..................................PPPPPPPPPPPPPPPPPPPPPPPPPPP...................\n\
                ....................................PPPPPPPPPPPPPPPPPPPPPPP.....................\n\
                ......................................PPPPPPPPPPPPPPPPPPPP......................\n\
                ........................................PPPPPPPPPPPPPPPPP.......................\n\
                ..........................................PPPPPPPPPPPPP.........................\n\
                ............................................PPPPPPPPPP..........................\n\
                ..............................................PPPPPP............................\n\
                ................................................PPP.............................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
                ................................................................................\n\
            "
        );
    }
}
