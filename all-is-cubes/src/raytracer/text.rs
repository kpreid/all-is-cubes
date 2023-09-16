//! Text-based raytracing output.

#![cfg_attr(not(feature = "std"), allow(unused_imports))]

use alloc::borrow::{Cow, ToOwned};
use alloc::string::{String, ToString};

use euclid::vec2;

use crate::camera::{eye_for_look_at, Camera, GraphicsOptions, Viewport};
use crate::math::{FreeVector, Rgba};
use crate::raytracer::{Accumulate, RaytraceInfo, RtBlockData, RtOptionsRef, SpaceRaytracer};
use crate::space::{Space, SpaceBlockData};

/// TODO: better name, docs
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct CharacterRtData(pub Cow<'static, str>);

impl RtBlockData for CharacterRtData {
    type Options = ();

    fn from_block(_: RtOptionsRef<'_, Self::Options>, s: &SpaceBlockData) -> Self {
        // TODO: For more Unicode correctness, index by grapheme cluster
        // TODO: allow customizing the fallback character
        Self(
            s.evaluated()
                .attributes
                .display_name
                .chars()
                .next()
                .map(|c| Cow::Owned(c.to_string()))
                .unwrap_or(Cow::Borrowed("#")),
        )
    }

    fn error(_: RtOptionsRef<'_, Self::Options>) -> Self {
        Self(Cow::Borrowed("X"))
    }

    fn sky(_: RtOptionsRef<'_, Self::Options>) -> Self {
        Self(Cow::Borrowed(" "))
    }
}

/// Implements [`Accumulate`] for text output: captures the first characters of block names
/// rather than colors.
#[derive(Clone, Debug, Default, PartialEq)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct CharacterBuf {
    /// Text to draw, if determined yet.
    hit_text: Option<String>,
}

impl Accumulate for CharacterBuf {
    type BlockData = CharacterRtData;

    #[inline]
    fn opaque(&self) -> bool {
        self.hit_text.is_some()
    }

    #[inline]
    fn add(&mut self, _surface_color: Rgba, d: &Self::BlockData) {
        if self.hit_text.is_none() {
            self.hit_text = Some(String::from(d.0.clone()));
        }
    }

    fn hit_nothing(&mut self) {
        self.hit_text = Some(".".to_owned());
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        // TODO: we should at least find the mode (or maybe prefer None) instead of the first
        Self {
            hit_text: items.into_iter().flat_map(|cb| cb.hit_text).next(),
        }
    }
}

impl From<CharacterBuf> for String {
    #[inline]
    fn from(buf: CharacterBuf) -> String {
        buf.hit_text.unwrap_or_else(|| ".".to_owned())
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
    print_space_impl(space, direction.into(), &mut |s| {
        std::print!("{s}");
    });
}

/// Version of `print_space` that takes a destination, for testing, and is non-generic,
/// for build performance.
#[cfg(any(feature = "std", test))]
fn print_space_impl(
    space: &Space,
    direction: FreeVector,
    write: &mut dyn FnMut(&str),
) -> RaytraceInfo {
    // TODO: optimize height (and thus aspect ratio) for the shape of the space
    let mut camera = Camera::new(
        GraphicsOptions::default(),
        Viewport {
            nominal_size: vec2(40., 40.),
            framebuffer_size: vec2(80, 40),
        },
    );
    camera.look_at_y_up(
        eye_for_look_at(space.bounds(), direction),
        space.bounds().center(),
    );

    SpaceRaytracer::<CharacterRtData>::new(space, GraphicsOptions::default(), ())
        .trace_scene_to_text::<CharacterBuf, _, _>(&camera, "\n", move |s| {
            write(s);
            let r: Result<(), ()> = Ok(());
            r
        })
        .unwrap()
}

#[cfg(test)]
mod tests {
    use euclid::vec3;

    use super::*;
    use crate::block::{Block, Resolution::R4};
    use crate::content::make_some_blocks;
    use crate::universe::Universe;

    #[test]
    fn print_space_test() {
        let mut space = Space::empty_positive(3, 1, 1);
        let [b0, b1, b2] = make_some_blocks();
        space.set([0, 0, 0], &b0).unwrap();
        space.set([1, 0, 0], &b1).unwrap();
        space.set([2, 0, 0], &b2).unwrap();

        let mut output = String::new();
        print_space_impl(&space, vec3(1., 1., 1.), &mut |s| output += s);
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
        let mut block_space = Space::empty_positive(4, 2, 4);
        block_space
            .fill_uniform(block_space.bounds(), Block::from(Rgba::WHITE))
            .unwrap();
        let space_ref = universe.insert_anonymous(block_space);
        let partial_block = Block::builder()
            .voxels_ref(resolution, space_ref.clone())
            .display_name("P")
            .build();

        let mut space = Space::empty_positive(2, 1, 1);
        let [b0] = make_some_blocks();
        space.set([0, 0, 0], &b0).unwrap();
        space.set([1, 0, 0], &partial_block).unwrap();

        let mut output = String::new();
        print_space_impl(&space, vec3(1., 1., 1.), &mut |s| output += s);
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
