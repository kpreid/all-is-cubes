//! First-run game content. (Well, all runs, since we don't have saving yet.)

use macro_rules_attribute::macro_rules_derive;
use paste::paste;

use all_is_cubes::block::Block;
use all_is_cubes::cgmath::{EuclideanSpace as _, Point3};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::linking::{BlockProvider, GenError, InGenError};
use all_is_cubes::math::{
    Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridVector, Rgb, Rgba,
};
use all_is_cubes::space::{LightPhysics, Space};
use all_is_cubes::universe::{Name, URef, Universe, UniverseIndex};
use all_is_cubes::util::YieldProgress;

use crate::fractal::menger_sponge;
use crate::menu::template_menu;
use crate::{atrium::atrium, demo_city, dungeon::demo_dungeon, install_demo_blocks};
use crate::{wavy_landscape, LandscapeBlocks};

/// Generate a `#[test]` function for each element of [`UniverseTemplate`].
/// This macro is used as a derive macro via [`macro_rules_derive`].
macro_rules! generate_template_test {
    (
        $(#[$type_meta:meta])*
        $vis:vis enum $enum_name:ident {
            $(
                $( #[doc = $doc:literal] )*
                $( #[cfg($variant_cfg:meta)] )*
                $variant_name:ident
            ),* $(,)?
        }
    ) => {
        $(
            paste! {
                $( #[cfg($variant_cfg)] )*
                #[cfg(test)]
                #[tokio::test]
                #[allow(non_snake_case)]
                async fn [< template_ $variant_name >] () {
                    tests::check_universe_template($enum_name::$variant_name).await;
                }
            }
        )*
    }
}

/// Selection of initial content for constructing a new [`Universe`].
//
// TODO: Stop using strum, because we will eventually want parameterized templates.
#[derive(
    Clone,
    Debug,
    Eq,
    Hash,
    PartialEq,
    strum::Display,
    strum::EnumString,
    strum::EnumIter,
    strum::IntoStaticStr,
)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
#[macro_rules_derive(generate_template_test!)]
pub enum UniverseTemplate {
    /// Provides an interactive menu of other templates.
    Menu,

    /// New universe with no contents at all.
    Blank,

    /// Always produces an error, for testing error-handling functionality.
    Fail,

    /// Space with assorted “exhibits” demonstrating or testing various features of All is Cubes.
    DemoCity,

    /// Randomly generated connected rooms.
    /// Someday this might have challenges or become a tutorial.
    Dungeon,

    /// Large space with separate floating islands.
    Islands,

    /// A procedural voxel version of the classic [Sponza] Atrium rendering test scene.
    ///
    /// [Sponza]: https://en.wikipedia.org/wiki/Sponza_Palace
    Atrium,

    /// A procedural voxel version of the classic [Cornell Box] rendering test scene.
    ///
    /// [Cornell Box]: https://en.wikipedia.org/wiki/Cornell_box
    CornellBox,

    /// A [Menger sponge] fractal.
    ///
    /// [Menger sponge]: https://en.wikipedia.org/wiki/Menger_sponge
    MengerSponge,

    /// A test scene containing various shapes and colors to exercise the lighting algorithm.
    LightingBench,

    /// Use entirely random choices.
    ///
    /// TODO: This doesn't yet produce anything even visible — we need more sanity constraints.
    #[cfg(feature = "arbitrary")]
    Random,
    // TODO: add an "nothing, you get a blank editor" option once we have enough editing support.
}

impl UniverseTemplate {
    /// Whether the template should be shown to users.
    /// (This does not control )
    pub fn include_in_lists(&self) -> bool {
        use UniverseTemplate::*;
        match self {
            DemoCity | Dungeon | Atrium | Islands | CornellBox | MengerSponge | LightingBench => {
                true
            }

            // Itself a list of templates!
            Menu => false,

            // More testing than interesting demos.
            Blank | Fail => false,

            #[cfg(feature = "arbitrary")]
            Random => false,
        }
    }

    /// Create a new [`Universe`] based on this template's specifications.
    pub async fn build(
        self,
        p: YieldProgress,
        params: TemplateParameters,
    ) -> Result<Universe, GenError> {
        let mut universe = Universe::new();

        // TODO: Later we want a "module loading" system that can lazily bring in content.
        // For now, unconditionally add all these blocks.
        let [demo_blocks_progress, p] = p.split(0.1);
        install_demo_blocks(&mut universe, demo_blocks_progress).await?;
        p.progress(0.).await;

        let default_space_name: Name = "space".into();

        let mut p = Some(p);
        use UniverseTemplate::*;
        let maybe_space: Option<Result<Space, InGenError>> = match self {
            Menu => Some(template_menu(&mut universe)),
            Blank => None,
            Fail => Some(Err(InGenError::Other(
                "the Fail template always fails to generate".into(),
            ))),
            DemoCity => {
                Some(demo_city(&mut universe, p.take().unwrap(), params.seed.unwrap_or(0)).await)
            }
            Dungeon => {
                Some(demo_dungeon(&mut universe, p.take().unwrap(), params.seed.unwrap_or(0)).await)
            }
            Islands => Some(islands(&mut universe, p.take().unwrap(), 1000).await),
            Atrium => Some(atrium(&mut universe, p.take().unwrap()).await),
            CornellBox => Some(cornell_box()),
            MengerSponge => Some(menger_sponge(&mut universe, 4)),
            LightingBench => Some(all_is_cubes::content::testing::lighting_bench_space(
                &mut universe,
            )),
            #[cfg(feature = "arbitrary")]
            Random => Some(
                arbitrary_space(&mut universe, p.take().unwrap(), params.seed.unwrap_or(0)).await,
            ),
        };

        if let Some(p) = p {
            p.progress(1.0).await;
        }

        // Insert the space and generate the initial character.
        if let Some(space_result) = maybe_space {
            let space_ref =
                insert_generated_space(&mut universe, default_space_name, space_result)?;

            // TODO: "character" is a special default name used for finding the character the
            // player actually uses, and we should replace that or handle it more formally.
            universe.insert("character".into(), Character::spawn_default(space_ref))?;
        }

        Ok(universe)
    }
}

impl Default for UniverseTemplate {
    fn default() -> Self {
        Self::DemoCity
    }
}

/// TODO: This should be a general Universe tool for "insert a generated value or report an error"
/// but for now was written to help out `UniverseTemplate::build`
fn insert_generated_space(
    universe: &mut Universe,
    name: Name,
    result: Result<Space, InGenError>,
) -> Result<URef<Space>, GenError> {
    match result {
        Ok(space) => Ok(universe.insert(name, space)?),
        Err(e) => Err(GenError::failure(e, name)),
    }
}

/// Configuration for exactly what a [`UniverseTemplate`] should produce.
///
/// Pass this structure to [`UniverseTemplate::build()`].
#[derive(Clone, Debug, Default, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct TemplateParameters {
    /// Seed for any randomization which the template performs.
    /// Not all templates have random elements.
    ///
    /// The seed is optional so that user input processing can distinguish whether the
    /// seed was explicitly specified. If a template receives a seed of `None`, (TODO
    /// define what should happen. Fail? Treat equal to `Some(0)`?)
    //---
    // Design note: u64 was chosen because both `std::hash::Hasher` and `rand::SeedableRng`
    // agree on this many bits for seeds.
    pub seed: Option<u64>,
}

// -- Specific templates below this point ---

async fn islands(
    universe: &mut Universe,
    p: YieldProgress,
    diameter: GridCoordinate,
) -> Result<Space, InGenError> {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;

    // Set up dimensions
    let bounds = {
        let height = 400;
        let low_corner = diameter / -2;
        GridAab::from_lower_size(
            [low_corner, height / -2, low_corner],
            [diameter, height, diameter],
        )
    };

    let mut space = Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn.set_eye_position(bounds.center());
            // TODO: Make this tidier by having a "shrink to centermost point or cube" operation on GridAab
            let cp = bounds.center().map(|c| c as GridCoordinate);
            spawn.set_bounds(GridAab::from_lower_size(
                cp - GridVector::new(30, 30, 30),
                [60, 60, 60],
            ));
            spawn
        })
        .build();

    // Set up grid in which islands are placed
    let island_stride = 50;
    let island_grid = bounds.divide(island_stride);

    for (i, island_pos) in island_grid.interior_iter().enumerate() {
        let cell_bounds = GridAab::from_lower_size(
            Point3::from_vec(island_pos.to_vec() * island_stride),
            [island_stride, island_stride, island_stride],
        )
        .intersection(bounds)
        .expect("island outside space bounds");
        // TODO: randomize island location in cell?
        let occupied_bounds = cell_bounds.expand(FaceMap::repeat(-10).with(Face6::PY, -25));
        wavy_landscape(occupied_bounds, &mut space, &landscape_blocks, 0.5)?;
        p.progress(i as f32 / island_grid.volume() as f32).await;
    }

    Ok(space)
}

#[rustfmt::skip]
fn cornell_box() -> Result<Space, InGenError> {
    // Coordinates are set up based on this dimension because, being blocks, we're not
    // going to *exactly* replicate the original data, but we might want to adjust the
    // scale to something else entirely.
    let box_size = 55;
    // Add one block to all sides for wall thickness.
    let bounds = GridAab::from_lower_size(
        [-1, -1, -1],
        GridVector::new(1, 1, 1) * box_size + GridVector::new(2, 2, 2),
    );
    let mut space = Space::builder(bounds)
        // There shall be no light but that which we make for ourselves!
        .sky_color(Rgb::ZERO)
        .light_physics(LightPhysics::Rays {
            maximum_distance: (box_size * 2).try_into().unwrap_or(u16::MAX),
        })
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn.set_eye_position(Point3::<FreeCoordinate>::new(0.5, 0.5, 1.6) * box_size.into());
            spawn
        })
        .build();

    let white: Block = Rgba::new(1.0, 1.0, 1.0, 1.0).into();
    let red: Block = Rgba::new(0.57, 0.025, 0.025, 1.0).into();
    let green: Block = Rgba::new(0.025, 0.236, 0.025, 1.0).into();
    let light: Block = Block::builder()
        .display_name("Light")
        .light_emission(Rgb::new(40., 40., 40.))
        .color(Rgba::new(1.0, 1.0, 1.0, 1.0))
        .build();

    // Floor.
    space.fill_uniform(GridAab::from_lower_size([0, -1, 0], [box_size, 1, box_size]), &white)?;
    // Ceiling.
    space.fill_uniform(GridAab::from_lower_size([0, box_size, 0], [box_size, 1, box_size]), &white)?;
    // Light in ceiling.
    space.fill_uniform(GridAab::from_lower_upper([21, box_size, 23], [34, box_size + 1, 33]), &light)?;
    // Back wall.
    space.fill_uniform(GridAab::from_lower_size([0, 0, -1], [box_size, box_size, 1]), &white)?;
    // Right wall (green).
    space.fill_uniform(GridAab::from_lower_size([box_size, 0, 0], [1, box_size, box_size]), &green)?;
    // Left wall (red).
    space.fill_uniform(GridAab::from_lower_size([-1, 0, 0], [1, box_size, box_size]), &red)?;

    // Block #1
    space.fill_uniform(GridAab::from_lower_size([29, 0, 36], [16, 16, 15]), &white)?;
    // Block #2
    space.fill_uniform(GridAab::from_lower_size([10, 0, 13], [18, 33, 15]), &white)?;

    Ok(space)
}

#[cfg(feature = "arbitrary")]
async fn arbitrary_space(
    _: &mut Universe,
    mut progress: YieldProgress,
    seed: u64,
) -> Result<Space, InGenError> {
    use all_is_cubes::cgmath::{Vector3, Zero};
    use arbitrary::{Arbitrary, Error, Unstructured};
    use rand::{RngCore, SeedableRng};

    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);
    let mut bytes = [0u8; 16384];
    let mut attempt = 0;
    loop {
        attempt += 1;
        rng.fill_bytes(&mut bytes);
        let r: Result<Space, _> = Arbitrary::arbitrary(&mut Unstructured::new(&bytes));
        match r {
            Ok(mut space) => {
                // Patch spawn position to be reasonable
                let bounds = space.bounds();
                let mut spawn = space.spawn().clone();
                spawn.set_bounds(bounds.expand(FaceMap::repeat(20)));
                spawn.set_eye_position(bounds.center());
                space.set_spawn(spawn);

                // Patch physics to be reasonable
                let mut p = space.physics().clone();
                p.gravity = Vector3::zero(); // won't be a floor
                p.sky_color = p.sky_color * (0.5 / p.sky_color.luminance());
                space.set_physics(p);

                // TODO: These patches are still not enough to get a good result.

                return Ok(space);
            }
            Err(Error::IncorrectFormat) => {
                if attempt >= 1000000 {
                    return Err(InGenError::Other("Out of attempts".into()));
                }
            }
            Err(e) => panic!("{}", e),
        }
        progress = progress.finish_and_cut(0.1).await; // mostly nonsense but we do want to yield
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::time::Tick;
    use futures_core::future::BoxFuture;

    #[allow(clippy::let_underscore_future)]
    fn _test_build_future_is_send() {
        let _: BoxFuture<'_, _> = Box::pin(
            UniverseTemplate::Atrium.build(YieldProgress::noop(), TemplateParameters::default()),
        );
    }

    pub(super) async fn check_universe_template(template: UniverseTemplate) {
        let result = if let UniverseTemplate::Islands = template {
            // Kludge: the islands template is known to be very slow.
            // We should work on making what it does faster, but for now, let's
            // run a much smaller instance of it for the does-it-succeed test.
            // TODO: This should instead be handled by the template having an official
            // user-configurable size parameter.

            let mut universe = Universe::new();
            install_demo_blocks(&mut universe, YieldProgress::noop())
                .await
                .unwrap();
            islands(&mut universe, YieldProgress::noop(), 100)
                .await
                .unwrap();

            return; // TODO: skipping final test because we didn't create the character
        } else {
            template
                .clone()
                .build(
                    YieldProgress::noop(),
                    TemplateParameters {
                        seed: Some(0x7f16dfe65954583e),
                    },
                )
                .await
        };

        if matches!(template, UniverseTemplate::Fail) {
            // The Fail template _should_ return an error
            result.unwrap_err();
        } else {
            let mut u = result.unwrap();

            if template != UniverseTemplate::Blank {
                let _ = u.get_default_character().unwrap().read().unwrap();
            }
            u.step(Tick::arbitrary());
        }
    }
}
