//! Items to assist with the definition of exhibits in the demo city.

use all_is_cubes::linking::InGenError;
use all_is_cubes::math::GridCoordinate;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;
use all_is_cubes_ui::vui::widgets;

use crate::city::CityPlanner;

pub(crate) struct Exhibit {
    pub name: &'static str,
    pub subtitle: &'static str,
    pub placement: Placement,
    pub factory: for<'a> fn(Context<'a>) -> Result<(Space, ExhibitTransaction), InGenError>,
}

pub(crate) struct Context<'a> {
    pub(super) exhibit: &'a Exhibit,
    pub(super) universe: &'a Universe,
    pub(super) widget_theme: &'a widgets::WidgetTheme,
}

/// How an exhibit should be placed in the city surroundings.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Placement {
    Surface,
    Underground,
}

impl Placement {
    pub(crate) fn floor(self) -> GridCoordinate {
        match self {
            Placement::Surface => CityPlanner::SURFACE_Y,
            Placement::Underground => CityPlanner::UNDERGROUND_FLOOR_Y,
        }
    }
}

pub(crate) type ExhibitTransaction = all_is_cubes::universe::UniverseTransaction;

macro_rules! exhibit {
    attr($( $fields:tt )*) (
        fn $name:ident($( $args:tt )*) {
            $( $body:tt )*
        }
    ) => {
        pub(in crate::city) const $name: Exhibit = Exhibit {
            factory: |$( $args )*| { $( $body )* },
            $( $fields )*
        };
    }
}
pub(super) use exhibit;
