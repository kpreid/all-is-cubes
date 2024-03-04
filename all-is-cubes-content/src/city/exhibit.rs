//! Items to assist with the definition of exhibits in the demo city.

use all_is_cubes::linking::InGenError;
use all_is_cubes::math::GridCoordinate;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;

use crate::city::CityPlanner;

#[allow(clippy::type_complexity)]
pub(crate) struct Exhibit {
    pub name: &'static str,
    pub subtitle: &'static str,
    pub placement: Placement,
    pub factory:
        for<'a> fn(&'a Exhibit, &'a Universe) -> Result<(Space, ExhibitTransaction), InGenError>,
}

/// How an exhibit should be placed in the city surroundings.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Placement {
    Surface,
    #[allow(unused)] // TODO: polish this and then use it
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
    (
        #[exhibit($( $fields:tt )*)]
        fn $name:ident($( $args:tt )*) {
            $( $body:tt )*
        }
    ) => {
        const $name: Exhibit = Exhibit {
            factory: |$( $args )*| { $( $body )* },
            $( $fields )*
        };
    }
}
pub(super) use exhibit;
