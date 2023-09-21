use alloc::rc::Rc;
use alloc::sync::Arc;

use all_is_cubes::euclid;
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::math::{
    Axis, Cube, Face6, FaceMap, GridAab, GridCoordinate, GridPoint, GridVector, VectorOps,
};
use all_is_cubes::space::{Space, SpaceBuilder, SpaceTransaction};
use all_is_cubes::transaction::{self, Merge as _, Transaction as _};

use crate::vui::{InstallVuiError, Widget, WidgetBehavior};

// TODO: can we come up with a way to not even need this type alias?
// The Arcs are clunky to use.
pub type WidgetTree = Arc<LayoutTree<Arc<dyn Widget>>>;

/// Lay out a widget tree and produce the transaction to install it.
///
/// This is a combination of:
///
/// * [`LayoutTree::perform_layout()`] to choose locations
/// * [`LayoutTree::installation()`] to convert the tree to a transaction
///
/// with error propagation from all operations and constraint of the input type.
///
/// See also [`LayoutTree::to_space()`] if you do not need to install widgets in an
/// already-existing [`Space`].
///
/// TODO: This function needs a better name and location. Also, it would be nice if it could
/// help with handling the potential error resulting from executing the transaction.
pub fn install_widgets(
    grant: LayoutGrant,
    tree: &WidgetTree,
) -> Result<SpaceTransaction, InstallVuiError> {
    tree.perform_layout(grant).unwrap(/* currently infallible */).installation()
}

/// Requested size and relative positioning of a widget or other thing occupying space,
/// to be interpreted by a layout algorithm to choose the real position.
///
/// TODO: give this type and [`Layoutable`] better names
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct LayoutRequest {
    /// The minimum dimensions required, without which correct functionality
    /// is not possible.
    pub minimum: GridVector,
}

impl LayoutRequest {
    /// A request for no space at all.
    pub const EMPTY: Self = Self {
        minimum: GridVector::new(0, 0, 0),
    };
}

/// Region a widget has been given by the layout algorithm, based on its
/// [`LayoutRequest`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)] // TODO: constructor or something
pub struct LayoutGrant {
    /// The widget may have exclusive access to this volume.
    pub bounds: GridAab,

    /// Preferred alignment for non-stretchy widgets.
    pub gravity: Gravity,
}

impl LayoutGrant {
    /// Construct a `LayoutGrant` from scratch, such as to begin layout.
    pub fn new(bounds: GridAab) -> Self {
        LayoutGrant {
            bounds,
            gravity: Vector3D::new(Align::Center, Align::Center, Align::Center),
        }
    }

    /// Shrink the bounds to the requested size, obeying the gravity
    /// parameter to choose where to position the result.
    ///
    /// If the given size is larger than this grant on any axis then that axis will be
    /// unchanged.
    ///
    /// `enlarge_for_symmetry` controls the behavior when `sizes` has different parity on
    /// any axis than `self.bounds` (one size is odd and the other is even), and
    /// `self.gravity` requests centering; `true` requests that the returned size should
    /// be one greater on that axis, and `false` that the position should be rounded down
    /// (asymmetric placement).
    #[must_use]
    pub fn shrink_to(self, mut sizes: GridVector, enlarge_for_symmetry: bool) -> Self {
        assert!(
            sizes.x >= 0 && sizes.y >= 0 && sizes.z >= 0,
            "sizes to shrink to must be positive, not {sizes:?}"
        );

        if enlarge_for_symmetry {
            for axis in Axis::ALL {
                if self.gravity[axis] == Align::Center
                    && self.bounds.size()[axis].rem_euclid(2) != sizes[axis].rem_euclid(2)
                {
                    sizes[axis] += 1;
                }
            }
        }

        // Ensure we don't enlarge the size of self by clamping the proposed size
        let sizes = sizes.zip(self.bounds.size(), GridCoordinate::min);

        let mut origin = GridPoint::new(0, 0, 0);
        for axis in Axis::ALL {
            let l = self.bounds.lower_bounds()[axis];
            let h = self.bounds.upper_bounds()[axis] - sizes[axis];
            origin[axis] = match self.gravity[axis] {
                Align::Low => l,
                Align::Center => l + (h - l) / 2,
                Align::High => h,
            };
        }
        LayoutGrant {
            bounds: GridAab::from_lower_size(origin, sizes),
            gravity: self.gravity,
        }
    }

    /// As `shrink_to()` but returning a single cube, as long as the grant is nonempty.
    ///
    /// This is a common pattern but I'm not sure it should be, so this isn't public.
    pub(crate) fn shrink_to_cube(&self) -> Option<Cube> {
        self.shrink_to(GridVector::new(1, 1, 1), false)
            .bounds
            .interior_iter()
            .next()
    }
}

/// Where to position things, on a given axis, when available space exceeds required space.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Align {
    /// All the way in the direction of the lower corner (left, down, back).
    Low,
    /// Centered, or as close as possible.
    Center,
    /// All the way in the direction of the upper corner (right, up, front).
    High,
}

/// Specifies which corner of available space a widget should prefer to position
/// itself towards if it is not intending to fill that space.
///
/// TODO: Use a better enum
pub type Gravity = euclid::default::Vector3D<Align>;

/// Something which can occupy space in a [`LayoutTree`], or is one.
///
/// TODO: give this trait and [`LayoutRequest`] better names
pub trait Layoutable {
    fn requirements(&self) -> LayoutRequest;
}

impl<T: ?Sized + Layoutable> Layoutable for &'_ T {
    fn requirements(&self) -> LayoutRequest {
        (**self).requirements()
    }
}
impl<T: ?Sized + Layoutable> Layoutable for Box<T> {
    fn requirements(&self) -> LayoutRequest {
        (**self).requirements()
    }
}
impl<T: ?Sized + Layoutable> Layoutable for Rc<T> {
    fn requirements(&self) -> LayoutRequest {
        (**self).requirements()
    }
}
impl<T: ?Sized + Layoutable> Layoutable for Arc<T> {
    fn requirements(&self) -> LayoutRequest {
        (**self).requirements()
    }
}

/// A user interface laid out in 3-dimensional space.
///
/// Leaf nodes contain values of type `W` which describe individual 'widgets' (values
/// that implement [`Layoutable`]); the tree structure itself describes how they are
/// arranged relative to each other. In this system, widgets do not contain other widgets
/// (at least, not for the purposes of the layout algorithm).
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum LayoutTree<W> {
    /// A single widget.
    Leaf(W),

    /// An space laid out like a widget but left empty.
    Spacer(LayoutRequest),

    /// Add the specified amount of space around the child.
    Margin {
        margin: FaceMap<GridCoordinate>,
        child: Arc<LayoutTree<W>>,
    },

    /// Fill the available space with the children arranged along an axis.
    Stack {
        /// Which axis of space to arrange on.
        direction: Face6,
        children: Vec<Arc<LayoutTree<W>>>,
    },

    /// Don't lay out the contents bigger than minimum.
    Shrink(Arc<LayoutTree<W>>),

    /// A custom layout dedicated to the HUD.
    /// TODO: Find a better abstraction than a variant of `LayoutTree` for this.
    Hud {
        crosshair: Arc<LayoutTree<W>>,
        toolbar: Arc<LayoutTree<W>>,
        control_bar: Arc<LayoutTree<W>>,
    },
}

/// Result of [`LayoutTree::perform_layout`]: specifies where items were positioned, in
/// absolute coordinates (independent of the tree).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct Positioned<W> {
    pub value: W,
    pub position: LayoutGrant,
}

impl<W> LayoutTree<W> {
    pub fn empty() -> Arc<Self> {
        Arc::new(Self::Spacer(LayoutRequest::EMPTY))
    }

    pub fn leaf(widget_value: W) -> Arc<Self> {
        Arc::new(Self::Leaf(widget_value))
    }

    pub fn spacer(requirements: LayoutRequest) -> Arc<Self> {
        Arc::new(Self::Spacer(requirements))
    }

    pub fn leaves<'s>(&'s self) -> impl Iterator<Item = &'s W> + Clone {
        let mut leaves: Vec<&'s W> = Vec::new();
        self.for_each_leaf(&mut |leaf| leaves.push(leaf));
        leaves.into_iter()
    }

    fn for_each_leaf<'s, F>(&'s self, function: &mut F)
    where
        F: FnMut(&'s W),
    {
        match self {
            LayoutTree::Leaf(value) => function(value),
            LayoutTree::Spacer(_) => {}
            LayoutTree::Margin { margin: _, child } => child.for_each_leaf(function),
            LayoutTree::Stack {
                direction: _,
                children,
            } => {
                for child in children {
                    child.for_each_leaf(function)
                }
            }
            LayoutTree::Shrink(child) => {
                child.for_each_leaf(function);
            }
            LayoutTree::Hud {
                crosshair,
                toolbar,
                control_bar,
            } => {
                crosshair.for_each_leaf(function);
                toolbar.for_each_leaf(function);
                control_bar.for_each_leaf(function);
            }
        }
    }
}

impl<W: Layoutable + Clone> LayoutTree<W> {
    /// Given the specified outermost bounds, perform layout and return a tree
    /// whose leaves are all [`Positioned`].
    ///
    /// TODO: haven't decided whether layout can fail yet, hence the placeholder non-error
    pub fn perform_layout(
        &self,
        grant: LayoutGrant,
    ) -> Result<Arc<LayoutTree<Positioned<W>>>, core::convert::Infallible> {
        Ok(Arc::new(match *self {
            LayoutTree::Leaf(ref w) => LayoutTree::Leaf(Positioned {
                // TODO: Implicitly Arc the leaf values? Or just drop this idea of the tree being
                // shared at all?
                value: W::clone(w),
                position: grant,
            }),
            LayoutTree::Spacer(ref r) => LayoutTree::Spacer(r.clone()),
            LayoutTree::Margin { margin, ref child } => LayoutTree::Margin {
                margin,
                child: child.perform_layout(LayoutGrant {
                    bounds: grant.bounds.expand(margin.map(|_, m| -m)),
                    gravity: grant.gravity,
                })?,
            },
            LayoutTree::Stack {
                direction,
                ref children,
            } => {
                let mut positioned_children = Vec::with_capacity(children.len());
                let mut bounds = grant.bounds;
                for child in children {
                    let requirements = child.requirements();
                    let axis = direction.axis();
                    let size_on_axis = requirements.minimum[axis];
                    let available_size = bounds.size()[axis];
                    if size_on_axis > available_size {
                        // TODO: emit detectable warning
                        break;
                    }

                    // TODO: remainder computation is inelegant - we want .expand() but single axis
                    let child_bounds = bounds.abut(direction.opposite(), -size_on_axis)
                        .unwrap(/* always smaller, can't overflow */);
                    let remainder_bounds = bounds.abut(direction, -(available_size - size_on_axis))
                        .unwrap(/* always smaller, can't overflow */);

                    positioned_children.push(child.perform_layout(LayoutGrant {
                        bounds: child_bounds,
                        gravity: grant.gravity,
                    })?);
                    bounds = remainder_bounds;
                }
                LayoutTree::Stack {
                    direction,
                    children: positioned_children,
                }
            }
            LayoutTree::Shrink(ref child) => {
                let grant = grant.shrink_to(child.requirements().minimum, true);
                LayoutTree::Shrink(child.perform_layout(grant)?)
            }
            LayoutTree::Hud {
                ref crosshair,
                ref toolbar,
                ref control_bar,
            } => {
                let mut crosshair_pos =
                    Cube::containing(grant.bounds.center()).unwrap(/* TODO: not unwrap */);
                crosshair_pos.z = 0;
                let crosshair_bounds = crosshair_pos.grid_aab();
                // TODO: bounds of toolbar and control_bar should be just small enough to miss the crosshair. Also figure out exactly what their Z range should be
                LayoutTree::Hud {
                    crosshair: crosshair.perform_layout(LayoutGrant {
                        bounds: crosshair_bounds,
                        gravity: Vector3D::new(Align::Center, Align::Center, Align::Center),
                    })?,
                    toolbar: toolbar.perform_layout(LayoutGrant {
                        bounds: GridAab::from_lower_upper(
                            [
                                grant.bounds.lower_bounds().x,
                                grant.bounds.lower_bounds().y,
                                0,
                            ],
                            [
                                grant.bounds.upper_bounds().x,
                                crosshair_bounds.lower_bounds().y,
                                grant.bounds.upper_bounds().z,
                            ],
                        ),
                        gravity: Vector3D::new(Align::Center, Align::Low, Align::Center),
                    })?,
                    control_bar: control_bar.perform_layout(LayoutGrant {
                        bounds: GridAab::from_lower_upper(
                            [
                                grant.bounds.lower_bounds().x,
                                crosshair_bounds.upper_bounds().y,
                                -1,
                            ],
                            grant.bounds.upper_bounds(),
                        ),
                        gravity: Vector3D::new(Align::High, Align::High, Align::Low),
                    })?,
                }
            }
        }))
    }
}

impl LayoutTree<Arc<dyn Widget>> {
    pub fn to_space<B: all_is_cubes::space::SpaceBuilderBounds>(
        self: Arc<Self>,
        builder: SpaceBuilder<B>,
        gravity: Gravity,
    ) -> Result<Space, InstallVuiError> {
        let mut space = builder
            .bounds_if_not_set(|| GridAab::from_lower_size([0, 0, 0], self.requirements().minimum))
            .build();

        install_widgets(
            LayoutGrant {
                bounds: space.bounds(),
                gravity,
            },
            &self,
        )?
        .execute(&mut space, &mut transaction::no_outputs)
        .map_err(|error| InstallVuiError::ExecuteInstallation { error })?;

        Ok(space)
    }
}

impl LayoutTree<Positioned<Arc<dyn Widget>>> {
    /// Creates a transaction which will install all of the widgets in this tree.
    ///
    /// Returns an error if the widgets conflict with each other.
    pub fn installation(&self) -> Result<SpaceTransaction, InstallVuiError> {
        let mut txn = SpaceTransaction::default();
        for positioned_widget @ Positioned { value, position } in self.leaves() {
            let widget = value.clone();
            let controller_installation = WidgetBehavior::installation(
                positioned_widget.clone(),
                widget.controller(position),
            )?;
            validate_widget_transaction(value, &controller_installation, position)?;
            txn = txn
                .merge(controller_installation)
                .map_err(|error| InstallVuiError::Conflict { error })?;
        }
        Ok(txn)
    }
}

impl<W: Layoutable> Layoutable for LayoutTree<W> {
    fn requirements(&self) -> LayoutRequest {
        match *self {
            LayoutTree::Leaf(ref w) => w.requirements(),
            LayoutTree::Spacer(ref requirements) => requirements.clone(),
            LayoutTree::Margin { margin, ref child } => {
                let mut req = child.requirements();
                req.minimum = req.minimum + margin.negatives() + margin.positives();
                req
            }
            LayoutTree::Stack {
                direction,
                ref children,
            } => {
                let mut accumulator = GridVector::zero();
                let stack_axis = direction.axis();
                for child in children {
                    let child_req = child.requirements();
                    for axis in Axis::ALL {
                        if axis == stack_axis {
                            accumulator[axis] += child_req.minimum[axis];
                        } else {
                            accumulator[axis] = accumulator[axis].max(child_req.minimum[axis]);
                        }
                    }
                }
                LayoutRequest {
                    minimum: accumulator,
                }
            }
            LayoutTree::Shrink(ref child) => child.requirements(),
            LayoutTree::Hud {
                ref crosshair,
                ref toolbar,
                ref control_bar,
            } => {
                // Minimum space is the same as a stack, for now
                LayoutTree::Stack {
                    direction: Face6::PY,
                    children: vec![crosshair.clone(), toolbar.clone(), control_bar.clone()],
                }
                .requirements()
            }
        }
    }
}

pub(super) fn validate_widget_transaction(
    widget: &Arc<dyn Widget>,
    transaction: &SpaceTransaction,
    grant: &LayoutGrant,
) -> Result<(), InstallVuiError> {
    match transaction.bounds() {
        None => Ok(()),
        Some(txn_bounds) => {
            if grant.bounds.contains_box(txn_bounds) {
                Ok(())
            } else {
                // TODO: This being InstallVuiError isn't great if we might want to validate
                // transactions happening after installation.
                Err(InstallVuiError::OutOfBounds {
                    widget: widget.clone(),
                    grant: *grant,
                    erroneous: txn_bounds,
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::Face6;
    use pretty_assertions::assert_eq;

    /// Trivial implementation of [`Layoutable`].
    #[derive(Clone, Debug, PartialEq)]
    struct LT {
        label: &'static str,
        requirements: LayoutRequest,
    }

    impl LT {
        fn new(label: &'static str, minimum_size: impl Into<GridVector>) -> Self {
            Self {
                label,
                requirements: LayoutRequest {
                    minimum: minimum_size.into(),
                },
            }
        }
    }

    impl Layoutable for LT {
        fn requirements(&self) -> LayoutRequest {
            self.requirements.clone()
        }
    }

    #[test]
    fn simple_stack_with_extra_room() {
        let tree = LayoutTree::Stack {
            direction: Face6::PX,
            children: vec![
                LayoutTree::leaf(LT::new("a", [1, 1, 1])),
                LayoutTree::leaf(LT::new("b", [1, 1, 1])),
                LayoutTree::leaf(LT::new("c", [1, 1, 1])),
            ],
        };
        let grant = LayoutGrant::new(GridAab::from_lower_size([10, 10, 10], [10, 10, 10]));
        assert_eq!(
            tree.perform_layout(grant)
                .unwrap()
                .leaves()
                .collect::<Vec<_>>(),
            vec![
                &Positioned {
                    value: LT::new("a", [1, 1, 1]),
                    position: LayoutGrant {
                        bounds: GridAab::from_lower_size([10, 10, 10], [1, 10, 10]),
                        gravity: grant.gravity,
                    },
                },
                &Positioned {
                    value: LT::new("b", [1, 1, 1]),
                    position: LayoutGrant {
                        bounds: GridAab::from_lower_size([11, 10, 10], [1, 10, 10]),
                        gravity: grant.gravity,
                    },
                },
                &Positioned {
                    value: LT::new("c", [1, 1, 1]),
                    position: LayoutGrant {
                        bounds: GridAab::from_lower_size([12, 10, 10], [1, 10, 10]),
                        gravity: grant.gravity,
                    },
                }
            ]
        );
    }

    #[test]
    fn spacer() {
        let tree = LayoutTree::Stack {
            direction: Face6::PX,
            children: vec![
                LayoutTree::leaf(LT::new("a", [1, 1, 1])),
                LayoutTree::spacer(LayoutRequest {
                    minimum: GridVector::new(3, 1, 1),
                }),
                LayoutTree::leaf(LT::new("b", [1, 1, 1])),
            ],
        };
        let grant = LayoutGrant::new(GridAab::from_lower_size([10, 10, 10], [10, 10, 10]));
        assert_eq!(
            tree.perform_layout(grant)
                .unwrap()
                .leaves()
                .collect::<Vec<_>>(),
            vec![
                &Positioned {
                    value: LT::new("a", [1, 1, 1]),
                    position: LayoutGrant {
                        bounds: GridAab::from_lower_size([10, 10, 10], [1, 10, 10]),
                        gravity: grant.gravity,
                    },
                },
                &Positioned {
                    value: LT::new("b", [1, 1, 1]),
                    position: LayoutGrant {
                        bounds: GridAab::from_lower_size([14, 10, 10], [1, 10, 10]),
                        gravity: grant.gravity,
                    },
                }
            ]
        );
    }

    #[test]
    fn shrink_to_bigger_than_grant_does_not_enlarge_grant() {
        // X axis is too small already
        // Y axis is exactly right
        // Z axis is too large and thus subject to centering
        // (because LayoutGrant::new sets gravity to center)
        assert_eq!(
            LayoutGrant::new(GridAab::from_lower_size([0, 0, 0], [5, 10, 20]))
                .shrink_to(GridVector::new(10, 10, 10), false),
            LayoutGrant::new(GridAab::from_lower_size([0, 0, 5], [5, 10, 10]))
        );
    }

    /// `shrink_to()` called with centering and `enlarge_for_symmetry` false.
    #[test]
    fn shrink_to_rounding_without_enlarging() {
        let grant = LayoutGrant {
            bounds: GridAab::from_lower_size([10, 10, 10], [10, 10, 11]),
            gravity: Vector3D::new(Align::Center, Align::Center, Align::Center),
        };
        assert_eq!(
            grant.shrink_to(GridVector::new(1, 2, 2), false),
            LayoutGrant {
                bounds: GridAab::from_lower_size([14, 14, 14], [1, 2, 2]), // TODO: oughta be rounding down
                gravity: grant.gravity,
            }
        );
    }

    /// `shrink_to()` called with centering and `enlarge_for_symmetry` true.
    #[test]
    fn shrink_to_with_enlarging() {
        // In each case where the parity does not match between the minimum and the
        // grant, the size of the post-layout grant should be enlarged to match the
        // original grant's parity.
        // X axis is an odd size in even grant
        // Y axis is an even size in an even grant (should stay the same size)
        // Z axis is an even size in an odd grant
        let grant = LayoutGrant {
            bounds: GridAab::from_lower_size([10, 10, 10], [10, 10, 9]),
            gravity: Vector3D::new(Align::Center, Align::Center, Align::Center),
        };
        assert_eq!(
            grant.shrink_to(GridVector::new(1, 2, 2), true),
            LayoutGrant {
                bounds: GridAab::from_lower_size([14, 14, 13], [2, 2, 3]),
                gravity: grant.gravity,
            }
        );
    }
}
