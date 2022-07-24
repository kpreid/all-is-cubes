use std::rc::Rc;
use std::sync::Arc;

use cgmath::{Vector3, Zero as _};

use crate::math::{point_to_enclosing_cube, Face6, GridAab, GridPoint, GridVector};
use crate::space::SpaceTransaction;
use crate::transaction::Merge as _;
use crate::vui::{InstallVuiError, Widget, WidgetBehavior};

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

/// Region a widget has been given by the layout algorithm, based on its
/// [`LayoutRequest`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
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
            gravity: Vector3::new(Align::Center, Align::Center, Align::Center),
        }
    }

    /// Shrink the bounds to the requested size, obeying the gravity
    /// parameter to choose where to position the result.
    #[must_use]
    pub fn shrink_to(self, sizes: GridVector) -> Self {
        let mut origin = GridPoint::new(0, 0, 0);
        for axis in 0..3 {
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
pub type Gravity = Vector3<Align>;

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

    /// Fill the available space with the children arranged along an axis.
    Stack {
        /// Which axis of space to arrange on.
        direction: Face6,
        children: Vec<Arc<LayoutTree<W>>>,
    },

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
            LayoutTree::Stack {
                direction: _,
                children,
            } => {
                for child in children {
                    child.for_each_leaf(function)
                }
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
    ) -> Result<Arc<LayoutTree<Positioned<W>>>, std::convert::Infallible> {
        Ok(Arc::new(match *self {
            LayoutTree::Leaf(ref w) => LayoutTree::Leaf(Positioned {
                // TODO: Implicitly Arc the leaf values? Or just drop this idea of the tree being
                // shared at all?
                value: W::clone(w),
                position: grant,
            }),
            LayoutTree::Spacer(ref r) => LayoutTree::Spacer(r.clone()),
            LayoutTree::Stack {
                direction,
                ref children,
            } => {
                let mut positioned_children = Vec::with_capacity(children.len());
                let mut bounds = grant.bounds;
                for child in children {
                    let requirements = child.requirements();
                    let axis = direction.axis_number();
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
            LayoutTree::Hud {
                ref crosshair,
                ref toolbar,
                ref control_bar,
            } => {
                let mut crosshair_pos =
                    point_to_enclosing_cube(grant.bounds.center()).unwrap(/* TODO: not unwrap */);
                crosshair_pos.z = 0;
                let crosshair_bounds = GridAab::single_cube(crosshair_pos);
                // TODO: bounds of toolbar and control_bar should be just small enough to miss the crosshair. Also figure out exactly what their Z range should be
                LayoutTree::Hud {
                    crosshair: crosshair.perform_layout(LayoutGrant {
                        bounds: crosshair_bounds,
                        gravity: Vector3::new(Align::Center, Align::Center, Align::Center),
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
                        gravity: Vector3::new(Align::Center, Align::Low, Align::Center),
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
                        gravity: Vector3::new(Align::High, Align::High, Align::Low),
                    })?,
                }
            }
        }))
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
            LayoutTree::Stack {
                direction,
                ref children,
            } => {
                let mut accumulator = GridVector::zero();
                let stack_axis = direction.axis_number();
                for child in children {
                    let child_req = child.requirements();
                    for axis in 0..3 {
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
    use crate::math::Face6;

    use super::*;
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
}
