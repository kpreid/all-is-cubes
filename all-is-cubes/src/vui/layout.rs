// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::rc::Rc;
use std::sync::Arc;

use cgmath::Zero as _;

use crate::math::GridVector;
use crate::raycast::Face;
use crate::space::Grid;

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
#[non_exhaustive]
pub enum LayoutTree<W> {
    /// A single widget.
    Leaf(W),
    /// Fill the available space with the children, in order in the given direction.
    Stack {
        // TODO: We should have a 6-valued Face type so that this cannot fail
        direction: Face,
        children: Vec<Arc<LayoutTree<W>>>,
    },
}

/// Result of [`LayoutTree::perform_layout`]: specifies where items were positioned, in
/// absolute coordinates (independent of the tree).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct Positioned<W> {
    pub value: W,
    pub position: Grid,
}

impl<W> LayoutTree<W> {
    pub fn leaf(widget_value: W) -> Arc<Self> {
        Arc::new(Self::Leaf(widget_value))
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
            LayoutTree::Stack {
                direction: _,
                children,
            } => {
                for child in children {
                    child.for_each_leaf(function)
                }
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
        mut bounds: Grid,
    ) -> Result<Arc<LayoutTree<Positioned<W>>>, std::convert::Infallible> {
        Ok(Arc::new(match *self {
            LayoutTree::Leaf(ref w) => LayoutTree::Leaf(Positioned {
                // TODO: Implicitly Arc the leaf values? Or just drop this idea of the tree being
                // shared at all?
                value: W::clone(w),
                position: bounds,
            }),
            LayoutTree::Stack {
                direction,
                ref children,
            } => {
                let mut positioned_children = Vec::with_capacity(children.len());
                for child in children {
                    let requirements = child.requirements();
                    let axis = direction.axis_number().unwrap();
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

                    positioned_children.push(child.perform_layout(child_bounds)?);
                    bounds = remainder_bounds;
                }
                LayoutTree::Stack {
                    direction,
                    children: positioned_children,
                }
            }
        }))
    }
}

impl<W: Layoutable> Layoutable for LayoutTree<W> {
    fn requirements(&self) -> LayoutRequest {
        match *self {
            LayoutTree::Leaf(ref w) => w.requirements(),
            LayoutTree::Stack {
                direction,
                ref children,
            } => {
                let mut accumulator = GridVector::zero();
                let stack_axis = direction.axis_number().unwrap();
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
        }
    }
}

#[cfg(test)]
mod tests {
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
            direction: Face::PX,
            children: vec![
                LayoutTree::leaf(LT::new("a", [1, 1, 1])),
                LayoutTree::leaf(LT::new("b", [1, 1, 1])),
                LayoutTree::leaf(LT::new("c", [1, 1, 1])),
            ],
        };
        assert_eq!(
            tree.perform_layout(Grid::new([10, 10, 10], [10, 10, 10]))
                .unwrap()
                .leaves()
                .collect::<Vec<_>>(),
            vec![
                &Positioned {
                    value: LT::new("a", [1, 1, 1]),
                    position: Grid::new([10, 10, 10], [1, 10, 10]),
                },
                &Positioned {
                    value: LT::new("b", [1, 1, 1]),
                    position: Grid::new([11, 10, 10], [1, 10, 10]),
                },
                &Positioned {
                    value: LT::new("c", [1, 1, 1]),
                    position: Grid::new([12, 10, 10], [1, 10, 10]),
                }
            ]
        );
    }
}
