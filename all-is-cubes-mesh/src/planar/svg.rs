use alloc::string::String;
use core::fmt::{self, Write as _};

use all_is_cubes::euclid::{Point2D, ScaleOffset2D};

use crate::planar;

// -------------------------------------------------------------------------------------------------

const XML_HEADER: &str = "<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.0//EN' 'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'>";

/// Renders the output of [`planar::Triangulator`] or [`planar::Outliner`] to SVG.
pub(crate) struct WriteSvg<'a, P> {
    /// Vertices that were input to the algorithm.
    ///
    /// For each vertex, if its `index` value is in range to be an ASCII letter,
    /// it will be displayed as such.
    pub vertices: &'a [planar::Vertex],

    /// Triangles produced by [`planar::Triangulator`],
    /// or loops produced by [`planar::Outliner`].
    ///
    /// In any case, must be a slice of indices into `vertices`, and must use
    /// counterclockwise winding for the outer boundary.
    pub loops: &'a [P],

    /// Scale factor from input coordinates to displayed `px` length units.
    ///
    /// Currently also affects the size of vertices and strokes.
    pub scale: f32,

    /// Whether to mark each vertex, in addition to drawing the triangles between them.
    pub show_vertices: bool,

    /// Should be `true` for writing `.svg` files and `false` for SVG-in-HTML.
    pub standalone_xml: bool,
}

impl<P> fmt::Display for WriteSvg<'_, P>
where
    P: AsRef<[u32]>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self {
            vertices,
            loops,
            scale,
            show_vertices,
            standalone_xml,
        } = self;

        if standalone_xml {
            write!(f, "{XML_HEADER}")?;
        }

        let bounding_box = all_is_cubes::euclid::Box2D::from_points(
            vertices.iter().map(|vertex| vertex.position.to_f32().xy()),
        )
        .inflate(1.0, 1.0);
        let document_size = bounding_box.size() * scale;

        // Transform we apply to the supplied vertices’ coordinates as we write them.
        let vertex_transform = ScaleOffset2D::<i32, _, ()>::scale(1, -1);

        // Note: \n\ discards the indentation. In general, this text when printed is
        // “lightly minified” and the indentation is just for source readability.
        writeln!(
            f,
            "<svg \
                xmlns='http://www.w3.org/2000/svg' \
                xmlns:xlink='http://www.w3.org/1999/xlink' \
                xmlns:h='http://www.w3.org/1999/xhtml' \
                width='{document_width}px' \
                height='{document_height}px' \
                viewBox='{view_box_min_x} {view_box_min_y} {view_box_width} {view_box_height}'\
            >\n\
                <style type='text/css'>\
                    #vertex, .triangle {{\
                    }}\
                    #vertex {{\
                        fill: yellow;\
                        stroke: #770;\
                        stroke-width: 0.03;\
                    }}\
                    .vertex text {{\
                        text-anchor: middle;\
                        dominant-baseline: central;\
                        font-size: 0.45px;\
                    }}\
                    .polygon {{\
                        fill: #AAF;\
                        stroke: #55F;\
                        stroke-width: 0.05;\
                        stroke-linejoin: round;\
                    }}\
                    svg:hover .vertex {{visibility:hidden;}}\
                </style>\n\
                <defs>\
                    <circle id='vertex' r='0.27'/>\
                </defs>",
            document_width = document_size.width,
            document_height = document_size.height,
            view_box_min_x = bounding_box.min.x,
            view_box_min_y = -bounding_box.max.y, // we flip Y-up to Y-down
            view_box_width = bounding_box.width(),
            view_box_height = bounding_box.height(),
        )?;

        writeln!(f, "<path class='polygon' d='")?;
        for loop_indices in loops {
            let mut previous_vertex: Option<Point2D<i32, _>> = None;
            for &index in loop_indices.as_ref() {
                // Assumption: There are few enough vertices that this linear search compares
                // favorably to building a hash table.
                let position @ Point2D { x, y, .. } = vertex_transform.transform_point(
                    vertices
                        .iter()
                        .find(|v| v.index == index)
                        .expect("missing vertex for triangle")
                        .position
                        .xy(),
                );

                // Since we have very many horizontal and vertical edges, it’s worth our time to
                // check for them and encode them as horizontal and vertical move instructions.
                // We could also do relative representation of the M instructions, but I think it’s
                // nice to make each triangle or loop readable alone.
                let delta = previous_vertex.map(|prev| (position - prev).to_array());
                match delta {
                    None => write!(f, "M{x} {y}")?,
                    Some([dx, 0]) => write!(f, "h{dx}")?,
                    Some([0, dy]) => write!(f, "v{dy}")?,
                    Some([dx, dy]) => write!(f, "l{dx} {dy}")?,
                }

                previous_vertex = Some(position);
            }
            writeln!(f, "Z")?;
        }
        writeln!(f, "'/>")?;

        if show_vertices {
            let mut label_in_buf = String::new();
            let mut label_out_buf = String::new();
            for vertex in vertices {
                {
                    label_in_buf.clear();
                    let index_as_char = vertex.index as u8 as char;
                    if index_as_char.is_ascii_alphabetic() {
                        write!(label_in_buf, "{index_as_char}")?;
                    } else {
                        write!(label_in_buf, "{}", vertex.index)?;
                    }
                }

                label_out_buf.clear();
                tinytemplate::escape(&label_in_buf, &mut label_out_buf);

                writeln!(
                    f,
                    "<g class='vertex' transform='translate({dx},{dy})'>\
                    <use xlink:href='#vertex'/><text>{label_out_buf}</text>\
                </g>",
                    dx = vertex.position.x,
                    dy = -vertex.position.y, // we flip Y-up to Y-down
                )?;
            }
        }

        write!(f, "</svg>")?;
        Ok(())
    }
}
