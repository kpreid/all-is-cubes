//! Approximate image comparison and visual diffing.

use image::{buffer::ConvertBuffer, GenericImageView as _, GrayImage, Pixel as _, RgbaImage};

/// Output of [`diff()`], a comparison between two images.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DiffResult {
    /// A histogram of luminance difference values.
    ///
    /// For example, the zero element of this array contains a count of the pixels
    /// that were considered equal.
    pub histogram: [usize; 256],
    /// An image intended for human viewing of which pixels are different.
    pub diff_image: RgbaImage,
}

impl DiffResult {
    pub fn equal_or_different_below_threshold(&self, threshold: u8) -> bool {
        assert!(threshold < 255, "threshold = 255 will always be different");
        self.histogram[usize::from(threshold) + 1..]
            .iter()
            .sum::<usize>()
            == 0
    }
}

/// Compare two images and produces an image highlighting the differences,
/// with a neighborhood-sensitive comparison which counts one pixel worth of
/// displacement as not a difference.
///
/// This does not have any threshold for ignoring color differences; rather, the
/// result can be checked against one.
///
/// Panics if the images do not have equal sizes.
pub fn diff(expected: &RgbaImage, actual: &RgbaImage) -> DiffResult {
    assert_eq!(
        expected.dimensions(),
        actual.dimensions(),
        "Image sizes are not equal"
    );

    let hd1 = half_diff(expected, actual);
    let hd2 = half_diff(actual, expected);

    // Combine the two half_diff results: _both_ must be small for the output to be small.
    let combined_diff: GrayImage = GrayImage::from_fn(hd1.width(), hd1.height(), |x, y| {
        hd1.get_pixel(x, y)
            .map2(hd2.get_pixel(x, y), |ch1, ch2| ch1.max(ch2))
    });

    // Compute a histogram of difference sizes.
    let mut histogram: [usize; 256] = [0; 256];
    for diff_value in combined_diff.pixels() {
        let diff_value: u8 = diff_value[0];
        histogram[diff_value as usize] += 1;
    }
    //eprintln!("{:?}", histogram);

    DiffResult {
        histogram,
        diff_image: combined_diff.convert(),
    }
}

/// Compare each pixel of `have` against a neighborhood of `want` (ignoring the edge).
/// Each pixel's color must be approximately equal to some pixel in the neighborhood.
///
/// This is "half" of the complete diffing process because the neighborhood comparison
/// could allow a 1-pixel line in `want` to completely vanish. By performing the same
/// comparison in both directions, we ensure that each color in each image must also
/// appear in the other image.
fn half_diff(have: &RgbaImage, want: &RgbaImage) -> GrayImage {
    let (width, height) = have.dimensions();
    let have_elems = have.view(1, 1, width - 2, height - 2);

    let mut buffer: Vec<u8> = Vec::new();
    for (x, y, hpixel) in have_elems.pixels() {
        // The x and y we get from the iterator start at (0, 0) ignoring our offset,
        // so when we use those same x,y as top-left corner of the neighborhood,
        // we get a centered neighborhood.
        let neighborhood = want.view(x, y, 3, 3);
        let minimum_diff_in_neighborhood: u8 = neighborhood
            .pixels()
            .map(|(_, _, wpixel)| -> u8 {
                // Diff each channel independently, then convert the difference to luma.
                // Note: this is not theoretically correct in that sRGB nonlinearity
                // means we're under-counting the brightness difference, but `image`
                // is also doing it with linear arithmetic anyway:
                // <https://docs.rs/image/0.24.2/src/image/color.rs.html#473>
                let channel_diffs = hpixel.map2(&wpixel, |hch, wch| hch.abs_diff(wch));
                let color_diff = channel_diffs.to_luma()[0];
                let alpha_diff = channel_diffs[3];
                color_diff.max(alpha_diff)
            })
            .min()
            .expect("neighborhood is never empty");
        buffer.push(minimum_diff_in_neighborhood);
    }

    GrayImage::from_raw(have_elems.width(), have_elems.height(), buffer).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use image::{ImageBuffer, Pixel, Rgba};

    /// Run diff() against two images defined as vectors,
    /// with an added border.
    fn diff_vecs<P: Pixel>(
        (width, height): (u32, u32),
        expected_data: Vec<P::Subpixel>,
        actual_data: Vec<P::Subpixel>,
        border_value: P,
    ) -> DiffResult
    where
        ImageBuffer<P, Vec<P::Subpixel>>: ConvertBuffer<RgbaImage>,
    {
        let expected = add_border(
            border_value,
            ImageBuffer::from_raw(width, height, expected_data)
                .expect("wrong expected_data length"),
        );
        let actual = add_border(
            border_value,
            ImageBuffer::from_raw(width, height, actual_data).expect("wrong actual_data length"),
        );
        diff(&expected.convert(), &actual.convert())
    }

    fn add_border<P: Pixel>(
        border_value: P,
        image: ImageBuffer<P, Vec<P::Subpixel>>,
    ) -> ImageBuffer<P, Vec<P::Subpixel>> {
        let (width, height) = image.dimensions();
        ImageBuffer::from_fn(width + 2, height + 2, |x, y| {
            if (1..(width + 1)).contains(&x) && (1..(height + 1)).contains(&y) {
                *image.get_pixel(x - 1, y - 1)
            } else {
                border_value
            }
        })
    }

    #[test]
    fn simple_equality() {
        let image: RgbaImage =
            GrayImage::from_raw(3, 3, vec![0, 255, 128, 0, 255, 12, 255, 13, 99])
                .unwrap()
                .convert();
        let diff_result = dbg!(diff(&image, &image));

        let mut expected_histogram = [0; 256];
        expected_histogram[0] = 1;
        assert_eq!(diff_result.histogram, expected_histogram);

        assert!(diff_result.equal_or_different_below_threshold(0));
        assert!(diff_result.equal_or_different_below_threshold(5));
        assert!(diff_result.equal_or_different_below_threshold(254));
    }

    #[test]
    fn simple_inequality() {
        let delta = 55u8;
        let dred = 11; // delta scaled down by being on red channel only

        let expected_diff_image: RgbaImage =
            RgbaImage::from_raw(1, 1, vec![dred, dred, dred, 255]).unwrap();

        let mut expected_histogram = [0; 256];
        expected_histogram[usize::from(dred)] = 1;

        // Try both orders; result should be symmetric
        let diff_result = dbg!(diff_vecs(
            (1, 1),
            vec![0, 0, 0, 255],
            vec![delta, 0, 0, 255],
            Rgba([0, 0, 0, 255]),
        ));
        assert_eq!(
            diff_result,
            diff_vecs(
                (1, 1),
                vec![delta, 0, 0, 255],
                vec![0, 0, 0, 255],
                Rgba([0, 0, 0, 255])
            )
        );

        assert_eq!(
            diff_result,
            DiffResult {
                histogram: expected_histogram,
                diff_image: expected_diff_image
            }
        );

        assert_eq!(
            (
                diff_result.equal_or_different_below_threshold(dred - 1),
                diff_result.equal_or_different_below_threshold(dred)
            ),
            (false, true)
        );
    }

    #[test]
    fn alpha_only_difference() {
        let delta = 55u8;
        assert_eq!(
            diff_vecs(
                (1, 1),
                vec![100, 200, 30, 100],
                vec![100, 200, 30, 100 + delta],
                Rgba([0, 0, 0, 255]),
            ),
            DiffResult {
                histogram: {
                    let mut h = [0; 256];
                    h[usize::from(delta)] = 1;
                    h
                },
                diff_image: RgbaImage::from_raw(1, 1, vec![delta, delta, delta, 255]).unwrap()
            }
        );
    }
}
