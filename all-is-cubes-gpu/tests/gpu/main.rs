//! Tests that require a GPU device, but are not rendering tests
//! (those are found in the `test-renderers` package).

use std::sync::Arc;

use all_is_cubes::math::{GridAab, GridSize, Rgb, Vol, ps64};
use all_is_cubes::space::{PackedLight, Space};

use all_is_cubes_gpu::{LightChunk, LightTexture, init};

// -------------------------------------------------------------------------------------------------

/// Obtain the common [`wgpu::Adapter`] used by `#[test]` tests in this module,
/// or exit the process if one is not available.
///
/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Adapter`] to create all of them.
pub(crate) async fn instance() -> &'static wgpu::Instance {
    static CELL: async_lock::OnceCell<wgpu::Instance> = async_lock::OnceCell::new();
    CELL.get_or_init(|| async { init::create_instance_for_test_or_exit(false).await })
        .await
}

// -------------------------------------------------------------------------------------------------

/// Test that the [`LightTexture`] performs partial updates correctly.
#[rstest::rstest]
fn light_texture_write_read(
    #[values(false, true)] use_scatter: bool,
    #[values(16, 19, 30, 50)] space_size_param: u32,
) {
    use all_is_cubes::block::AIR;

    async_io::block_on(async {
        let instance = instance().await;
        let adapter = init::create_adapter_for_test(instance).await;
        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor::default())
            .await
            .expect("failed to request_device");
        let device = Arc::new(device);

        let bounds = GridAab::from_lower_size([-10, 0, 0], GridSize::splat(space_size_param));
        // Create a space with well-defined (though nonsensical) light data.
        let space = Space::builder(bounds)
            .palette_and_contents(
                [AIR],
                Vol::repeat(bounds, 0),
                Some(Vol::from_fn(bounds, |cube| {
                    PackedLight::from_texel([cube.x as u8, cube.y as u8, cube.z as u8, 255])
                })),
            )
            .unwrap()
            .build();
        // Create a second space which is identical except that it has zero light.
        let dark_space = Space::builder(bounds).sky_color(Rgb::ZERO).build();

        let mut lt = LightTexture::new(
            "light_texture_write_test",
            &device,
            LightTexture::choose_size(&device.limits(), space.bounds(), ps64(1e6)),
            wgpu::TextureUsages::COPY_SRC,
        );

        if use_scatter {
            // First initialize with black from dark_space, then refresh it using update_scatter().
            lt.ensure_mapped(&queue, &dark_space.read(), space.bounds());

            lt.update_scatter(
                &device,
                &queue,
                &space.read(),
                LightChunk::all_in_region(space.bounds()).into_iter(),
            );
        } else {
            lt.ensure_mapped(&queue, &space.read(), space.bounds());
        }

        let texture_size = extent_to_size3d(lt.texture().size()).to_i32();
        let light_texels: Vec<[u8; 4]> =
            init::get_texels_from_gpu(&device, &queue, lt.texture(), 1).await;

        let mut wrong_texels = Vec::new();
        let mut count_both_zero = 0;
        for cube in space.bounds().interior_iter() {
            #[allow(clippy::cast_possible_wrap)]
            let zyx_index = cube.x.rem_euclid(texture_size.width)
                + texture_size.width
                    * (cube.y.rem_euclid(texture_size.height)
                        + texture_size.height * cube.z.rem_euclid(texture_size.depth));
            let expected = space.get_light(cube).as_texel();
            let actual = light_texels[zyx_index as usize];
            if expected != actual {
                wrong_texels.push((cube, expected, actual));
            } else if expected[0..3] == [0, 0, 0] {
                count_both_zero += 1;
            }
        }

        let volume = space.bounds().volume().unwrap();
        assert_eq!(
            wrong_texels,
            vec![],
            "out of {volume}, {len} were wrong and {nonzero} were not (correctly both zero)",
            len = wrong_texels.len(),
            nonzero = volume - count_both_zero,
        );
    });
}

// -------------------------------------------------------------------------------------------------

fn extent_to_size3d(size: wgpu::Extent3d) -> GridSize {
    GridSize::new(size.width, size.height, size.depth_or_array_layers)
}
