//! Creation of, and systems in, the [`App`][b::App] that backs the glTF renderer.

use std::collections::HashMap;
use std::sync::Arc;

use bevy::camera::RenderTarget;
use bevy::image::BevyDefault as _;
use bevy::post_process::bloom;
use bevy::prelude as b;
use bevy::prelude::StandardMaterial;
use bevy::render::render_resource as wgpu;
use bevy::render::view::window::screenshot::ScreenshotCaptured;
use half::f16;
use tokio::sync::mpsc;

use all_is_cubes::euclid::vec3 as evec3;
use all_is_cubes::math::{Cube, Face, GridAab, Rgba, Vol};
use all_is_cubes::space::PackedLight;
use all_is_cubes_content::palette;
use all_is_cubes_render::camera::{Camera, FogOption, Layers, LightingOption, Viewport};
use all_is_cubes_render::{Flaws, Rendering};

use crate::message::{RenderMsg, Transfer};

// -------------------------------------------------------------------------------------------------

/// Runs an [`App`][b::App] on a dedicated thread and reacts to [`RenderMsg`]es.
//
// TODO: viewport should be synced
// TODO: consider if we can usefully turn this explicit message loop into a plain `b::App::run()`
pub(crate) async fn bevy_app_actor(
    label: String,
    mut rx: mpsc::UnboundedReceiver<RenderMsg>,
    viewport: Viewport,
) {
    let mut app = b::App::new();
    init_plugins(&mut app);
    let render_target = init_entities_and_resources(&mut app, viewport);

    let mut flaws = Flaws::default();

    // TODO: consider inverting this and running it inside the App so we can use normal App::run()
    while let Some(msg) = rx.recv().await {
        // TODO: Instead of these message prefixes, hook into the TEST_ID mechanism the logger
        // already uses.
        log::trace!("actor({label}) receiving {msg:?}");
        match msg {
            RenderMsg::Update {
                export_task,
                transfer,
            } => {
                flaws = transfer.flaws;
                let cameras = transfer.cameras.clone();

                if transfer.cursor.is_some() {
                    // TODO: render cursor instead
                    flaws |= Flaws::NO_CURSOR;
                }
                if transfer.cameras.world.options().bloom_intensity > 0.0 {
                    // TODO: bloom should work but isn't (but that might be just lack of light emission)
                    flaws |= Flaws::NO_BLOOM;
                }

                // Wait for the glTF file to actually exist.
                export_task.await.unwrap();

                let gltf_asset_handle = app
                    .world_mut()
                    .run_system_cached_with(load_transfer_system, transfer)
                    .expect("load_gltf_system failed");

                log::trace!("actor({label}) waiting for asset load");
                loop {
                    let state = app
                        .world()
                        .resource::<b::AssetServer>()
                        .recursive_dependency_load_state(&gltf_asset_handle);
                    log::trace!("actor({label}) asset state = {state:?}");
                    match state {
                        bevy::asset::RecursiveDependencyLoadState::NotLoaded => {}
                        bevy::asset::RecursiveDependencyLoadState::Loading => {}
                        bevy::asset::RecursiveDependencyLoadState::Loaded => break,
                        bevy::asset::RecursiveDependencyLoadState::Failed(asset_load_error) => {
                            todo!("propagate error {asset_load_error:?}")
                        }
                    }
                    app.update();
                    tokio::task::yield_now().await;
                }
                log::trace!("actor({label}) completed asset load");

                app.world_mut().run_system_cached_with(patch_gltf_materials, cameras).unwrap();
            }
            RenderMsg::Render {
                info_text,
                completed,
            } => {
                let _ = info_text; // TODO: add a UI entity

                let mut completed = Some(completed); // observer must not be FnOnce

                let screenshot_entity = app
                    .world_mut()
                    .spawn(bevy::render::view::window::screenshot::Screenshot::image(
                        render_target.as_image().unwrap().clone(),
                    ))
                    .observe({
                        let label = label.clone();
                        move |trigger: b::On<ScreenshotCaptured>| {
                            let captured_image: &ScreenshotCaptured = &trigger.event();
                            let rendering = Rendering {
                                size: <[u32; 2]>::from(captured_image.size()).into(),
                                data: bytemuck::cast_vec(
                                    captured_image.data.clone().expect("image bytes missing"),
                                ),
                                info: Arc::new(crate::RenderInfo {}),
                                flaws,
                            };

                            let _error_ignored_because_it_is_just_cancellation = completed
                                .take()
                                .expect("extra screenshot event")
                                .send(Ok(rendering));
                            log::trace!("actor({label}) captured screenshot");
                        }
                    })
                    .id();

                // Run the world until the screenshot entity completes and despawns itself
                log::trace!("actor({label}) waiting for screenshot");
                while app.world().get_entity(screenshot_entity).is_ok() {
                    app.update();
                    tokio::task::yield_now().await;
                }
                log::trace!("actor({label}) stopped waiting for screenshot");
            }
        }
    }
}

fn init_plugins(app: &mut b::App) {
    app.add_plugins((
        b::MinimalPlugins,
        //
        b::WindowPlugin {
            primary_window: None,
            exit_condition: bevy::window::ExitCondition::DontExit,
            ..Default::default()
        },
        //
        b::AssetPlugin {
            // allow loading gltf from temporary directory
            unapproved_path_mode: bevy::asset::UnapprovedPathMode::Allow,
            ..Default::default()
        },
        //
        bevy::transform::TransformPlugin::default(),
        bevy::scene::ScenePlugin::default(),
        bevy::render::RenderPlugin::default(),
        bevy::camera::CameraPlugin::default(),
        bevy::mesh::MeshPlugin::default(),
        bevy::gltf::GltfPlugin::default(),
        //
        b::ImagePlugin::default(),
        bevy::render::pipelined_rendering::PipelinedRenderingPlugin::default(),
        bevy::core_pipeline::CorePipelinePlugin::default(),
        bevy::pbr::PbrPlugin::default(),
        bevy::light::LightPlugin::default(),
    ));

    app.finish();
    app.cleanup();
}

fn init_entities_and_resources(app: &mut bevy::app::App, viewport: Viewport) -> RenderTarget {
    let render_target = app
        .world_mut()
        .run_system_cached_with(create_render_target, viewport)
        .expect("create_render_target failed");

    // TODO: instead of spawning our own camera we should pass the camera through glTF,
    // at least partially
    app.world_mut().spawn((
        b::Camera3d::default(),
        b::Camera::default(),
        render_target.clone(),
        bevy::core_pipeline::tonemapping::Tonemapping::None, // TODO: link up tonemapping
        bloom::Bloom {
            intensity: 0.0,
            low_frequency_boost: 0.0,
            low_frequency_boost_curvature: 0.0,
            high_pass_frequency: 1.0,
            composite_mode: bloom::BloomCompositeMode::EnergyConserving,
            prefilter: bloom::BloomPrefilter {
                threshold: 0.0,
                threshold_softness: 0.0,
            },
            ..Default::default()
        },
        bevy::pbr::DistanceFog::default(),
        // This transform should get overwritten.
        b::Transform::from_xyz(5., 5., 5.).looking_at(b::Vec3::new(0.0, 0.3, 0.0), b::Vec3::Y),
    ));

    app.world_mut()
        .insert_resource(b::ClearColor(convert_color(palette::NO_WORLD_TO_SHOW)));

    render_target
}

fn create_render_target(
    b::In(viewport): b::In<Viewport>,
    //commands: b::Commands,
    mut images: b::ResMut<b::Assets<b::Image>>,
    //render_device: b::Res<bevy::render::renderer::RenderDevice>,
) -> RenderTarget {
    // TODO: hook up viewport updates
    let size = wgpu::Extent3d {
        width: viewport.framebuffer_size.width.max(1),
        height: viewport.framebuffer_size.height.max(1),
        ..Default::default()
    };

    // This is the texture that will be rendered to.
    let mut render_target_image = b::Image::new_fill(
        size,
        wgpu::TextureDimension::D2,
        &[0; 4],
        wgpu::TextureFormat::bevy_default(),
        bevy::asset::RenderAssetUsages::default(),
    );
    render_target_image.texture_descriptor.usage |= wgpu::TextureUsages::COPY_SRC
        | wgpu::TextureUsages::RENDER_ATTACHMENT
        | wgpu::TextureUsages::TEXTURE_BINDING;
    let render_target_image_handle = images.add(render_target_image);

    RenderTarget::Image(render_target_image_handle.into())
}

#[derive(b::Component)]
struct SceneTag;

fn load_transfer_system(
    b::In(transfer): b::In<Transfer>,
    mut commands: b::Commands,
    asset_server: b::Res<b::AssetServer>,
    mut images: b::ResMut<b::Assets<b::Image>>,
    old_scene_root: b::Query<b::Entity, b::With<SceneTag>>,
    mut clear_color: b::ResMut<b::ClearColor>,
    mut ambient_light: b::ResMut<b::GlobalAmbientLight>,
    mut bevy_camera: b::Single<
        (
            &mut b::Projection,
            &mut b::Transform,
            &mut b::Msaa,
            &mut bloom::Bloom,
            &mut bevy::pbr::DistanceFog,
        ),
        b::With<b::Camera3d>,
    >,
) -> b::UntypedHandle {
    let Transfer {
        gltf_path,
        flaws: _,
        has_scene,
        cursor: _, // TODO: render cursor as a gizmo
        cameras,
        sky,
        light_data,
    } = transfer;
    let aic_camera = cameras.world;

    // Delete old glTF scene entity.
    for old_entity in old_scene_root {
        commands.entity(old_entity).despawn();
    }

    // Create new glTF scene entity.
    let maybe_scene_handle = if has_scene {
        let handle = asset_server.load(b::GltfAssetLabel::Scene(0).from_asset(gltf_path));
        let new_scene_root = b::SceneRoot(handle.clone());

        let mut scene_entity = commands.spawn((SceneTag, new_scene_root));

        if let Some(light_data) = light_data
            && aic_camera.options().lighting_display != LightingOption::None
        {
            scene_entity.with_child(convert_volumetric_light(light_data, &mut images));
        }

        handle.untyped()
    } else {
        // Load the glTF but don't expect a scene from it.
        asset_server.load::<b::Gltf>(gltf_path).untyped()
    };

    // Update sky and ensure ambient light is disabled.
    let sky_color = convert_color(sky.mean().with_alpha_one());
    *clear_color = b::ClearColor(sky_color);
    ambient_light.brightness = 0.0;

    // Update camera
    let (bevy_projection, bevy_transform, msaa, bloom, fog) = &mut *bevy_camera;
    {
        // Convert translation
        let translation: b::Vec3 = <[f32; 3]>::from(aic_camera.view_position().to_f32()).into();

        // Convert rotation
        let rot_quat = aic_camera.view_transform().rotation;
        let rotation = b::Quat::from_xyzw(
            rot_quat.i as f32,
            rot_quat.j as f32,
            rot_quat.k as f32,
            rot_quat.r as f32,
        )
        .normalize();

        let view_distance = aic_camera.view_distance().into_inner() as f32;
        **bevy_projection = b::Projection::Perspective(b::PerspectiveProjection {
            fov: aic_camera.options().fov_y.into_inner().to_radians() as f32,
            aspect_ratio: aic_camera.viewport().nominal_aspect_ratio() as f32,
            near: aic_camera.near_plane_distance().into_inner() as f32,
            far: view_distance,
            ..Default::default()
        });
        **bevy_transform = b::Transform {
            translation,
            rotation,
            scale: b::Vec3::ONE,
        };
        **msaa = match aic_camera.options().antialiasing {
            all_is_cubes_render::camera::AntialiasingOption::None => b::Msaa::Off,
            all_is_cubes_render::camera::AntialiasingOption::IfCheap
            | all_is_cubes_render::camera::AntialiasingOption::Always => b::Msaa::Sample4,
            _ => todo!(),
        };
        **bloom = bloom::Bloom {
            intensity: aic_camera.options().bloom_intensity.into_inner() * 4.0,
            ..Default::default()
        };
        let falloff = match aic_camera.options().fog {
            FogOption::None => bevy::pbr::FogFalloff::Linear {
                start: view_distance * 2.,
                end: view_distance * 2.,
            },
            FogOption::Abrupt => bevy::pbr::FogFalloff::Linear {
                start: view_distance * 0.8,
                end: view_distance,
            },
            FogOption::Compromise => bevy::pbr::FogFalloff::from_visibility_squared(view_distance),
            FogOption::Physical => bevy::pbr::FogFalloff::from_visibility(view_distance),
            _ => unimplemented!("missing FogOption {fog:?}"),
        };
        **fog = match aic_camera.options().fog {
            FogOption::None => bevy::pbr::DistanceFog {
                color: b::Color::NONE,
                ..Default::default()
            },
            FogOption::Abrupt | FogOption::Compromise | FogOption::Physical => {
                bevy::pbr::DistanceFog {
                    color: sky_color,
                    falloff,
                    ..Default::default()
                }
            }
            _ => unimplemented!("missing FogOption {fog:?}"),
        };
    }

    maybe_scene_handle
}

/// Convert [`all-is-cubes`] light data into a Bevy `IrradianceVolume`.
fn convert_volumetric_light(
    light_data: Vol<Box<[PackedLight]>>,
    images: &mut b::Assets<b::Image>,
) -> impl b::Bundle {
    let light_data = expand_light(expand_light(light_data));

    let bounds = light_data.bounds();
    let size = bounds.size();
    let stride = size.to_vector().to_i32();
    let stride = evec3(stride.z, stride.y, stride.x);

    // TODO: Fill in all uninitialized light values with nearest neighbor.

    // See https://docs.rs/bevy/0.18.1/bevy/pbr/irradiance_volume/ for information on
    // the data layout we are constructing here.
    let texture_bounds = GridAab::from_lower_size(
        // scrambled axes because the ordering should be X-major but isn't
        [
            bounds.lower_bounds().z,
            bounds.lower_bounds().y,
            bounds.lower_bounds().x,
        ],
        [size.depth * 3, size.height * 2, size.width],
    );
    let mut texture_data: Vol<Box<[[f16; 4]]>> = Vol::repeat(texture_bounds, [f16::ZERO; 4]);
    for (cube, &light) in light_data.iter() {
        let [r, g, b]: [f16; 3] = <[f32; 3]>::from(light.value()).map(f16::from_f32);
        let texture_value = [r, g, b, f16::ONE];
        //let texture_value = [f16::ONE; 4]; // stub for debugging

        // Kludge: `Vol` doesn't support X-major ordering yet, so scramble Z-major to fit.
        let cube = Cube::new(cube.z, cube.y, cube.x);

        // We don't store distinct values for all 6 directions/faces, so duplicate them.
        texture_data[cube] = texture_value;
        texture_data[cube + stride.component_mul(evec3(1, 0, 0))] = texture_value;
        texture_data[cube + stride.component_mul(evec3(2, 0, 0))] = texture_value;
        texture_data[cube + stride.component_mul(evec3(0, 1, 0))] = texture_value;
        texture_data[cube + stride.component_mul(evec3(1, 1, 0))] = texture_value;
        texture_data[cube + stride.component_mul(evec3(2, 1, 0))] = texture_value;
    }

    (
        b::Transform {
            translation: <[f32; 3]>::from(bounds.center().to_f32()).into(),
            rotation: b::Quat::default(),
            scale: <[f32; 3]>::from(bounds.size().to_f32()).into(),
        },
        bevy::light::IrradianceVolume {
            voxels: images.add(b::Image::new(
                wgpu::Extent3d {
                    // unswizzle axes
                    width: texture_bounds.size().depth,
                    height: texture_bounds.size().height,
                    depth_or_array_layers: texture_bounds.size().width,
                },
                bevy::render::render_resource::TextureDimension::D3,
                texture_data
                    .into_elements()
                    .into_iter()
                    .flatten()
                    .flat_map(f16::to_ne_bytes)
                    .collect(),
                bevy::render::render_resource::TextureFormat::Rgba16Float,
                bevy::asset::RenderAssetUsages::RENDER_WORLD,
            )),
            // TODO: empirically chosen to match; justify this number
            intensity: 1024.0,
            affects_lightmapped_meshes: false,
        },
        b::LightProbe,
    )
}

/// Fill in uninitialized light data with neighbors.
fn expand_light(original: Vol<Box<[PackedLight]>>) -> Vol<Box<[PackedLight]>> {
    // Kludge: The meanings of the packed status values are not public but we're peeking at them
    // anyway. Larger status values are considered more “good” so we prefer them.
    fn valid(p: PackedLight) -> bool {
        p.as_texel()[3] == 255
    }

    Vol::from_fn(original.bounds(), |cube| {
        let original_value = original[cube];
        if valid(original_value) {
            original_value
        } else {
            Face::ALL
                .into_iter()
                .filter_map(|dir| original.get(cube + dir).copied())
                .max_by_key(|p| p.as_texel()[3])
                .unwrap_or(original_value)
        }
    })
}

/// System which modifies loaded glTF materials to obey our camera options.
fn patch_gltf_materials(
    b::In(cameras): b::In<Layers<Camera>>,
    scene: Option<b::Single<b::Entity, b::With<SceneTag>>>,
    mut commands: b::Commands,
    children: b::Query<&b::Children>,
    mesh_materials: b::Query<&b::MeshMaterial3d<StandardMaterial>>,
    mut asset_materials: b::ResMut<b::Assets<StandardMaterial>>,
) {
    // Based on https://bevy.org/examples/3d-rendering/edit-material-on-gltf/

    let mut patch_cache: HashMap<b::Handle<StandardMaterial>, b::Handle<StandardMaterial>> =
        HashMap::new();

    if let Some(scene) = scene {
        for descendant in children.iter_descendants(*scene) {
            let Ok(b::MeshMaterial3d(old_handle)) = mesh_materials.get(descendant) else {
                continue;
            };
            let Some(old_material) = asset_materials.get(old_handle).cloned() else {
                continue;
            };
            let patched_handle: &b::Handle<StandardMaterial> =
                &*patch_cache.entry(old_handle.clone()).or_insert_with(|| {
                    let mut new_material = old_material.clone();
                    if cameras.world.options().lighting_display == LightingOption::None {
                        new_material.unlit = true;
                    }
                    asset_materials.add(new_material)
                });

            // Replace material handle
            commands.entity(descendant).insert(b::MeshMaterial3d(patched_handle.clone()));
        }
    }
}

fn convert_color(color: Rgba) -> b::Color {
    let [r, g, b, a] = color.into();
    b::Color::linear_rgba(r, g, b, a)
}
