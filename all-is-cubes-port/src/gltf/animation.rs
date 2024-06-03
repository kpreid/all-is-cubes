use std::io;
use std::time::Duration;

use gltf::Index;

use super::buffer::create_buffer_and_accessor;
use super::glue::convert_quaternion;
use super::{GltfWriter, MeshInstance};

#[derive(Debug)]
pub(crate) struct FrameState {
    /// The set of mesh-instances that should be visible in this frame.
    /// This information will be used to assemble a glTF animation.
    ///
    /// glTF animation doesn't support adding/removing objects explicitly, but
    /// setting the scale to zero is explicitly noted in the specification, and so
    /// that is the animation that is generated.
    pub visible_mesh_instances: Vec<MeshInstance>,

    // The camera's state in this frame.
    pub camera_transform: all_is_cubes_render::camera::ViewTransform,
}

pub(crate) fn add_camera_animation(
    writer: &mut GltfWriter,
    camera_node_index: Index<gltf::Node>,
    frame_pace: Duration,
) -> io::Result<()> {
    let mut animation_channels = Vec::new();
    let mut animation_samplers = Vec::new();

    let time_accessor = create_buffer_and_accessor(
        &mut writer.root,
        &writer.buffer_dest,
        "camera animation time".into(),
        "camera-time",
        writer
            .frame_states
            .iter()
            .enumerate()
            .map(|(i, _)| [frame_pace.as_secs_f32() * i as f32]),
    )?;

    // Translation
    animation_channels.push(gltf::animation::Channel {
        sampler: Index::push(
            &mut animation_samplers,
            gltf::animation::Sampler {
                input: time_accessor,
                interpolation: gltf::animation::Interpolation::Linear,
                output: create_buffer_and_accessor(
                    &mut writer.root,
                    &writer.buffer_dest,
                    "camera animation translation".into(),
                    "camera-pos",
                    writer
                        .frame_states
                        .iter()
                        .map(|frame| frame.camera_transform.translation.to_f32().into()),
                )?,
                unrecognized_extensions: Default::default(),
                extras: Default::default(),
            },
        ),
        target: gltf::animation::Target {
            node: camera_node_index,
            path: gltf::animation::Property::Translation,
            unrecognized_extensions: Default::default(),
            extras: Default::default(),
        },
        unrecognized_extensions: Default::default(),
        extras: Default::default(),
    });
    // Rotation
    animation_channels.push(gltf::animation::Channel {
        sampler: Index::push(
            &mut animation_samplers,
            gltf::animation::Sampler {
                input: time_accessor,
                interpolation: gltf::animation::Interpolation::Linear,
                output: create_buffer_and_accessor(
                    &mut writer.root,
                    &writer.buffer_dest,
                    "camera animation rotation".into(),
                    "camera-rot",
                    writer
                        .frame_states
                        .iter()
                        .map(|frame| convert_quaternion(frame.camera_transform.rotation)),
                )?,
                unrecognized_extensions: Default::default(),
                extras: Default::default(),
            },
        ),
        target: gltf::animation::Target {
            node: camera_node_index,
            path: gltf::animation::Property::Rotation,
            unrecognized_extensions: Default::default(),
            extras: Default::default(),
        },
        unrecognized_extensions: Default::default(),
        extras: Default::default(),
    });

    writer.root.push(gltf::Animation {
        name: Some("camera movement".into()),
        channels: animation_channels,
        samplers: animation_samplers,
        unrecognized_extensions: Default::default(),
        extras: Default::default(),
    });

    Ok(())
}
