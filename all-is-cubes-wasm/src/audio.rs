//! Glue between [`all_is_cubes::sound`] and Web Audio API.
//!
//! Note: This contains extremely similar code to the desktop version at
//! `all-is-cubes-desktop/src/audio.rs`. Most changes should apply to both.

use core::fmt;
use std::collections::HashMap;

use rand::{RngExt as _, SeedableRng as _};
use wasm_bindgen::JsValue;
use web_sys::{AudioBuffer, AudioContext, AudioListener, GainNode};

use all_is_cubes::euclid::vec3;
use all_is_cubes::listen::{self, Listen as _, Listener as _};
use all_is_cubes::sound::{self, SoundDef};
use all_is_cubes_render::camera::Camera;
use all_is_cubes_ui::apps::{SessionFluff, SessionFluffSource};

use crate::web_session::Session;

// -------------------------------------------------------------------------------------------------

/// Allows setting audio playback parameters. Dropping this stops audio.
pub struct AudioTask {
    #[expect(dead_code, reason = "used for its drop effect")]
    gate: listen::Gate,
    // sender: flume::Sender<AudioCommand>,
    listener: AudioListener,
}

impl AudioTask {
    pub fn new(session: &Session) -> Result<Self, JsValue> {
        // We do not currently need any options
        let context = AudioContext::new()?;

        // Transfers audio events to the audio processing task.
        let (sender, receiver) = flume::bounded(256);

        // Hook up ambient sound to channel
        let ambient_source = session.ambient_sound();
        ambient_source.listen(UpdateAmbientListener(sender.clone()));
        sender.send(AudioCommand::UpdateAmbient).unwrap(); // ensure initial sync

        let (gate, listener) = FluffListener(sender).gate();
        session.listen_fluff(listener);

        let audio_task_handle = AudioTask {
            // sender,
            listener: context.listener(),
            gate,
        };

        wasm_bindgen_futures::spawn_local(audio_command_task(receiver, context, ambient_source));

        Ok(audio_task_handle)
    }

    pub fn set_listener(&self, camera: &Camera) {
        let transform = camera.view_transform().to_transform();
        let position = camera.view_position();
        let forward = transform.transform_vector3d(vec3(0., 0., -1.));
        let up = transform.transform_vector3d(vec3(0., 1., 0.));

        // Note: https://www.w3.org/TR/webaudio-1.1/#AudioListener says these setters are
        // deprecated, but `web-sys` does not seem to offer the alternative.
        self.listener.set_position(position.x, position.y, position.z);
        self.listener.set_orientation(forward.x, forward.y, forward.z, up.x, up.y, up.z);
        // TODO: listener.set_velocity(velocity.x, velocity.y, velocity.z);
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
enum AudioCommand {
    Fluff(SessionFluff),
    UpdateAmbient,
}

/// Async task which does most but not all of the [`AudioContext`] control.
async fn audio_command_task(
    receiver: flume::Receiver<AudioCommand>,
    context: AudioContext,
    ambient_source: listen::DynSource<sound::SpatialAmbient>,
) {
    let sample_rate = context.sample_rate().round() as u32;
    let mut sound_cache: HashMap<SoundDef, AudioBuffer> = HashMap::new();

    let mut ambient_noise_gains = match make_ambient_sound_synthesizer(sample_rate, &context) {
        Ok(g) => Some(g),
        Err(e) => {
            log::error!("error initializing ambient sound: {e:?}");
            None
        }
    };

    // This loop will exit when the channel is closed, which will happen when the `FluffListener`
    // or the `Gate` is dropped.
    while let Ok(message) = receiver.recv_async().await {
        match message {
            AudioCommand::Fluff(SessionFluff { fluff, source }) => {
                if let Some((sound_def, gain)) = fluff.sound() {
                    // TODO: Need a better solution than comparing sounds by value.
                    // When we have the sounds stored in Universes instead of as constants,
                    // we can compare the Handles by name.
                    let buffer: AudioBuffer = match sound_cache.get(sound_def) {
                        Some(buffer) => buffer.clone(),
                        None => match convert_sound_to_buffer(sample_rate, sound_def) {
                            Ok(buffer) => {
                                sound_cache.insert(sound_def.clone(), buffer.clone());
                                buffer
                            }
                            Err(e) => {
                                // If this actually happens, it is either a bug or we're out of
                                // memory to store audio.
                                log::warn!("Error while filling sound cache: {e:?}");
                                continue;
                            }
                        },
                    };

                    // Node chain we are constructing:
                    // BufferSource → Gain → (Panner)? → Destination

                    let source_node = context.create_buffer_source().unwrap();
                    source_node.set_buffer(Some(&buffer));

                    let gain_node = context.create_gain().unwrap();
                    gain_node.gain().set_value(gain);

                    source_node
                        .connect_with_audio_node(&gain_node)
                        .expect("audio graph logic error");

                    match source {
                        SessionFluffSource::World(position) => {
                            let panner_node = context.create_panner().unwrap();
                            panner_node.set_position(position.x, position.y, position.z);
                            panner_node.set_panning_model(web_sys::PanningModelType::Hrtf);

                            gain_node
                                .connect_with_audio_node(&panner_node)
                                .expect("audio graph logic error");
                            panner_node
                                .connect_with_audio_node(&context.destination())
                                .expect("audio graph logic error");
                        }
                        SessionFluffSource::NonSpatial => {
                            // Make direct connection without panner node
                            gain_node
                                .connect_with_audio_node(&context.destination())
                                .expect("audio graph logic error");
                        }
                        _ => unimplemented!("unknown `SessionFluffSource` variant {source:?}"),
                    }

                    // Randomize start time to reduce constructive interference effects.
                    // TODO: get size of time range from universe tick rate.
                    let when =
                        js_sys::Math::random().mul_add(const { 1. / 60. }, context.current_time());

                    source_node.start_with_when(when).expect("audio graph logic error");

                    // log::trace!("played {fluff:?}");
                }
            }

            AudioCommand::UpdateAmbient => {
                let new_ambient = ambient_source.get();

                let Some(ambient_noise_gains) = &mut ambient_noise_gains else {
                    continue;
                };
                for (left_amplitude, right_amplitude, gain_nodes) in itertools::izip!(
                    new_ambient.left.noise_bands.0.iter(),
                    new_ambient.right.noise_bands.0.iter(),
                    ambient_noise_gains.iter_mut()
                ) {
                    let [l, r] = gain_nodes;
                    // TODO: smoothing
                    l.gain().set_value(left_amplitude.into_inner());
                    r.gain().set_value(right_amplitude.into_inner());
                }
            }
        }
    }

    log::trace!("audio_command_task's channel closed");
}

fn make_ambient_sound_synthesizer(
    sample_rate: u32,
    context: &AudioContext,
) -> Result<[[GainNode; 2]; sound::Band::COUNT], JsValue> {
    // Errors that are logic bugs are unwrapped and errors that might be resource
    // exhaustion are not.

    // Node that outputs white noise for use by ambient sound synthesis.
    let white_noise_node = context.create_buffer_source()?;
    white_noise_node.set_buffer(Some(&white_noise(sample_rate, 50_000)?));
    white_noise_node.set_loop(true);
    white_noise_node.start()?;

    let ambient_noise_gains: [[GainNode; 2]; sound::Band::COUNT] =
        try_array_from_fn(|band_index| -> Result<[GainNode; 2], JsValue> {
            // Band-pass filter node to produce filter node.
            // TODO: start with band-limited noise instead of realtime filtering?
            let frequency: f32 =
                sound::Band::from_index(band_index).center_frequency().into_inner();
            let filter_node = context.create_biquad_filter()?;
            white_noise_node.connect_with_audio_node(&filter_node).expect("connection");
            filter_node.set_type(web_sys::BiquadFilterType::Bandpass);
            filter_node.frequency().set_value(frequency);
            filter_node.q().set_value(0.8); // TODO: standardize this instead of each backend setting it

            // Merge independent channel gain nodes into one stereo output
            let stereo_merge = context.create_channel_merger_with_number_of_inputs(2)?;
            stereo_merge
                .connect_with_audio_node(&context.destination())
                .expect("connection");

            let gain_nodes: [GainNode; 2] =
                try_array_from_fn(|channel| -> Result<GainNode, JsValue> {
                    let gain_node = context.create_gain()?;
                    filter_node.connect_with_audio_node(&gain_node).expect("connection");
                    gain_node
                        .connect_with_audio_node_and_output_and_input(
                            &stereo_merge,
                            0,
                            channel as u32,
                        )
                        .expect("connection");
                    Ok(gain_node)
                })?;

            Ok(gain_nodes)
        })?;

    Ok(ambient_noise_gains)
}

// -------------------------------------------------------------------------------------------------

/// Adapter from [`Listener`] to the audio command channel.
struct FluffListener(flume::Sender<AudioCommand>);

impl fmt::Debug for FluffListener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FluffListener")
            .field("alive", &!self.0.is_disconnected())
            .finish_non_exhaustive()
    }
}
impl listen::Listener<SessionFluff> for FluffListener {
    fn receive(&self, fluffs: &[SessionFluff]) -> bool {
        if fluffs.is_empty() {
            !self.0.is_disconnected()
        } else {
            for fluff in fluffs {
                match self.0.try_send(AudioCommand::Fluff(fluff.clone())) {
                    Ok(()) => {}
                    Err(flume::TrySendError::Full(_)) => {} // drop message if full
                    Err(flume::TrySendError::Disconnected(_)) => return false,
                }
            }
            true
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Adapter from [`Source`] to the audio command channel.
struct UpdateAmbientListener(flume::Sender<AudioCommand>);

impl fmt::Debug for UpdateAmbientListener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UpdateAmbientListener")
            .field("alive", &!self.0.is_disconnected())
            .finish_non_exhaustive()
    }
}

impl listen::Listener<()> for UpdateAmbientListener {
    fn receive(&self, messages: &[()]) -> bool {
        match messages {
            [] => !self.0.is_disconnected(),
            // Coalesce multiple messages into one.
            [(), ..] => match self.0.try_send(AudioCommand::UpdateAmbient) {
                Ok(()) => true,
                Err(flume::TrySendError::Full(_)) => true, // drop message if full
                Err(flume::TrySendError::Disconnected(_)) => false,
            },
        }
    }
}

// -------------------------------------------------------------------------------------------------

fn convert_sound_to_buffer(sample_rate: u32, sound: &SoundDef) -> Result<AudioBuffer, JsValue> {
    let (left_vec, right_vec): (Vec<f32>, Vec<f32>) =
        sound.synthesize(sample_rate as f32).map(|[l, r]| (l, r)).unzip();

    let buffer = AudioBuffer::new(&{
        let options = web_sys::AudioBufferOptions::new(
            left_vec.len().try_into().unwrap(),
            sample_rate as f32,
        );
        options.set_number_of_channels(2);
        options
    })?; // note: error could be OOM error

    buffer
        .copy_to_channel(left_vec.as_slice(), 0)
        .expect("channel buffer length error");
    buffer
        .copy_to_channel(right_vec.as_slice(), 1)
        .expect("channel buffer length error");

    Ok(buffer)
}

fn white_noise(sample_rate: u32, sample_count: u32) -> Result<AudioBuffer, JsValue> {
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);

    let buffer = AudioBuffer::new(&{
        let options = web_sys::AudioBufferOptions::new(sample_count, sample_rate as f32);
        options.set_number_of_channels(1);
        options
    })?;

    #[expect(clippy::map_with_unused_argument_over_ranges)]
    let vec: Vec<f32> = (0..sample_count).map(|_| rng.random()).collect();

    buffer.copy_to_channel(vec.as_slice(), 0).expect("channel buffer length error");

    Ok(buffer)
}

// -------------------------------------------------------------------------------------------------

// Stable substitute for <https://doc.rust-lang.org/std/array/fn.try_from_fn.html>.
//
// Caveat: Will call the function N times even if it fails.
fn try_array_from_fn<T, E, const N: usize>(
    function: impl FnMut(usize) -> Result<T, E>,
) -> Result<[T; N], E> {
    let array_of_results: [Result<T, E>; N] = core::array::from_fn(function);
    if array_of_results.iter().any(|result| result.is_err()) {
        Err(array_of_results.into_iter().find_map(Result::err).unwrap())
    } else {
        Ok(array_of_results.map(|result| match result {
            Ok(value) => value,
            Err(_) => unreachable!(),
        }))
    }
}
