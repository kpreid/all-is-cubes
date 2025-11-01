use core::fmt;
use std::collections::HashMap;
use std::sync::atomic;

use wasm_bindgen::JsValue;
use web_sys::{AudioBuffer, AudioContext};

use all_is_cubes::fluff::Fluff;
use all_is_cubes::listen::{self, Listener as _};
use all_is_cubes::sound::SoundDef;

use crate::web_session::Session;

// -------------------------------------------------------------------------------------------------

/// Create the audio context and task.
///
/// Dropping the returned [`listen::Gate`] stops audio.
/// TODO: Should it be stopped immediately or gracefully?
pub(crate) fn initialize_audio(session: &Session) -> Result<listen::Gate, JsValue> {
    // We do not currently need any options
    let context = AudioContext::new()?;

    // Transfers audio events to the audio processing task.
    let (sender, receiver) = flume::bounded(256);

    wasm_bindgen_futures::spawn_local(audio_command_task(receiver, context));

    let (gate, listener) = FluffListener::new(sender).gate();
    session.listen_fluff(listener);
    Ok(gate)
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
enum AudioCommand {
    Fluff(Fluff),
}

/// Async task for receiving commands, synthesizing audio, and managing the [`AudioContext`].
async fn audio_command_task(receiver: flume::Receiver<AudioCommand>, context: AudioContext) {
    let sample_rate = context.sample_rate().round() as u32;
    let mut sound_cache: HashMap<SoundDef, AudioBuffer> = HashMap::new();

    // This loop will exit when the channel is closed, which will happen when the `FluffListener`
    // or the `Gate` is dropped.
    while let Ok(message) = receiver.recv_async().await {
        match message {
            AudioCommand::Fluff(fluff) => {
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

                    let source_node = context.create_buffer_source().unwrap();
                    source_node.set_buffer(Some(&buffer));

                    let gain_node = context.create_gain().unwrap();
                    gain_node.gain().set_value(gain);

                    source_node
                        .connect_with_audio_node(&gain_node)
                        .expect("audio graph logic error");
                    gain_node
                        .connect_with_audio_node(&context.destination())
                        .expect("audio graph logic error");

                    source_node.start().expect("audio graph logic error");

                    // log::trace!("played {fluff:?}");
                }
            }
        }
    }

    log::trace!("audio_command_task's channel closed");
}

// -------------------------------------------------------------------------------------------------

/// Adapter from [`Listener`] to the audio command channel.
struct FluffListener {
    sender: flume::Sender<AudioCommand>,
    alive: atomic::AtomicBool,
}
impl FluffListener {
    fn new(sender: flume::Sender<AudioCommand>) -> Self {
        Self {
            sender,
            alive: atomic::AtomicBool::new(true),
        }
    }
}

impl fmt::Debug for FluffListener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FluffListener")
            .field("alive", &self.alive)
            .finish_non_exhaustive()
    }
}

impl listen::Listener<Fluff> for FluffListener {
    fn receive(&self, fluffs: &[Fluff]) -> bool {
        if !self.alive.load(atomic::Ordering::Relaxed) {
            return false;
        }
        for fluff in fluffs {
            match self.sender.try_send(AudioCommand::Fluff(fluff.clone())) {
                Ok(()) => {}
                Err(flume::TrySendError::Full(_)) => {
                    // If the channel is full, indicating the audio task is falling behind,
                    // drop the message.
                }
                Err(flume::TrySendError::Disconnected(_)) => {
                    self.alive.store(false, atomic::Ordering::Relaxed);
                    return false;
                }
            }
        }
        true
    }
}

// -------------------------------------------------------------------------------------------------

fn convert_sound_to_buffer(sample_rate: u32, sound: &SoundDef) -> Result<AudioBuffer, JsValue> {
    let (left_vec, right_vec): (Vec<f32>, Vec<f32>) =
        sound.synthesize(sample_rate as f32).map(|[l, r]| (l, r)).unzip();

    let options =
        web_sys::AudioBufferOptions::new(left_vec.len().try_into().unwrap(), sample_rate as f32);
    options.set_number_of_channels(2);
    let buffer = AudioBuffer::new(&options)?; // note: error could be OOM error

    buffer
        .copy_to_channel(left_vec.as_slice(), 0)
        .expect("channel buffer length error");
    buffer
        .copy_to_channel(right_vec.as_slice(), 1)
        .expect("channel buffer length error");

    Ok(buffer)
}
