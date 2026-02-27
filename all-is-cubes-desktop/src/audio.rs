//! Glue between [`all_is_cubes::sound`] and actual system audio.
//!
//! Note: This contains extremely similar code to the web version at
//! `all-is-cubes-wasm/src/audio.rs`. Most changes should apply to both.

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::AtomicU32;
use std::sync::{Arc, atomic};
use std::time::Duration;

use kira::listener::ListenerHandle;
use kira::sound::static_sound::{StaticSoundData, StaticSoundSettings};
use kira::track::SpatialTrackBuilder;
use kira::{AudioManager, PlaySoundError};
use rand::{RngExt as _, SeedableRng as _};

use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes::sound::{Band, SoundDef, SpatialAmbient};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes_render::camera::{self, Camera, Layers, StandardCameras};
use all_is_cubes_ui::apps::{SessionFluff, SessionFluffSource};

use crate::Session;

// -------------------------------------------------------------------------------------------------

/// Fills the audio slot in a `DesktopSession` to actually produce audio.
pub(crate) struct AudioTask {
    #[expect(
        dead_code,
        reason = "eventually we're going to need this for volume control etc."
    )]
    sender: flume::Sender<AudioCommand>,
    world_spatial_listener: ListenerHandle,

    // TODO: having our own separate `StandardCameras` is overkill for audio, but because it
    // needs to be updated exclusively, we don't have a good way to share it with the renderer
    // straightforwardly. Find a better architecture.
    cameras_for_audio: StandardCameras,
}

impl fmt::Debug for AudioTask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AudioTask").finish_non_exhaustive()
    }
}

impl AudioTask {
    pub(crate) fn new(session: &Session) -> Result<AudioTask, anyhow::Error> {
        let mut manager = AudioManager::<kira::backend::cpal::CpalBackend>::new(
            kira::AudioManagerSettings::default(),
        )?;
        let cameras_for_audio =
            session.create_cameras(listen::constant(camera::Viewport::with_scale(1.0, [0, 0])));

        // TODO: non-placeholder initial values
        let (initial_position, initial_orientation) =
            convert_view_to_kira_listener(cameras_for_audio.cameras().world.view_transform());
        let world_spatial_listener = manager.add_listener(initial_position, initial_orientation)?;
        let world_spatial_listener_id = world_spatial_listener.id();

        // Transfers audio events to the audio processing thread.
        // We never block on this channel, but drop things if full.
        let (sender, receiver) = flume::bounded(256);

        // Hook up ambient sound to channel
        let ambient_source = session.ambient_sound();
        // TODO: this whole custom listener scheme should be replaced with something more async
        // and less custom-code-per-use-case.
        ambient_source.listen(UpdateAmbientListener(sender.clone()));
        sender.send(AudioCommand::UpdateAmbient).unwrap(); // ensure initial sync

        // Hook up fluff to channel
        session.listen_fluff(FluffListener(sender.clone()));

        // The audio processing is done on a thread because `AudioManager` wants `&mut self`
        // to perform operations, so I either need a dedicated thread (actor) or a mutex.
        // We also don’t want to find ourselves running synthesis inside the Listener.
        // Note that this is *not* a sample-by-sample realtime audio thread.
        if let Err(e) = std::thread::Builder::new()
            .name("all_is_cubes audio core".to_owned())
            .spawn(move || {
                audio_command_thread(receiver, manager, ambient_source, world_spatial_listener_id)
            })
        {
            log::error!("failed to spawn audio command thread; audio disabled: {e}");
        }

        Ok(AudioTask {
            sender,
            world_spatial_listener,
            cameras_for_audio,
        })
    }

    pub(crate) fn update_listener(&mut self, read_tickets: Layers<ReadTicket<'_>>) {
        self.cameras_for_audio.update(read_tickets);
        Self::set_listener_state(
            &mut self.world_spatial_listener,
            &self.cameras_for_audio.cameras().world,
        );
    }

    fn set_listener_state(handle: &mut ListenerHandle, camera: &Camera) {
        let (position, rotation) = convert_view_to_kira_listener(camera.view_transform());
        let tween = kira::Tween {
            start_time: kira::StartTime::Immediate,
            duration: Duration::from_millis(16), // TODO: get from frame rate
            easing: kira::Easing::Linear,
        };

        handle.set_position(kira::Value::Fixed(position), tween);
        handle.set_orientation(kira::Value::Fixed(rotation), tween);
    }
}

#[derive(Debug)]
enum AudioCommand {
    Fluff(SessionFluff),
    UpdateAmbient,
}

/// Thread function for receiving commands and executing them on `&mut AudioManager`.
/// This is not a real-time audio thread.
#[expect(clippy::needless_pass_by_value)]
fn audio_command_thread(
    receiver: flume::Receiver<AudioCommand>,
    mut manager: AudioManager,
    ambient_source: listen::DynSource<SpatialAmbient>,
    spatial_listener_id: kira::listener::ListenerId,
) {
    let sample_rate = 44100;
    let mut sound_cache: HashMap<SoundDef, StaticSoundData> = HashMap::new();
    let mut rng = rand::rngs::StdRng::seed_from_u64(0);

    // TODO: maybe synthesize white noise continuously to avoid any chance of audible repetition?
    let white_noise = StaticSoundData {
        sample_rate: 44100,
        frames: std::iter::repeat_with(|| kira::Frame::from_mono(rng.random()))
            .take(44100 * 10)
            .collect(),
        settings: StaticSoundSettings {
            loop_region: Some(kira::sound::Region::default()),
            ..Default::default()
        },
        slice: None,
    };

    let mut ambient_noise_tracks: [(Option<StereoVolume>, Option<kira::track::TrackHandle>);
        Band::COUNT] = core::array::from_fn(|band_index| {
        let mut track_builder = kira::track::TrackBuilder::new();
        // TODO: start with band-limited noise instead of filtering?
        let frequency: f32 = Band::from_index(band_index).center_frequency().into_inner();
        track_builder.add_effect(
            kira::effect::filter::FilterBuilder::new()
                .cutoff(f64::from(frequency))
                .mode(kira::effect::filter::FilterMode::BandPass)
                .resonance(0.8),
        );
        let volume = track_builder.add_effect(StereoVolume::new());
        let mut ambient_track = manager.add_sub_track(track_builder).unwrap(); // TODO: log instead of unwrap
        ambient_track.play(white_noise.clone()).unwrap();
        (Some(volume), Some(ambient_track))
    });

    while let Ok(message) = receiver.recv() {
        match message {
            AudioCommand::Fluff(SessionFluff { fluff, source }) => {
                let _ = source; // TODO: spatial audio

                if let Some((sound_def, amplitude)) = fluff.sound() {
                    // TODO: Need a better solution than comparing sounds by value.
                    // When we have the sounds stored in Universes instead of as constants,
                    // we can compare the Handles by name.
                    let mut sound: StaticSoundData = match sound_cache.get(sound_def) {
                        Some(sound) => sound.clone(),
                        None => {
                            let new_sound = convert_sound_to_kira(sample_rate, sound_def);
                            sound_cache.insert(sound_def.clone(), new_sound.clone());
                            new_sound
                        }
                    };
                    sound.settings.volume = amplitude_to_value(amplitude);
                    // Randomize start time to reduce constructive interference effects.
                    // TODO: get size of time range from universe tick rate.
                    sound.settings.start_time = kira::StartTime::Delayed(Duration::from_nanos(
                        rng.random_range(0..16_000_000),
                    ));

                    match source {
                        SessionFluffSource::World(position) => {
                            match manager.add_spatial_sub_track(
                                spatial_listener_id,
                                position.to_vector().to_f32(),
                                SpatialTrackBuilder::new()
                                    .persist_until_sounds_finish(true)
                                    .sound_capacity(1),
                            ) {
                                Ok(mut track) => {
                                    log_playback_result(track.play(sound));
                                }
                                Err(error) => log::error!(
                                    "Playback error creating spatial_sub_track: {error}",
                                    error = all_is_cubes::util::ErrorChain(&error)
                                ),
                            }
                        }
                        SessionFluffSource::NonSpatial => {
                            log_playback_result(manager.play(sound));
                        }
                        _ => unimplemented!("unknown `SessionFluffSource` variant {source:?}"),
                    }
                }
            }
            AudioCommand::UpdateAmbient => {
                let new_ambient = ambient_source.get();

                for (left_amplitude, right_amplitude, band_playback_volume) in itertools::izip!(
                    new_ambient.left.noise_bands.0.iter(),
                    new_ambient.right.noise_bands.0.iter(),
                    ambient_noise_tracks.iter_mut().map(|(vol_handle, _)| vol_handle)
                ) {
                    if let Some(v) = band_playback_volume {
                        v.set_amplitude(
                            [left_amplitude.into_inner(), right_amplitude.into_inner()],
                            //kira::Tween::default(),
                        );
                    }
                }
            }
        }
    }
}

// TODO: wait, we are probably mixing up amplitude and power
fn amplitude_to_value(amplitude: f32) -> kira::Value<kira::Decibels> {
    kira::Decibels((amplitude.log10() * 20.0).max(-200.0)).into()
}

fn log_playback_result(
    result: Result<kira::sound::static_sound::StaticSoundHandle, PlaySoundError<()>>,
) {
    match result {
        Ok(_handle) => {}
        Err(PlaySoundError::SoundLimitReached) => {
            log::trace!("sound limit reached");
        }
        Err(error) => log::error!(
            "Playback error: {error}",
            error = all_is_cubes::util::ErrorChain(&error)
        ),
    }
}

// -------------------------------------------------------------------------------------------------

// TODO: Can we de-boilerplate these listener-to-channel adapters somehow?
// We have basically identical wasm and non-wasm code, but no clear place for it to live.
// (`all_is_cubes::util` is a place for adapters, but it doesn't depend on `flume`.)

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

/// Adapter from the `session.ambient_sound()` source to the audio command channel.
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

fn convert_view_to_kira_listener(
    view_transform: camera::ViewTransform,
) -> (mint::Vector3<f32>, mint::Quaternion<f32>) {
    let position: mint::Vector3<f32> = mint::Vector3::from(view_transform.translation.to_f32());

    // `euclid` quaternion has no scalar conversion operators, so we have to do it elementwise
    // despite `mint`.
    // On the positive side, `kira`'s coordinate system for listeners is identical to ours.
    let rotation: mint::Quaternion<f32> = mint::Quaternion {
        v: mint::Vector3::from(view_transform.rotation.vector_part().to_f32()),
        s: view_transform.rotation.r as f32,
    };

    (position, rotation)
}

fn convert_sound_to_kira(sample_rate: u32, sound: &SoundDef) -> StaticSoundData {
    StaticSoundData {
        sample_rate,
        frames: sound
            .synthesize(sample_rate as f32)
            .map(|[left, right]| kira::Frame { left, right })
            .collect(),
        settings: Default::default(),
        slice: Default::default(),
    }
}

// -------------------------------------------------------------------------------------------------

/// Effect node for stereo (independent left and right) volume control.
#[derive(Clone)]
struct StereoVolume(Arc<[AtomicU32; 2]>);
impl StereoVolume {
    fn new() -> Self {
        Self(Arc::new([
            AtomicU32::new(0.0f32.to_bits()),
            AtomicU32::new(0.0f32.to_bits()),
        ]))
    }

    fn set_amplitude(&self, [l, r]: [f32; 2]) {
        self.0[0].store(l.to_bits(), atomic::Ordering::Relaxed);
        self.0[1].store(r.to_bits(), atomic::Ordering::Relaxed);
    }
}

impl kira::effect::Effect for StereoVolume {
    fn process(&mut self, input: &mut [kira::Frame], _dt: f64, _info: &kira::info::Info<'_>) {
        let left_amplitude = f32::from_bits(self.0[0].load(atomic::Ordering::Relaxed));
        let right_amplitude = f32::from_bits(self.0[1].load(atomic::Ordering::Relaxed));
        for frame in input {
            frame.left *= left_amplitude;
            frame.right *= right_amplitude;
        }
    }
}

impl kira::effect::EffectBuilder for StereoVolume {
    type Handle = Self;

    fn build(self) -> (Box<dyn kira::effect::Effect>, Self::Handle) {
        (Box::new(self.clone()), self)
    }
}
