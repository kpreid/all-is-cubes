//! Glue between [`all_is_cubes::sound`] and actual system audio.
//!
//! Note: This contains extremely similar code to the web version at
//! `all-is-cubes-wasm/src/audio.rs`. Most changes should apply to both.

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::AtomicU32;
use std::sync::{Arc, atomic, mpsc};

use kira::AudioManager;
use kira::PlaySoundError;
use kira::sound::static_sound::{StaticSoundData, StaticSoundSettings};
use rand::{Rng as _, SeedableRng};

use all_is_cubes::fluff::Fluff;
use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes::sound::{Band, SoundDef, SpatialAmbient};

use crate::Session;

// -------------------------------------------------------------------------------------------------

/// Fills the audio slot in a `DesktopSession` to actually produce audio.
pub(crate) struct AudioOut {
    #[expect(
        dead_code,
        reason = "eventually we're going to need this for volume control etc."
    )]
    sender: mpsc::SyncSender<AudioCommand>,
}

impl fmt::Debug for AudioOut {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AudioOut").finish_non_exhaustive()
    }
}

#[derive(Debug)]
enum AudioCommand {
    Fluff(Fluff),
    UpdateAmbient,
}

pub(crate) fn init_sound(session: &Session) -> Result<AudioOut, anyhow::Error> {
    let manager = AudioManager::<kira::backend::cpal::CpalBackend>::new(
        kira::AudioManagerSettings::default(),
    )?;

    // Transfers audio events to the audio processing thread.
    // We never block on this channel, but drop things if full.
    let (sender, receiver) = mpsc::sync_channel(256);

    // Hook up ambient sound to channel
    let ambient_source = session.ambient_sound();
    // TODO: this whole custom listener scheme should be replaced with something more async
    // and less custom-code-per-use-case.
    ambient_source.listen(UpdateAmbientListener::new(sender.clone()));
    sender.send(AudioCommand::UpdateAmbient).unwrap(); // ensure initial sync

    // Hook up fluff to channel
    session.listen_fluff(FluffListener::new(sender.clone()));

    // The audio processing is done on a thread because `AudioManager` wants `&mut self`
    // to perform operations, so I either need a dedicated thread (actor) or a mutex.
    // We also don’t want to find ourselves running synthesis inside the Listener.
    // Note that this is *not* a sample-by-sample realtime audio thread.
    if let Err(e) = std::thread::Builder::new()
        .name("all_is_cubes audio core".to_owned())
        .spawn(move || audio_command_thread(receiver, manager, ambient_source))
    {
        log::error!("failed to spawn audio command thread; audio disabled: {e}");
    }

    Ok(AudioOut { sender })
}

/// Thread function for receiving commands and executing them on `&mut AudioManager`.
/// This is not a real-time audio thread.
#[expect(clippy::needless_pass_by_value)]
fn audio_command_thread(
    receiver: mpsc::Receiver<AudioCommand>,
    mut manager: AudioManager,
    ambient_source: listen::DynSource<SpatialAmbient>,
) {
    let sample_rate = 44100;
    let mut sound_cache: HashMap<SoundDef, StaticSoundData> = HashMap::new();

    // TODO: maybe synthesize white noise continuously to avoid any chance of audible repetition?
    let white_noise = StaticSoundData {
        sample_rate: 44100,
        frames: (0..44100 * 10)
            .map({
                let mut rng = rand::rngs::StdRng::seed_from_u64(0);
                move |_| kira::Frame::from_mono(rng.random())
            })
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
            AudioCommand::Fluff(fluff) => {
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
                    play_fluff(&mut manager, sound)
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

fn play_fluff(manager: &mut AudioManager, sound: StaticSoundData) {
    match manager.play(sound) {
        Ok(_handle) => {}
        Err(PlaySoundError::SoundLimitReached) => {
            log::trace!("sound limit reached");
            // Ignore this, since fluff is inconsequential
        }
        Err(error) => log::error!(
            "Playback error: {error}",
            error = all_is_cubes::util::ErrorChain(&error)
        ),
    }
}

// -------------------------------------------------------------------------------------------------

// TODO: De-boilerplate these listener-to-channel adapters

/// Adapter from [`Listener`] to the audio command channel.
struct FluffListener {
    sender: mpsc::SyncSender<AudioCommand>,
    alive: atomic::AtomicBool,
}
impl FluffListener {
    fn new(sender: mpsc::SyncSender<AudioCommand>) -> Self {
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
                Err(mpsc::TrySendError::Full(_)) => {
                    // If the channel is full, indicating the audio thread is falling behind,
                    // drop the message.
                }
                Err(mpsc::TrySendError::Disconnected(_)) => {
                    self.alive.store(false, atomic::Ordering::Relaxed);
                    return false;
                }
            }
        }
        true
    }
}

// -------------------------------------------------------------------------------------------------

/// Adapter from [`Source`] to the audio command channel.
struct UpdateAmbientListener {
    sender: mpsc::SyncSender<AudioCommand>,
    alive: atomic::AtomicBool,
}
impl UpdateAmbientListener {
    fn new(sender: mpsc::SyncSender<AudioCommand>) -> Self {
        Self {
            sender,
            alive: atomic::AtomicBool::new(true),
        }
    }
}

impl fmt::Debug for UpdateAmbientListener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UpdateAmbientListener")
            .field("alive", &self.alive)
            .finish_non_exhaustive()
    }
}

impl listen::Listener<()> for UpdateAmbientListener {
    fn receive(&self, messages: &[()]) -> bool {
        if !self.alive.load(atomic::Ordering::Relaxed) {
            return false;
        }
        if messages.is_empty() {
            return true;
        }
        match self.sender.try_send(AudioCommand::UpdateAmbient) {
            Ok(()) => {}
            Err(mpsc::TrySendError::Full(_)) => {}
            Err(mpsc::TrySendError::Disconnected(_)) => {
                self.alive.store(false, atomic::Ordering::Relaxed);
                return false;
            }
        }
        true
    }
}

// -------------------------------------------------------------------------------------------------

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
