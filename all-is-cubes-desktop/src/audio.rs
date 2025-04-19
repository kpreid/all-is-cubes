use std::fmt;
use std::sync::{atomic, mpsc};

use hashbrown::HashMap;
use kira::AudioManager;
use kira::PlaySoundError;
use kira::sound::static_sound::StaticSoundData;

use all_is_cubes::fluff::Fluff;
use all_is_cubes::listen::Listener;
use all_is_cubes::sound::SoundDef;

use crate::Session;

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
}

pub(crate) fn init_sound(session: &Session) -> Result<AudioOut, anyhow::Error> {
    let manager = AudioManager::<kira::backend::cpal::CpalBackend>::new(
        kira::AudioManagerSettings::default(),
    )?;

    // Transfers audio events to the audio processing thread.
    // We never block on this channel, but drop things if full.
    let (sender, receiver) = mpsc::sync_channel(256);

    // The audio processing is done on a thread because `AudioManager` wants `&mut self`
    // to perform operations, so I either need a dedicated thread (actor) or a mutex.
    // We also don’t want to find ourselves running synthesis inside the Listener.
    // Note that this is *not* a sample-by-sample realtime audio thread.
    std::thread::Builder::new()
        .name("all_is_cubes_audio".to_owned())
        .spawn(move || audio_command_thread(receiver, manager))
        .unwrap();

    // Hook up fluff channel
    session.listen_fluff(FluffListener::new(sender.clone()));

    Ok(AudioOut { sender })
}

/// Thread function for receiving commands and executing them on `&mut AudioManager`.
/// This is not a real-time audio thread.
#[expect(clippy::needless_pass_by_value)]
fn audio_command_thread(receiver: mpsc::Receiver<AudioCommand>, mut manager: AudioManager) {
    let sample_rate = 44100;
    let mut sound_cache: HashMap<SoundDef, StaticSoundData> = HashMap::new();

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

                    sound.settings.volume = kira::Decibels(amplitude.log10() * 20.0).into();
                    play_fluff(&mut manager, sound)
                }
            }
        }
    }
}

fn play_fluff(manager: &mut AudioManager, sound: StaticSoundData) {
    match manager.play(sound) {
        Ok(_handle) => {}
        Err(PlaySoundError::SoundLimitReached) => {
            // Ignore this, since fluff is inconsequential
        }
        Err(error) => log::error!(
            "Playback error: {error}",
            error = all_is_cubes::util::ErrorChain(&error)
        ),
    }
}

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

impl Listener<Fluff> for FluffListener {
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
