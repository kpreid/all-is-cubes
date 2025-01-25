use std::f32::consts::PI;
use std::fmt;
use std::sync::{atomic, mpsc};

use all_is_cubes::fluff::Fluff;
use all_is_cubes::listen::Listener;

use kira::sound::static_sound::StaticSoundData;
use kira::AudioManager;
use kira::PlaySoundError;

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

    let (sender, receiver) = mpsc::sync_channel(256);

    // The audio processing is done on a thread because `AudioManager` wants `&mut self`
    // to perform operations, so I either need a dedicated thread (actor) or a mutex.
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
#[expect(clippy::needless_pass_by_value)]
fn audio_command_thread(receiver: mpsc::Receiver<AudioCommand>, mut manager: AudioManager) {
    // TODO: better sound and more sounds
    let beep = StaticSoundData {
        sample_rate: 44100,
        frames: (0..2205)
            .map(|i| {
                let wave = (i as f32 / 44.1 * 4.0).sin() * 0.1;
                kira::Frame {
                    left: wave,
                    right: wave,
                }
            })
            .collect(),

        settings: kira::sound::static_sound::StaticSoundSettings::default(),
        slice: None,
    };
    let happened = StaticSoundData {
        sample_rate: 44100,
        frames: (0..220)
            .map(|i| {
                let wave = (i as f32 / 44.1 * 2.0).sin() * 0.1;
                kira::Frame {
                    left: wave,
                    right: wave,
                }
            })
            .collect(),

        settings: kira::sound::static_sound::StaticSoundSettings::default(),
        slice: None,
    };
    let thump = StaticSoundData {
        sample_rate: 44100,
        frames: (0..440)
            .map(|i| {
                let envelope = ((i as f32 / 440. * PI).sin() + 1.0) / 2.0;
                let wave = (i as f32 / 44.1 * 0.25).sin() * envelope;
                kira::Frame {
                    left: wave,
                    right: wave,
                }
            })
            .collect(),

        settings: kira::sound::static_sound::StaticSoundSettings::default(),
        slice: None,
    };

    while let Ok(message) = receiver.recv() {
        match message {
            AudioCommand::Fluff(fluff) => match fluff {
                Fluff::Beep => play_fluff(&mut manager, beep.clone()),
                Fluff::Happened | Fluff::PlaceBlockGeneric => {
                    play_fluff(&mut manager, happened.clone())
                }
                Fluff::BlockImpact { velocity, .. } => {
                    let velocity: f32 = velocity.into_inner();

                    let amplitude = (velocity * 0.01).clamp(0.0, 1.0);

                    let mut sound = thump.clone();
                    sound.settings.volume = kira::Decibels(amplitude.log10() * 20.0).into();
                    play_fluff(&mut manager, sound)
                }
                Fluff::BlockFault(_) => {}
                f => log::debug!("No known sound for Fluff value: {f:?}"),
            },
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

/// Adapter from [`Listener`] to the audio thread channel.
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
                Err(mpsc::TrySendError::Full(_)) => {}
                Err(mpsc::TrySendError::Disconnected(_)) => {
                    self.alive.store(false, atomic::Ordering::Relaxed);
                    return false;
                }
            }
        }
        true
    }
}
