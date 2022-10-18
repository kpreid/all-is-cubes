use std::fmt;
use std::sync::{atomic, mpsc, Arc};

use all_is_cubes::apps::Session;
use all_is_cubes::fluff::Fluff;
use all_is_cubes::listen::Listener;
use kira::manager::AudioManager;
use kira::sound::static_sound::StaticSoundData;

pub(crate) struct AudioOut {
    #[allow(dead_code)] // eventually we're going to need this for volume control etc.
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
    let manager = AudioManager::<kira::manager::backend::cpal::CpalBackend>::new(
        kira::manager::AudioManagerSettings::default(),
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
fn audio_command_thread(receiver: mpsc::Receiver<AudioCommand>, mut manager: AudioManager) {
    // TODO: better sound and more sounds
    let beep = StaticSoundData {
        sample_rate: 44100,
        frames: Arc::new(
            (0..2205)
                .map(|i| {
                    let wave = (i as f32 / 44.1 * 4.0).sin() * 0.1;
                    kira::dsp::Frame {
                        left: wave,
                        right: wave,
                    }
                })
                .collect(),
        ),
        settings: kira::sound::static_sound::StaticSoundSettings::default(),
    };

    while let Ok(message) = receiver.recv() {
        match message {
            AudioCommand::Fluff(Fluff::Beep) => match manager.play(beep.clone()) {
                Ok(_) => {}
                Err(e) => log::error!("Playback error: {e}"),
            },
            AudioCommand::Fluff(f) => log::debug!("No known sound for Fluff value: {f:?}"),
        }
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

impl Listener<Fluff> for FluffListener {
    fn receive(&self, fluff: Fluff) {
        match self.sender.try_send(AudioCommand::Fluff(fluff)) {
            Ok(()) => {}
            Err(mpsc::TrySendError::Full(_)) => {}
            Err(mpsc::TrySendError::Disconnected(_)) => {
                self.alive.store(false, atomic::Ordering::Relaxed);
            }
        }
    }

    fn alive(&self) -> bool {
        self.alive.load(atomic::Ordering::Relaxed)
    }
}
