#![allow(dead_code, reason = "TODO: Implement `Script` recording and playback")]

use std::fs;
use std::io;
use std::ops;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;

use all_is_cubes::character::{self, Character};
use all_is_cubes::euclid::{Point3D, Vector3D, num::One, num::Zero as _};
use all_is_cubes::listen;
use all_is_cubes::math::{Cube, NotNan};
use all_is_cubes::physics::BodyTransaction;
use all_is_cubes::transaction::Merge as _;
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::{behavior, universe};
use all_is_cubes_render::{
    Flaws,
    camera::{Layers, StandardCameras},
};

// -------------------------------------------------------------------------------------------------

/// Definition of an animation to make a [`Character`] follow a predefined path.
#[derive(Clone, Debug, Default, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
pub struct Script {
    // TODO: stop depending on euclid’s serde formats
    position: Data<Point3D<NotNan<f64>, Cube>>,
    look_direction: Data<Vector3D<NotNan<f64>, Cube>>,
}

/// Behavior attached to a [`Character`] which executes a [`Script`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Performance {
    script: Arc<Script>,
    // time is kept as Duration rather than f64 to avoid accumulating rounding error
    time: Duration,
}

// -------------------------------------------------------------------------------------------------

impl Script {
    /// Read the specified JSON script file.
    ///
    /// TODO: document format
    pub fn from_file(path: &std::path::Path) -> Result<Arc<Self>, anyhow::Error> {
        let file = fs::File::open(path)
            .with_context(|| format!("failed to open script file {}", path.display()))?;

        Ok(Arc::new(serde_json::from_reader(io::BufReader::new(file))?))
    }

    pub(crate) fn installation(self: Arc<Self>) -> character::CharacterTransaction {
        character::CharacterTransaction::behaviors(behavior::BehaviorSetTransaction::insert(
            (),
            Arc::new(Performance {
                script: self,
                time: Duration::ZERO,
            }),
        ))
    }

    fn add_frame(
        &mut self,
        time_since_start: Duration,
        position: Point3D<NotNan<f64>, Cube>,
        look_direction: Vector3D<NotNan<f64>, Cube>,
    ) {
        self.position.push(time_since_start, position);
        self.look_direction.push(time_since_start, look_direction);
    }
}

// -------------------------------------------------------------------------------------------------

impl behavior::Behavior<Character> for Performance {
    fn step(
        &self,
        context: &behavior::Context<'_, Character>,
    ) -> (universe::UniverseTransaction, behavior::Then) {
        let mut body_txn = BodyTransaction::default();
        if let Some(position) = self.script.position.get(self.time) {
            body_txn = body_txn.with_position(position.map(NotNan::into_inner));
        }
        if let Some(look_direction) = self.script.look_direction.get(self.time) {
            body_txn = body_txn.with_look_direction(look_direction.map(NotNan::into_inner));
        }

        let new_self = Performance {
            time: self.time + context.tick.delta_t(),
            script: self.script.clone(),
        };

        (
            context
                .bind_host(character::CharacterTransaction::body(body_txn))
                .merge(context.replace_self(new_self))
                .unwrap(),
            behavior::Then::Step,
        )
    }

    fn persistence(&self) -> Option<behavior::Persistence> {
        None
    }
}

impl universe::VisitHandles for Performance {
    // No handles
    fn visit_handles(&self, _visitor: &mut dyn universe::HandleVisitor) {}
}

// -------------------------------------------------------------------------------------------------

/// glTF-style animation data: sequence of key frames, each of which has a timestamp and a value.
///
/// TODO: enforce there is at least one element?
#[derive(Clone, Debug, Default, Eq, PartialEq, serde::Deserialize, serde::Serialize)] // TODO: customize deserialization to enforce well-formedness
struct Data<T> {
    times: Vec<NotNan<f32>>,
    values: Vec<T>,
}
impl<T> Data<T>
where
    T: Copy + Lerp,
    T::Mix: From<NotNan<f32>>,
{
    /// Returns None if and only if the data is empty.
    fn get(&self, at_time: Duration) -> Option<T> {
        let at_time = NotNan::new(at_time.as_secs_f32()).unwrap();
        let partition = self.times.partition_point(|&t| t <= at_time);
        if partition == self.times.len() {
            self.values.last().copied()
        } else if partition == 0 {
            self.values.first().copied()
        } else {
            let t_before = self.times[partition - 1];
            let t_after = self.times[partition];
            let t_range_size = t_after - t_before;
            let interpolation: NotNan<f32> = if t_range_size > NotNan::zero() {
                ((at_time - t_before) / t_range_size).clamp(NotNan::zero(), NotNan::one())
            } else {
                // zero-length spans are ties broken in favor of the later value
                NotNan::one()
            };

            let val_before = self.values[partition - 1];
            let val_after = self.values[partition];
            Some(Lerp::lerp(val_before, val_after, interpolation.into()))
        }
    }

    /// Append data. `at_time` must be newer than any previous time.
    fn push(&mut self, at_time: Duration, value: T) {
        let at_time = NotNan::new(at_time.as_secs_f32()).unwrap();
        self.times.push(at_time);
        self.values.push(value);
    }
}

trait Lerp {
    type Mix;
    fn lerp(a: Self, b: Self, mix: Self::Mix) -> Self;
}
impl<T: Copy + One + ops::Sub<Output = T> + ops::Mul<Output = T> + ops::Add<Output = T>, U> Lerp
    for Vector3D<T, U>
{
    type Mix = T;
    fn lerp(a: Self, b: Self, mix: Self::Mix) -> Self {
        Self::lerp(a, b, mix)
    }
}
impl<T: Copy + One + ops::Sub<Output = T> + ops::Mul<Output = T> + ops::Add<Output = T>, U> Lerp
    for Point3D<T, U>
{
    type Mix = T;
    fn lerp(a: Self, b: Self, mix: Self::Mix) -> Self {
        Self::lerp(a, b, mix)
    }
}

// -------------------------------------------------------------------------------------------------

/// Capture a [`Script`] from player actions.
#[derive(Debug)]
pub(crate) struct ScriptRecorder {
    cameras: StandardCameras,
    script: Script,
    path_to_write: PathBuf,
    status_notifier: Arc<listen::Notifier<super::Status>>,
}
impl ScriptRecorder {
    pub(crate) fn new(
        cameras: StandardCameras,
        path_to_write: PathBuf,
        status_notifier: Arc<listen::Notifier<super::Status>>,
    ) -> Self {
        Self {
            cameras,
            path_to_write,
            script: Script::default(),
            status_notifier,
        }
    }

    pub(crate) fn capture_frame(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        this_frame_number: usize,
        time_since_start: Duration,
    ) {
        self.cameras.update(read_tickets);
        let Some(character) = self.cameras.character() else {
            return;
        };
        let character = character.read(read_tickets.world).unwrap();
        self.script.add_frame(
            time_since_start,
            character.body().position().cast(),
            character.body().look_direction().cast(),
        );

        // Script recording is trivial so we can immediately notify success
        self.status_notifier.notify(&super::Status {
            frame_number: this_frame_number,
            flaws: Flaws::empty(),
        });
    }
}

// TODO: we need an explicit finish-recording operation for better error reporting
impl Drop for ScriptRecorder {
    fn drop(&mut self) {
        serde_json::to_writer(fs::File::create(&self.path_to_write).unwrap(), &self.script)
            .unwrap();
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::vec3;
    use pretty_assertions::assert_eq;

    type TestVector = Vector3D<f32, ()>;

    #[test]
    fn interpolation() {
        let mut data = Data::<TestVector>::default();
        data.push(Duration::from_secs(10), vec3(1.0, 0.0, 0.0));
        data.push(Duration::from_secs(20), vec3(0.0, 1.0, 0.0));
        data.push(Duration::from_secs(40), vec3(0.0, 0.0, 1.0));

        assert_eq!(
            (0..10)
                .map(|i| data.get(Duration::from_secs(i * 5)).unwrap())
                .collect::<Vec<TestVector>>(),
            vec![
                vec3(1.0, 0.0, 0.0),
                vec3(1.0, 0.0, 0.0),
                vec3(1.0, 0.0, 0.0),
                vec3(0.5, 0.5, 0.0),
                vec3(0.0, 1.0, 0.0),
                vec3(0.0, 0.75, 0.25),
                vec3(0.0, 0.5, 0.5),
                vec3(0.0, 0.25, 0.75),
                vec3(0.0, 0.0, 1.0),
                vec3(0.0, 0.0, 1.0),
            ]
        );
    }
}
