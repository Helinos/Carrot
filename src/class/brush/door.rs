use bevy::prelude::*;
use bevy_trenchbroom::prelude::*;
use nestify::nest;
use nil::prelude::SmartDefault;

use crate::class::{Busy, TargetEvent};

pub struct DoorPlugin;

impl Plugin for DoorPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<SlidingDoorClass>()
            .add_systems(Update, while_busy)
            .add_observer(on_trigger);
    }
}

nest! {
    #[solid_class(base(Targetable, Transform), classname("func_sliding_door"))]
    #[derive(Clone, SmartDefault)]
    pub struct SlidingDoorClass {
        /// How far the door will travel in map units when opened.
        #[class(must_set)]
        distance: f32,
        /// How many map units per second the door will travel.
        #[default(50.0)]
        speed: f32,
        #[class(ignore)]
        #>[derive(Default, Clone, Reflect)]
        state: enum DoorState {
            #[default]
            Opening,
            Closing,
        },
    }
}

pub fn on_trigger(
    trigger: Trigger<TargetEvent>,
    mut commands: Commands,
    doors: Populated<Entity, (With<SlidingDoorClass>, Without<Busy>)>,
) -> Result {
    let target = trigger.target();
    let Ok(door_ent) = doors.get(target) else {
        return Ok(());
    };

    commands.entity(door_ent).insert(Busy);

    Ok(())
}

pub fn while_busy(
    mut commands: Commands,
    time: Res<Time>,
    tb_sever: Res<TrenchBroomServer>,
    mut doors: Populated<(Entity, &mut Transform, &mut SlidingDoorClass), With<Busy>>,
) {
    for (entity, mut transform, mut door) in doors.iter_mut() {
        let direction: Vec3 = match door.state {
            DoorState::Opening => *transform.forward(),
            DoorState::Closing => *transform.back(),
        };

        let max_distance = door.distance / tb_sever.config.scale;
        let mut translation = transform.translation;
        translation += direction * door.speed / tb_sever.config.scale * time.delta_secs();
        // The length of the new translation can have a length that comes up short of max_distance;
        transform.translation = translation.clamp_length_max(max_distance + f32::EPSILON);
        let distance = transform.translation.dot(*transform.forward());

        if distance >= max_distance - 0.001 || distance <= 0.0 {
            commands.entity(entity).remove::<Busy>();
            let new_state = match door.state {
                DoorState::Opening => DoorState::Closing,
                DoorState::Closing => DoorState::Opening,
            };
            door.state = new_state;
        }
    }
}
