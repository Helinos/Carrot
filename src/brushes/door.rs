use bevy::prelude::*;
use bevy_trenchbroom::prelude::*;

pub struct DoorPlugin;

impl Plugin for DoorPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<SlidingDoorClass>();
    }
}

#[derive(Component)]
pub struct DoorOpeningMarker;

#[derive(Component)]
pub struct DoorClosingMarker;

#[solid_class(classname("func_sliding_door"))]
pub struct SlidingDoorClass;
