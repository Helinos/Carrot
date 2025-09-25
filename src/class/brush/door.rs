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

#[solid_class(base(Targetable, Transform), classname("func_sliding_door"))]
#[derive(Clone, Default)]
pub struct SlidingDoorClass {
    /// How far the door should travel when opened.
    #[class(must_set)]
    distance: Option<f32>,
}
