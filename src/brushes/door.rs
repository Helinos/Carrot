use std::default;

use bevy::prelude::*;
use bevy_trenchbroom::prelude::*;
use nil::prelude::SmartDefault;

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
#[derive(Clone, SmartDefault)]
pub struct SlidingDoorClass {
    // #[class(must_set)]
    // #[default(vec3(0., 0., 0.))]
    // direction: Vec3,
    // #[class(must_set)]
    // #[default(0.0)]
    // distance: f32,
}
