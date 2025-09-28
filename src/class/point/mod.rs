use bevy::prelude::*;

use crate::class::point::interaction::CarrotInteractPlugin;

pub mod interaction;

pub struct PointPlugin;

impl Plugin for PointPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(CarrotInteractPlugin);
    }
}
