use bevy::prelude::*;

use crate::class::brush::{door::DoorPlugin, portal::PortalPlugin};

pub mod door;
pub mod portal;

pub struct BrushPlugin;

impl Plugin for BrushPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((DoorPlugin, PortalPlugin));
    }
}
