use bevy::prelude::*;

pub struct CarrotInteractionPlugin;

impl Plugin for CarrotInteractionPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<InteractableMarker>()
            .register_type::<QuickInteractableMarker>()
            .add_systems(Update, quick_interaction_hints);
    }
}

#[derive(Component, Default, Reflect)]
#[reflect(Component)]
pub struct InteractableMarker;

#[derive(Component, Default, Reflect)]
#[reflect(Component)]
#[require(InteractableMarker)]
pub struct QuickInteractableMarker;

pub fn quick_interaction_hints(
    mut commands: Commands,
    quick_interactables: Populated<&Transform, With<QuickInteractableMarker>>,
) {
}
