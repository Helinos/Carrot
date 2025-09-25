use bevy::prelude::*;
use bevy_trenchbroom::prelude::*;
use nil::prelude::SmartDefault;

use crate::player::PlayerControllerMarker;

pub struct CarrotInteractionPlugin;

const DEFAULT_INTERACTION_RANGE: f32 = 1.5;

impl Plugin for CarrotInteractionPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<InteractionPointClass>()
            .register_type::<QuickInteractionPointClass>()
            .add_systems(Update, quick_interaction_hints);
    }
}

#[point_class(base(Target), classname("info_interact"))]
#[derive(SmartDefault)]
pub struct InteractionPointClass {
    /// The target_name of the entity that will be trigged when this entity is interacted with.
    #[class(must_set)]
    target: String,
    /// How far away the player should be able to interact with this entity with.
    #[default(DEFAULT_INTERACTION_RANGE)]
    range: f32,
}

#[point_class(base(Target), classname("info_interact_quick"))]
#[derive(SmartDefault)]
pub struct QuickInteractionPointClass {
    /// The target_name of the entity that will be trigged when this entity is interacted with.
    #[class(must_set)]
    target: String,
    /// How far away the player should be able to interact with this entity with.
    #[default(DEFAULT_INTERACTION_RANGE)]
    range: f32,
}

pub fn quick_interaction_hints(
    mut commands: Commands,
    quick_interactables: Populated<(&Transform, &QuickInteractionPointClass)>,
    player: Single<&Transform, (Changed<Transform>, With<PlayerControllerMarker>)>,
) {
    let player_transform = player.into_inner();

    for (quick_interactable_transform, quick_interactable) in quick_interactables.iter() {
        if (quick_interactable_transform.translation - player_transform.translation).length()
            <= quick_interactable.range
        {
            info_once!("Fired");
        }
    }
}
