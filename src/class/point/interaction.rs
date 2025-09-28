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
            .add_systems(PostUpdate, spawn_quick_interaction_hints);
    }
}

#[derive(Component)]
#[relationship(relationship_target = QuickInteractUIElementChild)]
pub struct QuickInteractUIElementChildOf(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = QuickInteractUIElementChildOf)]
pub struct QuickInteractUIElementChild(Entity);

#[point_class(base(Target), classname("info_interact"))]
#[derive(SmartDefault)]
pub struct InteractionPointClass {
    /// How far away the player should be able to interact with this entity with.
    #[default(DEFAULT_INTERACTION_RANGE)]
    range: f32,
}

#[point_class(base(Target), classname("info_interact_quick"))]
#[derive(SmartDefault)]
pub struct QuickInteractionPointClass {
    /// How far away the player should be able to interact with this entity with.
    #[default(DEFAULT_INTERACTION_RANGE)]
    range: f32,
}

pub fn spawn_quick_interaction_hints(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    quick_interactables: Populated<(
        Entity,
        &Transform,
        &QuickInteractionPointClass,
        Option<&QuickInteractUIElementChild>,
    )>,
    player: Single<&Transform, (Changed<Transform>, With<PlayerControllerMarker>)>,
) {
    let player_transform = player.into_inner();

    for (entity, transform, quick_interactable, ui_element) in quick_interactables.iter() {
        if ui_element.is_none()
            && (transform.translation - player_transform.translation).length()
                <= quick_interactable.range
        {
            commands.spawn((
                ImageNode {
                    image: asset_server.load("textures/ui/open_hand.png"),
                    color: Color::WHITE,
                    ..default()
                },
                Node {
                    position_type: PositionType::Absolute,
                    aspect_ratio: Some(1.0),
                    min_height: Val::Percent(10.),
                    ..default()
                },
                QuickInteractUIElementChildOf(entity),
            ));
        } else if ui_element.is_some()
            && (transform.translation - player_transform.translation).length()
                > quick_interactable.range
        {
            commands.entity(ui_element.unwrap().0).despawn();
        }
    }
}
