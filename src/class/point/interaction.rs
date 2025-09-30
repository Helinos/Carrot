use bevy::{prelude::*, render::camera::ViewportConversionError};
use bevy_trenchbroom::prelude::*;
use itertools::Itertools;
use leafwing_input_manager::prelude::*;
use nil::prelude::SmartDefault;

use crate::{
    InputAction,
    class::{Busy, TargetEvent, Targeting},
    player::{PlayerControllerMarker, PlayerWorldCameraMarker},
};

pub struct CarrotInteractPlugin;

const DEFAULT_INTERACTION_RANGE: f32 = 3.0;

impl Plugin for CarrotInteractPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<InteractPointClass>()
            .register_type::<QuickInteractPointClass>()
            .add_systems(Update, (update_quick_interact_ui_position, interact))
            .add_systems(PostUpdate, spawn_despawn_quick_interact_ui);
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
pub struct InteractPointClass {
    /// How far away the player should be able to interact with this entity with.
    #[default(DEFAULT_INTERACTION_RANGE)]
    range: f32,
}

#[point_class(base(Target), classname("info_interact_quick"))]
#[derive(SmartDefault)]
pub struct QuickInteractPointClass {
    /// How far away the player should be able to interact with this entity with.
    #[default(DEFAULT_INTERACTION_RANGE)]
    range: f32,
}

pub fn spawn_despawn_quick_interact_ui(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    quick_interact_points: Populated<(
        Entity,
        &Transform,
        &QuickInteractPointClass,
        Option<&QuickInteractUIElementChild>,
    )>,
    player: Single<&Transform, (Changed<Transform>, With<PlayerControllerMarker>)>,
) {
    let player_transform = player.into_inner();

    for (entity, transform, quick_interact_class, ui_element) in quick_interact_points.iter() {
        if ui_element.is_none()
            && (transform.translation - player_transform.translation).length()
                <= quick_interact_class.range
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
                > quick_interact_class.range
        {
            commands.entity(ui_element.unwrap().0).despawn();
        }
    }
}

pub fn update_quick_interact_ui_position(
    mut ui_elements: Populated<(&QuickInteractUIElementChildOf, &mut Node)>,
    player_camera: Single<(&Camera, &GlobalTransform), With<PlayerWorldCameraMarker>>,
    quick_interact_points: Populated<
        &Transform,
        (
            With<QuickInteractUIElementChild>,
            With<QuickInteractPointClass>,
        ),
    >,
) -> Result {
    let (camera, camera_transform) = player_camera.into_inner();

    for (ui_element_child_of, mut ui_element_node) in ui_elements.iter_mut() {
        let interact_point_transform = quick_interact_points.get(ui_element_child_of.0)?;

        // Copied from `bevy_render/camera/camera.rs` and modified.
        fn world_to_viewport_edge_clamped(
            camera: &Camera,
            camera_transform: &GlobalTransform,
            world_position: Vec3,
        ) -> Result<Vec2, ViewportConversionError> {
            let target_rect = camera
                .logical_viewport_rect()
                .ok_or(ViewportConversionError::NoViewportSize)?;
            let ndc_space_coords = camera
                .world_to_ndc(camera_transform, world_position)
                .ok_or(ViewportConversionError::InvalidData)?;

            let behind_view = ndc_space_coords.z < 0.0;

            let mut ndc_xy = if behind_view {
                -ndc_space_coords.truncate()
            } else {
                ndc_space_coords.truncate()
            };

            let within_ndc = ndc_xy.x.abs() <= 1.0 && ndc_xy.y.abs() <= 1.0;

            let ndc_position = if !behind_view && within_ndc {
                ndc_xy
            } else {
                // If the world position is directly behind the camera (0, 0) then pick some sensible default direction.
                if ndc_xy.length_squared() <= f32::EPSILON {
                    ndc_xy = Vec2::NEG_Y; // Top of the screen
                }

                // Calculate the scale that allows either the x or y coordinate to be at +/-1.0 (Intersect with the NDC box).
                let scale = 1.0 / ndc_xy.x.abs().max(ndc_xy.y.abs());
                ndc_xy *= scale;
                ndc_xy
            };

            let viewport_position = (Vec2::new(ndc_position.x, -ndc_position.y) + Vec2::ONE) / 2.0
                * target_rect.size()
                + target_rect.min * target_rect.size()
                + target_rect.min;
            Ok(viewport_position)
        }

        let viewport_translation = world_to_viewport_edge_clamped(
            camera,
            camera_transform,
            interact_point_transform.translation,
        )?;
        let viewport_size = camera.physical_viewport_size().unwrap().as_vec2();
        let ui_element_side_length = ui_element_node
            .min_height
            .resolve(viewport_size.y, viewport_size)?;
        let position = viewport_translation - ui_element_side_length / 2.0;
        let max = viewport_size - Vec2::splat(ui_element_side_length);

        ui_element_node.left = Val::Px(position.x.clamp(0.0, max.x));
        ui_element_node.top = Val::Px(position.y.clamp(0.0, max.y));
    }

    Ok(())
}

pub fn interact(
    mut commands: Commands,
    action_state: Single<&ActionState<InputAction>>,
    quick_interact_points: Populated<
        &Targeting,
        (
            With<QuickInteractUIElementChild>, // This counts as a range check
            With<QuickInteractPointClass>,
        ),
    >,
    targetables: Populated<Entity, (With<Targetable>, Without<Busy>)>,
) {
    if action_state.just_pressed(&InputAction::Interact) {
        let targets = quick_interact_points
            .iter()
            .filter_map(|targeting| targetables.get(targeting.0).ok())
            .collect_vec();

        commands.trigger_targets(TargetEvent, targets);
    }
}
