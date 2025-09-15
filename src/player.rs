use std::f32::consts::FRAC_PI_2;

use bevy::{
    input::mouse::AccumulatedMouseMotion, prelude::*, render::view::RenderLayers,
    scene::SceneInstanceReady,
};
use bevy_rapier3d::prelude::*;
use bevy_trenchbroom::class::builtin::InfoPlayerStart;
use nestify::nest;

use crate::{DEFAULT_RENDER_LAYER, PORTAL_RENDER_LAYER_1, PORTAL_RENDER_LAYER_2};

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<MovementAction>()
            .init_resource::<FOV>()
            .add_systems(PreUpdate, (mouse_input, keyboard_input))
            .add_systems(Update, (headbob_effect, look, movement.after(look)))
            .add_observer(spawn_player);
    }
}

#[derive(Event)]
pub enum MovementAction {
    Move {
        input_direction: Vec2,
        movement_speed: f32,
    },
    Jump,
    Look {
        delta: Vec2,
    },
}

#[derive(Resource)]
pub enum FOV {
    horizontal_degrees_4x3(f32),
    horizontal_degrees_16x9(f32),
    horizontal_degrees_aspect_ratio(f32),
    vertical_degrees(f32),
    horizontal_radians_4x3(f32),
    horizontal_radians_16x9(f32),
    horizontal_radians_aspect_ratio(f32),
    vertical_radians(f32),
}

impl Default for FOV {
    fn default() -> Self {
        Self::horizontal_degrees_aspect_ratio(90.0)
    }
}

impl Into<f32> for &FOV {
    fn into(self) -> f32 {
        match self {
            FOV::horizontal_degrees_4x3(h_deg_4x3) => todo!(),
            FOV::horizontal_degrees_16x9(h_deg_16x9) => todo!(),
            FOV::horizontal_degrees_aspect_ratio(h_deg_ratio) => h_deg_ratio.to_radians(),
            FOV::vertical_degrees(v_deg) => todo!(),
            FOV::horizontal_radians_4x3(h_rad_4x3) => todo!(),
            FOV::horizontal_radians_16x9(h_rad_16x9) => todo!(),
            FOV::horizontal_radians_aspect_ratio(h_rad_ratio) => *h_rad_ratio,
            FOV::vertical_radians(v_rad) => todo!(),
        }
    }
}

nest! {
    #[derive(Component)]
    pub struct CameraSettings {
        pub allow_movement: bool,
        #>[derive(Clone, Copy)]
        pub mouse_sensitivity: pub enum CameraSensitivity {
            Linear(f32),
            Relative(Vec2),
        },
        pub controller_sensitivity: CameraSensitivity,
        pub distance_from_ground: f32,
    }
}

impl Into<Vec2> for CameraSensitivity {
    fn into(self) -> Vec2 {
        match self {
            CameraSensitivity::Linear(linear) => Vec2::splat(linear),
            CameraSensitivity::Relative(relative) => relative,
        }
    }
}

#[derive(Component)]
pub struct JumpSettings {
    impulse: f32,
    auto_bhop: bool,
}

impl Default for JumpSettings {
    fn default() -> Self {
        Self {
            impulse: 6.0,
            auto_bhop: false,
        }
    }
}

#[derive(Component)]
pub struct MovementSettings {
    walking_speed: f32,
    sprinting_speed: Option<f32>,
    crouching_speed: Option<f32>,

    ground_acceleration: f32,
    ground_deceleration: f32,
    ground_friction: f32,

    air_cap: f32,
    air_acceleration: f32,
    air_move_speed: f32,
}

#[derive(Component)]
#[require(HeadbobTime)]
pub struct HeadbobSettings {
    headbob_move_amount: f32,
    headbob_frquency: f32,
}

impl Default for HeadbobSettings {
    fn default() -> Self {
        Self {
            headbob_move_amount: 0.06,
            headbob_frquency: 2.4,
        }
    }
}

#[derive(Component, Default)]
struct HeadbobTime(f32);

#[derive(Component)]
pub struct ControllerAimAcceleration(f32);

impl Default for ControllerAimAcceleration {
    fn default() -> Self {
        Self(5.0)
    }
}

#[derive(Component, Default)]
struct PlayerVelocity(Vec3);

#[derive(Component, Default)]
#[require(
    Transform,
    CameraSettings {
        allow_movement: false,
        mouse_sensitivity: CameraSensitivity::Linear(0.003),
        controller_sensitivity: CameraSensitivity::Linear(0.005),
        distance_from_ground: 1.79,
    },
    MovementSettings {
        walking_speed: 7.0,
        sprinting_speed: Some(8.5),
        crouching_speed: Some(3.5),

        ground_acceleration: 14.0,
        ground_deceleration: 10.0,
        ground_friction: 6.0,

        air_cap: 0.05,
        air_acceleration: 800.0,
        air_move_speed: 500.0,
    },
    RigidBody::KinematicPositionBased,
    PlayerVelocity,
    KinematicCharacterController {
        max_slope_climb_angle: 45_f32.to_radians(),
        autostep: Some(CharacterAutostep {
            max_height: CharacterLength::Absolute(0.3),
            min_width: CharacterLength::Absolute(0.25),
            include_dynamic_bodies: true,
        }),
        custom_shape: Some((Collider::cuboid(0.25, 1.0, 0.25), Vec3::new(0.0, 1.0, 0.0), Quat::default())),
        ..default()
    },
    GravityScale(1.5),
)]

pub struct PlayerControllerMarker;

#[derive(Component)]
pub struct WorldCameraMarker;

fn spawn_player(
    _trigger: Trigger<SceneInstanceReady>,
    mut player_entity_query: Query<
        (Entity, &mut Transform, &CameraSettings),
        With<PlayerControllerMarker>,
    >,
    player_start_query: Populated<
        &Transform,
        (With<InfoPlayerStart>, Without<PlayerControllerMarker>),
    >,
    fov: Res<FOV>,
    mut commands: Commands,
) -> Result {
    let (entity, mut player_transform, camera_settings) = player_entity_query.single_mut()?;

    let transforms: Vec<&Transform> = player_start_query.iter().collect();
    if let Some(player_start_transform) = transforms.get(0) {
        *player_transform = **player_start_transform;
    }

    commands.entity(entity).with_children(|parent| {
        parent.spawn((
            WorldCameraMarker,
            Camera3d::default(),
            Projection::from(PerspectiveProjection {
                fov: fov.into_inner().into(),
                ..default()
            }),
            Transform::from_xyz(0.0, camera_settings.distance_from_ground, 0.0),
            RenderLayers::from_layers(&[
                DEFAULT_RENDER_LAYER,
                PORTAL_RENDER_LAYER_1,
                PORTAL_RENDER_LAYER_2,
            ]),
        ));
    });

    Ok(())
}

fn keyboard_input(
    player_settings: Single<
        (Option<&JumpSettings>, &MovementSettings),
        With<PlayerControllerMarker>,
    >,
    mut movement_event_writer: EventWriter<MovementAction>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
) {
    let (jump_settings, movement_settings) = player_settings.into_inner();

    let forward = keyboard_input.pressed(KeyCode::KeyW);
    let backward = keyboard_input.pressed(KeyCode::KeyS);
    let left = keyboard_input.pressed(KeyCode::KeyA);
    let right = keyboard_input.pressed(KeyCode::KeyD);

    let lateral = right as i8 - left as i8;
    let longitudinal = backward as i8 - forward as i8;
    let input_direction = Vec2::new(lateral as f32, longitudinal as f32).normalize_or_zero();

    if input_direction != Vec2::ZERO {
        movement_event_writer.write(MovementAction::Move {
            input_direction,
            movement_speed: if let Some(sprinting_speed) = movement_settings.sprinting_speed
                && keyboard_input.pressed(KeyCode::ShiftLeft)
            {
                sprinting_speed
            } else {
                movement_settings.walking_speed
            },
        });
    }

    if let Some(jump_settings) = jump_settings {
        if keyboard_input.just_pressed(KeyCode::Space)
            || (jump_settings.auto_bhop && keyboard_input.pressed(KeyCode::Space))
        {
            movement_event_writer.write(MovementAction::Jump);
        }
    }
}

fn mouse_input(
    camera_settings: Single<&CameraSettings>,
    accumulated_mouse_motion: Res<AccumulatedMouseMotion>,
    mut movement_event_writer: EventWriter<MovementAction>,
) {
    let delta = accumulated_mouse_motion.delta;
    if delta != Vec2::ZERO && camera_settings.allow_movement {
        let sensitivity: Vec2 = camera_settings.mouse_sensitivity.into();
        let delta = -delta * sensitivity;

        movement_event_writer.write(MovementAction::Look { delta });
    }
}

fn look(
    player_query: Single<&mut Transform, (With<PlayerControllerMarker>, Without<ChildOf>)>,
    camera_query: Single<&mut Transform, (With<Camera3d>, With<ChildOf>, With<WorldCameraMarker>)>,
    mut movement_event_reader: EventReader<MovementAction>,
) {
    let mut player_transform = player_query.into_inner();
    let mut camera_transform = camera_query.into_inner();

    for event in movement_event_reader.read() {
        match event {
            MovementAction::Look { delta } => {
                let (_, camera_pitch, _) = camera_transform.rotation.to_euler(EulerRot::YXZ);

                const PITCH_LIMIT: f32 = FRAC_PI_2 - 0.01;
                let clamped_delta =
                    (camera_pitch + delta.y).clamp(-PITCH_LIMIT, PITCH_LIMIT) - camera_pitch;

                player_transform.rotate_local_y(delta.x);
                camera_transform.rotate_local_x(clamped_delta);
            }
            _ => (),
        }
    }
}

fn movement(
    time: Res<Time>,
    mut movement_event_reader: EventReader<MovementAction>,
    mut query: Query<
        (
            &Transform,
            &mut PlayerVelocity,
            &mut KinematicCharacterController,
            Option<&KinematicCharacterControllerOutput>,
            Option<&JumpSettings>,
            &MovementSettings,
            &GravityScale,
        ),
        With<PlayerControllerMarker>,
    >,
) -> Result {
    let delta_time = time.delta_secs();
    let (
        transform,
        mut velocity,
        mut controller,
        output,
        jump_settings,
        movement_settings,
        gravity_scale,
    ) = query.single_mut()?;

    let vertical_velocity = &mut velocity.0.y;
    let is_grounded = output.map_or(false, |o| o.grounded && *vertical_velocity <= 0.0);

    // Ground movement
    if is_grounded {
        *vertical_velocity = 0.0;

        for event in movement_event_reader.read() {
            match event {
                MovementAction::Move {
                    input_direction,
                    movement_speed,
                } => {
                    let wish_direction = transform
                        .rotation
                        .mul_vec3(input_direction.extend(0.0).xzy());
                    let current_speed_in_wish_direction = velocity.0.dot(wish_direction);
                    let add_speed_till_cap = movement_speed - current_speed_in_wish_direction;
                    if add_speed_till_cap > 0.0 {
                        let mut acceleration =
                            movement_settings.ground_acceleration * movement_speed * delta_time;
                        acceleration = acceleration.min(add_speed_till_cap);

                        velocity.0 += acceleration * wish_direction;
                    }
                }
                MovementAction::Jump => {
                    if let Some(jump_settings) = jump_settings {
                        velocity.0.y = jump_settings.impulse;
                    }
                }
                _ => (),
            }
        }

        let lateral_velocity = velocity.0.xz().length();
        let deceleration_force = lateral_velocity.max(movement_settings.ground_deceleration);
        let deceleration_velocity =
            deceleration_force * movement_settings.ground_friction * delta_time as f32;
        let mut new_speed = (lateral_velocity - deceleration_velocity).max(0.0);
        if lateral_velocity > 0.0 {
            new_speed /= lateral_velocity;
        }

        velocity.0.x *= new_speed;
        velocity.0.z *= new_speed;
    }
    // Air movement
    else {
        *vertical_velocity -= 9.8 * gravity_scale.0 * delta_time;

        for event in movement_event_reader.read() {
            match event {
                MovementAction::Move {
                    input_direction,
                    movement_speed: _,
                } => {
                    let wish_direction = transform
                        .rotation
                        .mul_vec3(input_direction.extend(0.0).xzy());
                    let current_speed_in_wish_direction = velocity.0.dot(wish_direction);
                    let capped_speed = (movement_settings.air_move_speed * wish_direction)
                        .length()
                        .min(movement_settings.air_cap);
                    let add_speed_till_cap = capped_speed - current_speed_in_wish_direction;
                    if add_speed_till_cap > 0.0 {
                        let mut acceleration_speed = movement_settings.air_acceleration
                            * movement_settings.air_move_speed
                            * delta_time;
                        acceleration_speed = acceleration_speed.min(add_speed_till_cap);

                        velocity.0 += acceleration_speed * wish_direction;
                    }
                }
                _ => (),
            }
        }
    }

    controller.translation = Some(velocity.0 * delta_time);

    Ok(())
}

fn headbob_effect(
    time: Res<Time>,
    headbob_settings: Option<Single<&HeadbobSettings, With<PlayerControllerMarker>>>,
) {
    if let Some(headbob_settings) = headbob_settings.map(|hs| hs.into_inner()) {}
}
