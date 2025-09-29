use bevy::{
    prelude::*,
    window::{CursorGrabMode, PrimaryWindow},
};

#[cfg(feature = "debug")]
use bevy::remote::{RemotePlugin, http::RemoteHttpPlugin};
use bevy_framepace::{FramepacePlugin, FramepaceSettings, Limiter};
use bevy_rapier3d::prelude::*;
use bevy_trenchbroom::{
    TrenchBroomPlugins,
    config::TrenchBroomConfig,
    prelude::{ComputeLightmapSettings, SpawnHooks},
};
use leafwing_input_manager::{buttonlike, prelude::*};
use nil::ShortToString;

use crate::{
    class::CarrotClassPlugin,
    player::{
        CameraSettings, ControllerAimAcceleration, JumpSettings, PlayerControllerMarker,
        PlayerPlugin,
    },
    special_materials::SpecialMaterialsPlugin,
};

mod class;
mod player;
mod projections;
mod special_materials;

const DEFAULT_RENDER_LAYER: usize = 0;
const _VIEW_MODEL_RENDER_LAYER: usize = 1;
const PORTAL_RENDER_LAYER_1: usize = 2;
const PORTAL_RENDER_LAYER_2: usize = 3;

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::srgb(1.0, 0.0, 1.0)))
        .insert_resource(AmbientLight::NONE)
        .add_plugins((
            DefaultPlugins,
            SpecialMaterialsPlugin,
            RapierPhysicsPlugin::<()>::default(),
            bevy_panic_handler::PanicHandler::new().build(),
            PlayerPlugin,
            TrenchBroomPlugins(
                TrenchBroomConfig::new("Carrot")
                    .default_solid_spawn_hooks(|| SpawnHooks::new().convex_collider())
                    .bicubic_lightmap_filtering(true)
                    .compute_lightmap_settings(ComputeLightmapSettings {
                        extrusion: 1,
                        ..default()
                    })
                    .texture_extensions(vec![
                        //"bmp".s(),
                        //"exr".s(),
                        //"hdr".s(),
                        //"jpeg".s(),
                        //"jpg".s(),
                        "png".s(),
                        //"tga".s(),
                        //"webp".s(),
                        "D".s(),
                        //"C".s(),
                    ]),
            ),
            FramepacePlugin,
            CarrotClassPlugin,
            InputManagerPlugin::<InputAction>::default(),
            #[cfg(feature = "debug")]
            RemotePlugin::default(),
            #[cfg(feature = "debug")]
            RemoteHttpPlugin::default(),
            #[cfg(feature = "debug")]
            RapierDebugRenderPlugin {
                enabled: false,
                ..default()
            },
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, grab_cursor)
        .run();
}

#[derive(Actionlike, PartialEq, Eq, Hash, Clone, Copy, Debug, Reflect)]
enum InputAction {
    #[actionlike(DualAxis)]
    Move,
    #[actionlike(DualAxis)]
    LookMouse,
    #[actionlike(DualAxis)]
    LookController,
    Jump,
    Sprint,
    Crouch,
    Pause,
    UnlockMouse,
    Interact,
}

fn setup(
    mut commands: Commands,
    mut framepace_setting: ResMut<FramepaceSettings>,
    asset_server: Res<AssetServer>,
) {
    framepace_setting.limiter = Limiter::Auto;

    commands.spawn(SceneRoot(asset_server.load("maps/test.bsp#Scene")));

    use InputAction::*;

    let mut input_map: InputMap<InputAction> = InputMap::default();

    input_map.insert_dual_axis(Move, VirtualDPad::wasd().with_circle_bounds(1.0));
    input_map.insert_dual_axis(
        Move,
        GamepadStick::LEFT
            .with_circle_bounds(1.0)
            .with_circle_deadzone(0.1),
    );

    input_map.insert_dual_axis(LookMouse, MouseMove::default());
    input_map.insert_dual_axis(
        LookController,
        GamepadStick::RIGHT
            .with_circle_bounds(1.0)
            .with_circle_deadzone(0.1),
    );

    input_map.insert(Jump, KeyCode::Space);
    input_map.insert(Jump, GamepadButton::South);

    input_map.insert(Sprint, KeyCode::ShiftLeft);
    input_map.insert(Sprint, GamepadButton::West);

    input_map.insert(Pause, KeyCode::Escape);
    input_map.insert(Pause, GamepadButton::Start);

    input_map.insert(UnlockMouse, KeyCode::KeyF);

    input_map.insert(Interact, MouseButton::Left);
    input_map.insert(Interact, GamepadButton::East);

    commands.spawn(input_map);

    commands.spawn((
        PlayerControllerMarker,
        ControllerAimAcceleration::default(),
        JumpSettings::default(),
    ));
}

fn grab_cursor(
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
    mut camera_settings: Single<&mut CameraSettings>,
    action_state: Single<&ActionState<InputAction>>,
    button: Res<ButtonInput<MouseButton>>,
) -> Result {
    let mut window = windows.single_mut()?;

    if action_state.just_pressed(&InputAction::UnlockMouse)
        && window.cursor_options.grab_mode == CursorGrabMode::Locked
    {
        window.cursor_options.grab_mode = CursorGrabMode::Confined;
        window.cursor_options.visible = true;
        camera_settings.allow_movement = false;
    }

    if action_state.just_released(&InputAction::UnlockMouse)
        && window.cursor_options.grab_mode == CursorGrabMode::Confined
    {
        window.cursor_options.grab_mode = CursorGrabMode::Locked;
        window.cursor_options.visible = false;
        camera_settings.allow_movement = true;
    }

    if button.just_pressed(MouseButton::Left)
        && window.cursor_options.grab_mode == CursorGrabMode::None
    {
        window.cursor_options.grab_mode = CursorGrabMode::Locked;
        window.cursor_options.visible = false;
        camera_settings.allow_movement = true;
    }

    if action_state.just_pressed(&InputAction::Pause)
        && window.cursor_options.grab_mode == CursorGrabMode::Locked
    {
        window.cursor_options.grab_mode = CursorGrabMode::None;
        window.cursor_options.visible = true;
        camera_settings.allow_movement = false;
    }

    Ok(())
}
