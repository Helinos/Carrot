use bevy::{
    prelude::*,
    window::{CursorGrabMode, PrimaryWindow},
};
use bevy_framepace::{FramepacePlugin, FramepaceSettings, Limiter};
use bevy_inspector_egui::{bevy_egui::EguiPlugin, quick::WorldInspectorPlugin};
use bevy_rapier3d::prelude::*;
use bevy_trenchbroom::{
    TrenchBroomPlugins,
    config::TrenchBroomConfig,
    prelude::{ComputeLightmapSettings, SpawnHooks},
};
use nil::ShortToString;

use crate::{
    player::{
        CameraSettings, ControllerAimAcceleration, JumpSettings, PlayerControllerMarker,
        PlayerPlugin,
    },
    portal::PortalPlugin,
    special_materials::SpecialMaterialsPlugin,
};

mod player;
mod portal;
mod projection;
mod special_materials;

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::srgb(1.0, 0.0, 1.0)))
        .insert_resource(AmbientLight::NONE)
        //.insert_resource(Time::<Fixed>::from_hz(60.0))
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
                        "bmp".s(),
                        "exr".s(),
                        "hdr".s(),
                        "jpeg".s(),
                        "jpg".s(),
                        "png".s(),
                        "tga".s(),
                        "webp".s(),
                        "D".s(),
                        "C".s(),
                    ]),
            ),
            EguiPlugin::default(),
            WorldInspectorPlugin::new(),
            FramepacePlugin,
            PortalPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, grab_cursor)
        .run();
}

const DEFAULT_RENDER_LAYER: usize = 0;
const _VIEW_MODEL_RENDER_LAYER: usize = 1;
const PORTAL_RENDER_LAYER_1: usize = 2;
const PORTAL_RENDER_LAYER_2: usize = 3;

fn setup(
    mut commands: Commands,
    mut framepace_setting: ResMut<FramepaceSettings>,
    asset_server: Res<AssetServer>,
) {
    framepace_setting.limiter = Limiter::Auto;

    commands.spawn(SceneRoot(asset_server.load("maps/test.bsp#Scene")));

    commands.spawn((
        PlayerControllerMarker,
        ControllerAimAcceleration::default(),
        JumpSettings::default(),
    ));
}

fn grab_cursor(
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
    mut camera_settings: Single<&mut CameraSettings>,
    button: Res<ButtonInput<MouseButton>>,
    key: Res<ButtonInput<KeyCode>>,
) -> Result {
    let mut window = windows.single_mut()?;

    if key.just_pressed(KeyCode::KeyF) && window.cursor_options.grab_mode == CursorGrabMode::Locked
    {
        window.cursor_options.grab_mode = CursorGrabMode::Confined;
        window.cursor_options.visible = true;
        camera_settings.allow_movement = false;
    }

    if key.just_released(KeyCode::KeyF)
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

    if key.just_pressed(KeyCode::Escape)
        && window.cursor_options.grab_mode == CursorGrabMode::Locked
    {
        window.cursor_options.grab_mode = CursorGrabMode::None;
        window.cursor_options.visible = true;
        camera_settings.allow_movement = false;
    }

    Ok(())
}
