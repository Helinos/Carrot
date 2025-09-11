use bevy::{
    prelude::*,
    scene::SceneInstanceReady,
    window::{CursorGrabMode, PrimaryWindow},
};
use bevy_framepace::{FramepacePlugin, FramepaceSettings, Limiter};
use bevy_inspector_egui::{bevy_egui::EguiPlugin, quick::WorldInspectorPlugin};
use bevy_rapier3d::prelude::*;
use bevy_trenchbroom::{
    TrenchBroomPlugins,
    config::TrenchBroomConfig,
    geometry::MapGeometry,
    physics::SceneCollidersReady,
    prelude::{ComputeLightmapSettings, GenericMaterial3d, SpawnHooks},
};
use nil::ShortToString;

use crate::{
    player::{
        CameraSettings, ControllerAimAcceleration, JumpSettings, PlayerController, PlayerPlugin,
    },
    special_textures::{SkyMaterial, SpecialTexturesPlugin},
};

mod player;
mod special_textures;

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::BLACK))
        .insert_resource(AmbientLight::NONE)
        //.insert_resource(Time::<Fixed>::from_hz(60.0))
        .add_plugins((
            DefaultPlugins,
            SpecialTexturesPlugin,
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
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, grab_cursor)
        .add_observer(replace_materials)
        .run();
}

fn setup(
    mut commands: Commands,
    mut framepace_setting: ResMut<FramepaceSettings>,
    asset_server: Res<AssetServer>,
) {
    framepace_setting.limiter = Limiter::Auto;

    commands.spawn(SceneRoot(asset_server.load("maps/test.bsp#Scene")));

    commands.spawn((
        PlayerController,
        ControllerAimAcceleration::default(),
        JumpSettings::default(),
    ));
}

fn replace_materials(
    _trigger: Trigger<SceneCollidersReady>,
    mut commands: Commands,
    mut materials: ResMut<Assets<SkyMaterial>>,
    sky_query: Query<(Entity, &Name), With<MapGeometry>>,
    asset_server: Res<AssetServer>,
) {
    let sky_material = MeshMaterial3d(materials.add(SkyMaterial {
        texture: asset_server.load("textures/skybox/sky_cloudy018_hdr.exr"),
        ..default()
    }));

    sky_query
        .iter()
        .filter_map(|(entity, name)| name.starts_with("sky").then(|| entity))
        .for_each(|entity| {
            commands
                .entity(entity)
                .remove::<GenericMaterial3d>()
                .insert(sky_material.clone());
        });
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
