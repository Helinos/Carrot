use bevy::{asset::embedded_asset, prelude::*, render::render_resource::AsBindGroup};
use bevy_trenchbroom::{
    geometry::MapGeometry, physics::SceneCollidersReady, prelude::GenericMaterial3d,
};
use nil::prelude::SmartDefault;

pub struct SpecialTexturesPlugin;

impl Plugin for SpecialTexturesPlugin {
    fn build(&self, app: &mut App) {
        embedded_asset!(app, "shaders/sky.wgsl");
        embedded_asset!(app, "shaders/portal.wgsl");

        app.add_plugins((
            MaterialPlugin::<SkyMaterial>::default(),
            MaterialPlugin::<PortalMaterial>::default(),
        ))
        .add_observer(replace_materials);
    }
}

fn replace_materials(
    _trigger: Trigger<SceneCollidersReady>,
    mut commands: Commands,
    mut materials: ResMut<Assets<SkyMaterial>>,
    map_geometry_query: Query<(Entity, &Name), With<MapGeometry>>,
    asset_server: Res<AssetServer>,
) {
    let sky_material = MeshMaterial3d(materials.add(SkyMaterial {
        texture: asset_server.load("textures/skybox/sky_cloudy018_hdr.exr"),
        ..default()
    }));

    map_geometry_query
        .iter()
        .filter_map(|(entity, name)| name.starts_with("sky").then(|| entity))
        .for_each(|entity| {
            commands
                .entity(entity)
                .remove::<GenericMaterial3d>()
                .insert(sky_material.clone());
        });
}

#[derive(Asset, AsBindGroup, Reflect, Clone, SmartDefault)]
pub struct SkyMaterial {
    #[texture(1)]
    #[sampler(2)]
    pub texture: Handle<Image>,
}

impl Material for SkyMaterial {
    fn fragment_shader() -> bevy::render::render_resource::ShaderRef {
        "embedded://Carrot/shaders/sky.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        AlphaMode::Opaque
    }
}

#[derive(Asset, AsBindGroup, Reflect, Clone, SmartDefault)]
pub struct PortalMaterial {
    #[texture(1)]
    #[sampler(2)]
    pub texture: Handle<Image>,
}

impl Material for PortalMaterial {
    fn fragment_shader() -> bevy::render::render_resource::ShaderRef {
        "embedded://Carrot/shaders/portal.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        AlphaMode::Opaque
    }
}
