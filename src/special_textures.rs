use bevy::{asset::embedded_asset, prelude::*, render::render_resource::AsBindGroup};
use nil::prelude::SmartDefault;

pub struct SpecialTexturesPlugin;

impl Plugin for SpecialTexturesPlugin {
    fn build(&self, app: &mut App) {
        embedded_asset!(app, "shaders/sky.wgsl");

        app.add_plugins(MaterialPlugin::<SkyMaterial>::default());
    }
}

#[derive(Asset, AsBindGroup, Reflect, Clone, SmartDefault)]
pub struct SkyMaterial {
    #[uniform(0)]
    #[default(2.)]
    pub radius: f32,

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
