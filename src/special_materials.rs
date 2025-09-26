use bevy::{asset::embedded_asset, prelude::*, render::render_resource::AsBindGroup};
use bevy_trenchbroom::{geometry::MapGeometry, prelude::GenericMaterial3d};

pub struct SpecialMaterialsPlugin;

impl Plugin for SpecialMaterialsPlugin {
    fn build(&self, app: &mut App) {
        embedded_asset!(app, "shaders/sky.wgsl");
        embedded_asset!(app, "shaders/portal.wgsl");

        app.add_plugins((
            MaterialPlugin::<SkyMaterial>::default(),
            MaterialPlugin::<PortalMaterial>::default(),
        ))
        .add_systems(PostUpdate, setup_new_brushes);
    }
}

#[derive(Component)]
pub struct MaterialSetupFinishedMarker;

fn setup_new_brushes(
    mut commands: Commands,
    mut materials: ResMut<Assets<SkyMaterial>>,
    new_brushes: Populated<
        (Entity, &Name),
        (With<MapGeometry>, Without<MaterialSetupFinishedMarker>),
    >,
    asset_server: Res<AssetServer>,
) {
    // TODO: Not this
    let sky_material = MeshMaterial3d(materials.add(SkyMaterial {
        texture: asset_server.load("textures/skybox/sky_cloudy018_hdr.exr"),
        ..default()
    }));

    for (brush_entity, brush_name) in new_brushes.iter() {
        if brush_name.starts_with("sky") {
            commands
                .entity(brush_entity)
                .remove::<GenericMaterial3d>()
                .insert(sky_material.clone());
        }

        commands
            .entity(brush_entity)
            .insert(MaterialSetupFinishedMarker);
    }
}

#[derive(Asset, AsBindGroup, Reflect, Clone, Default)]
pub struct SkyMaterial {
    #[texture(0)]
    #[sampler(1)]
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

#[derive(Asset, AsBindGroup, Reflect, Clone, Default)]
pub struct PortalMaterial {
    #[texture(0)]
    #[sampler(1)]
    pub texture_handle: Handle<Image>,
}

impl Material for PortalMaterial {
    fn fragment_shader() -> bevy::render::render_resource::ShaderRef {
        "embedded://Carrot/shaders/portal.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        AlphaMode::Opaque
    }

    fn specialize(
        _pipeline: &bevy::pbr::MaterialPipeline<Self>,
        descriptor: &mut bevy::render::render_resource::RenderPipelineDescriptor,
        _layout: &bevy::render::mesh::MeshVertexBufferLayoutRef,
        _key: bevy::pbr::MaterialPipelineKey<Self>,
    ) -> Result<(), bevy::render::render_resource::SpecializedMeshPipelineError> {
        descriptor.primitive.cull_mode = None;

        // Prevent Z-fighting on the inside faces of a portal brush.
        if let Some(depth_stencil) = descriptor.depth_stencil.as_mut() {
            depth_stencil.bias.constant = 48;
        }

        Ok(())
    }
}
