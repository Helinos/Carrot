use bevy::{
    asset::RenderAssetUsages,
    prelude::*,
    render::{
        primitives::Aabb,
        render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages},
        view::RenderLayers,
    },
    window::PrimaryWindow,
};
use bevy_rapier3d::plugin::RapierTransformPropagateSet;
use bevy_trenchbroom::{
    anyhow::{self, anyhow},
    class::QuakeClassSpawnView,
    prelude::*,
};
use nil::prelude::SmartDefault;

use crate::{
    DEFAULT_RENDER_LAYER, PORTAL_RENDER_LAYER_1, PORTAL_RENDER_LAYER_2,
    player::{FOV, WorldCameraMarker},
    special_materials::PortalMaterial,
};

pub struct PortalPlugin;

impl Plugin for PortalPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<WorldPortalClass>()
            .register_type::<WorldPortal>()
            .add_systems(PreUpdate, update_visibility)
            .add_systems(
                PostUpdate,
                (
                    link_portals,
                    update_camera_positions
                        .after(RapierTransformPropagateSet)
                        .before(TransformSystem::TransformPropagate),
                ),
            );
    }
}

#[derive(Component)]
#[relationship(relationship_target = PortalCameraChildren)]
struct PortalCameraChildOf(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = PortalCameraChildOf, linked_spawn)]
struct PortalCameraChildren(Vec<Entity>);

#[derive(Component)]
#[relationship(relationship_target = WorldPortalDest)]
struct WorldPortalSource(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = WorldPortalSource, linked_spawn)]
struct WorldPortalDest(Vec<Entity>);

#[derive(Component, Reflect)]
#[reflect(Component)]
struct WorldPortal {
    pub id: String,
    pub destination_id: String,
    pub facing: Vec3,
    pub visible: bool,
    pub lateral_offset: Vec3,
    pub rotational_offset: Quat,
    pub destination_center: Vec3,
}

impl WorldPortal {
    fn new(id: String, destination_id: String, facing: Vec3) -> Self {
        Self {
            id,
            destination_id,
            facing,
            visible: false,
            lateral_offset: Vec3::default(),
            rotational_offset: Quat::default(),
            destination_center: Vec3::default(),
        }
    }
}

#[solid_class(
    hooks(SpawnHooks::new().push(Self::spawn_hook)),
    classname("func_world_portal")
)]
#[derive(Clone, SmartDefault)]
struct WorldPortalClass {
    #[class(must_set)]
    id: String,
    #[class(must_set)]
    destination_id: String,
    #[class(must_set)]
    #[default(vec3(0., 0., 0.))]
    facing: Vec3,
}

impl WorldPortalClass {
    pub fn spawn_hook(view: &mut QuakeClassSpawnView) -> anyhow::Result<()> {
        let quake_mesh_view = if view.meshes.len() == 1 {
            &view.meshes[0]
        } else {
            return Err(anyhow!(
                "A worldportal entity may not contain more than one mesh"
            ));
        };

        let source_entity = view.src_entity;
        let id: String = source_entity.get("id")?;
        let destination_id: String = source_entity.get("destination_id")?;
        let facing: Vec3 = source_entity.get("facing")?;

        view.world
            .commands()
            .entity(quake_mesh_view.entity)
            .insert(WorldPortal::new(id, destination_id, facing));

        Ok(())
    }
}

fn link_portals(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<PortalMaterial>>,
    fov: Res<FOV>,
    mut portals: Populated<(Entity, &mut WorldPortal, &Aabb), Without<WorldPortalSource>>,
    windows: Query<&Window, With<PrimaryWindow>>,
) -> Result {
    let window = windows.single()?;
    let fov = fov.into_inner().into();

    let size = Extent3d {
        width: window.physical_width(),
        height: window.physical_height(),
        ..default()
    };

    let mut link = |source_portal: &mut (Entity, Mut<'_, WorldPortal>, &Aabb),
                    dest_portal: &(Entity, Mut<'_, WorldPortal>, &Aabb),
                    portal_render_layer: usize| {
        let (source_portal_ent, source_portal, source_portal_aabb) = source_portal;
        let (dest_portal_ent, dest_portal, dest_portal_aabb) = dest_portal;

        source_portal.lateral_offset = (dest_portal_aabb.center - source_portal_aabb.center).into();
        let rotational_offset = dest_portal.facing - source_portal.facing;
        source_portal.rotational_offset = Quat::from_euler(
            EulerRot::YXZ,
            rotational_offset.x.to_radians(),
            rotational_offset.y.to_radians(),
            rotational_offset.z.to_radians(),
        );
        source_portal.destination_center = dest_portal_aabb.center.into();

        commands
            .entity(*source_portal_ent)
            .insert(WorldPortalSource(*dest_portal_ent));

        let mut texture = Image::new_uninit(
            size,
            TextureDimension::D2,
            TextureFormat::Rgba8UnormSrgb,
            RenderAssetUsages::default(),
        );

        texture.texture_descriptor.usage = TextureUsages::TEXTURE_BINDING
            | TextureUsages::COPY_DST
            | TextureUsages::RENDER_ATTACHMENT;

        let texture_handle = images.add(texture);
        let portal_material_handle = materials.add(PortalMaterial {
            texture_handle: texture_handle.clone(),
        });

        commands.spawn((
            Camera {
                target: texture_handle.into(),
                ..default()
            },
            Camera3d::default(),
            Projection::from(PerspectiveProjection { fov, ..default() }),
            PortalCameraChildOf(*source_portal_ent),
        ));

        commands
            .entity(*source_portal_ent)
            .remove::<GenericMaterial3d>()
            .insert((
                MeshMaterial3d(portal_material_handle),
                RenderLayers::layer(portal_render_layer),
            ));
    };

    let mut combinations = portals.iter_combinations_mut();
    while let Some([mut portal_1, mut portal_2]) = combinations.fetch_next() {
        if portal_1.1.id != portal_2.1.destination_id || portal_1.1.destination_id != portal_2.1.id
        {
            continue;
        }

        link(&mut portal_1, &portal_2, PORTAL_RENDER_LAYER_1);
        link(&mut portal_2, &portal_1, PORTAL_RENDER_LAYER_2);
    }

    Ok(())
}

fn update_visibility(
    mut portals: Populated<(Entity, &mut WorldPortal, &ViewVisibility)>,
    mut portal_cameras: Populated<(Entity, &mut Camera), With<PortalCameraChildOf>>,
    camera_relationships: Populated<&PortalCameraChildOf>,
) {
    for (camera_ent, mut camera) in portal_cameras.iter_mut() {
        for related in camera_relationships.iter_ancestors(camera_ent) {
            let Ok((_portal_ent, mut portal, view_visibility)) = portals.get_mut(related) else {
                continue;
            };

            let visibility = view_visibility.get();
            if visibility != portal.visible {
                portal.visible = visibility;
                camera.is_active = visibility;
            }
        }
    }
}

fn update_camera_positions(
    player_camera: Single<&GlobalTransform, With<WorldCameraMarker>>,
    portals: Populated<&WorldPortal>,
    mut portal_cameras: Populated<
        (Entity, &mut Transform),
        (With<PortalCameraChildOf>, Without<WorldCameraMarker>),
    >,
    camera_relationships: Populated<&PortalCameraChildOf>,
) -> Result {
    let (_, player_camera_rotation, player_camera_translation) =
        player_camera.into_inner().to_scale_rotation_translation();

    for (camera_ent, mut camera_transform) in portal_cameras.iter_mut() {
        let Some(source_portal) = camera_relationships
            .iter_ancestors(camera_ent)
            .find_map(|ancestor| portals.get(ancestor).ok())
        else {
            return Err(
                anyhow!("There is a PortalCamera that isn't related to any portal.").into(),
            );
        };

        camera_transform.translation = player_camera_translation + source_portal.lateral_offset;
        camera_transform.rotation = player_camera_rotation;
        camera_transform.rotate_around(
            source_portal.destination_center,
            source_portal.rotational_offset,
        );
    }

    Ok(())
}
