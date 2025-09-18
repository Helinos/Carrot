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
    PORTAL_RENDER_LAYER_1, PORTAL_RENDER_LAYER_2, player::WorldCameraMarker,
    special_materials::PortalMaterial,
};

pub struct PortalPlugin;

impl Plugin for PortalPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<PortalClass>()
            .register_type::<PortalGeometry>()
            .add_systems(PreUpdate, update_visibility)
            .add_systems(
                PostUpdate,
                (
                    setup_portals,
                    update_camera_positions
                        .after(RapierTransformPropagateSet)
                        .before(TransformSystem::TransformPropagate),
                ),
            );
    }
}

#[derive(Component)]
pub struct PortalCamera {
    pub lateral_offset: Vec3,
    pub rotational_offset: Quat,
    pub destination_center: Vec3,
}

#[derive(Component)]
#[relationship(relationship_target = PortalCameraChildren)]
pub struct PortalCameraChildOf(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = PortalCameraChildOf, linked_spawn)]
pub struct PortalCameraChildren(Vec<Entity>);

#[derive(Component)]
#[relationship(relationship_target = PortalDestination)]
pub struct PortalSource(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = PortalSource, linked_spawn)]
pub struct PortalDestination(Vec<Entity>);

#[derive(Component)]
#[relationship(relationship_target = PortalSurfaceChildren)]
pub struct PortalSurfaceChildOf(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = PortalSurfaceChildOf, linked_spawn)]
pub struct PortalSurfaceChildren(Vec<Entity>);

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct PortalGeometry {
    pub id: String,
    pub destination_id: String,
    pub facing: Vec3,
}

#[derive(Component)]
pub struct PortalSurface {
    visible: bool,
}

#[solid_class(
    hooks(SpawnHooks::new().push(Self::spawn_hook)),
    classname("func_world_portal")
)]
#[derive(Clone, SmartDefault)]
pub struct PortalClass {
    #[class(must_set)]
    id: String,
    #[class(must_set)]
    destination_id: String,
    #[class(must_set)]
    #[default(vec3(0., 0., 0.))]
    facing: Vec3,
}

impl PortalClass {
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
            .insert(PortalGeometry {
                id,
                destination_id,
                facing,
            });

        Ok(())
    }
}

pub fn setup_portals(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<PortalMaterial>>,
    portals: Populated<(Entity, &PortalGeometry, &Aabb), Without<PortalSurfaceChildren>>,
    windows: Query<&Window, With<PrimaryWindow>>,
    player_camera: Single<&Projection, With<WorldCameraMarker>>,
) -> Result {
    let window = windows.single()?;
    let player_camera_projection = player_camera.into_inner();

    let size = Extent3d {
        width: window.physical_width(),
        height: window.physical_height(),
        ..default()
    };

    fn euler_to_quat(euler: Vec3) -> Quat {
        Quat::from_euler(
            EulerRot::YXZ,
            euler.x.to_radians(),
            euler.y.to_radians(),
            euler.z.to_radians(),
        )
    }

    let mut setup_portal = |source_portal: &(Entity, &PortalGeometry, &Aabb),
                            dest_portal: &(Entity, &PortalGeometry, &Aabb),
                            portal_render_layer: usize| {
        let (source_portal_ent, source_portal, source_portal_aabb) = source_portal;
        let (dest_portal_ent, dest_portal, dest_portal_aabb) = dest_portal;

        let lateral_offset = dest_portal_aabb.center - source_portal_aabb.center;
        let rotational_offset_euler = dest_portal.facing - source_portal.facing;
        let rotational_offset = euler_to_quat(rotational_offset_euler);
        let destination_center = dest_portal_aabb.center.into();

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

        let portal_normal_quat = euler_to_quat(source_portal.facing);
        let portal_up_vec = portal_normal_quat * Vec3A::Y;
        let portal_side_vec = portal_normal_quat * Vec3A::Z;

        let portal_height = source_portal_aabb.relative_radius(&portal_up_vec, &Mat3A::IDENTITY);
        let portal_width = source_portal_aabb.relative_radius(&portal_side_vec, &Mat3A::IDENTITY);

        let (fov, near, aspect_ratio) = match player_camera_projection {
            Projection::Perspective(perspective_projection) => (
                perspective_projection.fov,
                perspective_projection.near,
                perspective_projection.aspect_ratio,
            ),
            _ => panic!("Expected player to have a perspective projection."),
        };

        let near_plane_half_height = near * (fov * 0.5).to_radians().tan();
        let near_plane_half_width = near_plane_half_height * aspect_ratio;
        let near_plane_diagonal_radius =
            Vec3::new(near_plane_half_width, near_plane_half_height, near).length();

        let portal_cuboid = Cuboid {
            half_size: Vec3::new(near_plane_diagonal_radius, portal_height, portal_width),
        };

        commands
            .entity(*source_portal_ent)
            .remove::<GenericMaterial3d>();

        commands.spawn((
            Camera {
                target: texture_handle.into(),
                ..default()
            },
            Camera3d::default(),
            Projection::from(PerspectiveProjection { fov, ..default() }),
            PortalCameraChildOf(*source_portal_ent),
            PortalCamera {
                lateral_offset: lateral_offset.into(),
                rotational_offset,
                destination_center,
            },
        ));

        commands.spawn((
            PortalSurface { visible: false },
            PortalSurfaceChildOf(*source_portal_ent),
            PortalSource(*dest_portal_ent),
            Mesh3d(meshes.add(portal_cuboid)),
            MeshMaterial3d(portal_material_handle),
            RenderLayers::layer(portal_render_layer),
            Transform::from_rotation(portal_normal_quat)
                .with_translation(source_portal_aabb.center.into()),
        ));
    };

    let mut combinations = portals.iter_combinations();
    while let Some([portal_1, portal_2]) = combinations.fetch_next() {
        if portal_1.1.id != portal_2.1.destination_id || portal_1.1.destination_id != portal_2.1.id
        {
            continue;
        }

        setup_portal(&portal_1, &portal_2, PORTAL_RENDER_LAYER_1);
        setup_portal(&portal_2, &portal_1, PORTAL_RENDER_LAYER_2);
    }

    Ok(())
}

pub fn update_visibility(
    mut portals: Populated<(Entity, &mut PortalSurface, &ViewVisibility)>,
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

pub fn update_camera_positions(
    player_camera: Single<&GlobalTransform, With<WorldCameraMarker>>,
    mut portal_cameras: Populated<(&mut Transform, &PortalCamera), Without<WorldCameraMarker>>,
) -> Result {
    let player_camera_transform = player_camera.into_inner().compute_transform();

    for (mut portal_camera_transform, portal_camera) in portal_cameras.iter_mut() {
        portal_camera_transform.translation =
            player_camera_transform.translation + portal_camera.lateral_offset;
        portal_camera_transform.rotation = player_camera_transform.rotation;
        portal_camera_transform.rotate_around(
            portal_camera.destination_center,
            portal_camera.rotational_offset,
        )
    }

    Ok(())
}
