use bevy::{
    asset::RenderAssetUsages,
    prelude::*,
    render::{
        Extract, Render, RenderApp, RenderSet,
        camera::{CameraProjection, extract_cameras},
        primitives::Aabb,
        render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages},
        sync_world::RenderEntity,
        view::{ExtractedView, RenderLayers},
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
    projections::ObliquePerspectiveProjection, special_materials::PortalMaterial,
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
                    update_camera_projections.after(TransformSystem::TransformPropagate),
                ),
            );

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_systems(ExtractSchedule, extract_projections.after(extract_cameras))
                .add_systems(
                    Render,
                    replace_extracted_projection
                        .after(RenderSet::ExtractCommands)
                        .before(RenderSet::PrepareAssets),
                );
        }
    }
}

#[derive(Component)]
pub struct PortalCamera {
    pub source_transform: Transform,
    pub dest_transform: Transform,
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
    pub normal: Vec3,
}

#[derive(Component)]
pub struct PortalSurface {
    visible: bool,
    dest_transform: Transform,
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
    normal: Vec3,
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
        let normal: Vec3 = source_entity.get("normal")?;

        view.world
            .commands()
            .entity(quake_mesh_view.entity)
            .insert(PortalGeometry {
                id,
                destination_id,
                normal,
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

    println!("Hey");
    info!("Hey");

    let mut setup_portal = |source_portal: &(Entity, &PortalGeometry, &Aabb),
                            dest_portal: &(Entity, &PortalGeometry, &Aabb),
                            portal_render_layer: usize| {
        let (source_portal_ent, source_portal_geometry, source_portal_aabb) = source_portal;
        let (dest_portal_ent, dest_portal_geometry, dest_portal_aabb) = dest_portal;

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

        fn euler_deg_to_quat(euler: Vec3) -> Quat {
            Quat::from_euler(
                EulerRot::YXZ,
                euler.x.to_radians(),
                euler.y.to_radians(),
                euler.z.to_radians(),
            )
        }

        let portal_rotation = euler_deg_to_quat(source_portal_geometry.normal);
        let dest_portal_rotation = euler_deg_to_quat(dest_portal_geometry.normal);
        let source_transform = Transform::from_rotation(portal_rotation)
            .with_translation(source_portal_aabb.center.into());
        let dest_transform = Transform::from_rotation(dest_portal_rotation)
            .with_translation(dest_portal_aabb.center.into());
        let portal_up = source_transform.up();
        let portal_right = source_transform.right();

        let portal_height =
            source_portal_aabb.relative_radius(&portal_up.as_vec3().into(), &Mat3A::IDENTITY);
        let portal_width =
            source_portal_aabb.relative_radius(&portal_right.as_vec3().into(), &Mat3A::IDENTITY);

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
            half_size: Vec3::new(portal_width, portal_height, near_plane_diagonal_radius),
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
            Projection::custom(ObliquePerspectiveProjection {
                perspective: PerspectiveProjection { fov, ..default() },
                ..default()
            }),
            PortalCameraChildOf(*source_portal_ent),
            PortalCamera {
                source_transform,
                dest_transform,
            },
        ));

        commands.spawn((
            PortalSurface {
                visible: false,
                dest_transform: dest_transform,
            },
            PortalSurfaceChildOf(*source_portal_ent),
            PortalSource(*dest_portal_ent),
            Mesh3d(meshes.add(portal_cuboid)),
            MeshMaterial3d(portal_material_handle),
            RenderLayers::layer(portal_render_layer),
            source_transform,
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

    for (mut camera_transform, portal_camera) in portal_cameras.iter_mut() {
        camera_transform.translation = player_camera_transform.translation
            + (portal_camera.dest_transform.translation
                - portal_camera.source_transform.translation);
        camera_transform.rotation = player_camera_transform.rotation;
        camera_transform.rotate_around(
            portal_camera.dest_transform.translation,
            portal_camera.dest_transform.rotation
                * portal_camera.source_transform.rotation.inverse(),
        );
    }

    Ok(())
}

pub fn update_camera_projections(
    mut portal_cameras: Populated<(&GlobalTransform, &PortalCamera, &mut Projection)>,
) {
    for (camera_transform, portal_camera, projection) in portal_cameras.iter_mut() {
        let projection: &mut ObliquePerspectiveProjection = match projection.into_inner() {
            Projection::Custom(custom_projection) => custom_projection.downcast_mut().unwrap(),
            _ => panic!("Expected portal camera to have a custom projection."),
        };

        let world_from_view = camera_transform.compute_matrix();
        let view_from_world = world_from_view.inverse();
        let adj_view_from_world = view_from_world.determinant() * world_from_view;

        let sign = portal_camera
            .dest_transform
            .forward()
            .dot(portal_camera.dest_transform.translation - camera_transform.translation())
            .signum();

        let v_dest_translation =
            view_from_world.transform_point3(portal_camera.dest_transform.translation);
        let v_dest_normal = adj_view_from_world
            .transpose()
            .transform_vector3(portal_camera.dest_transform.forward().as_vec3())
            .normalize()
            * sign;

        // There is a very small seam between the portal and the near plane that is seemingly exagerated at higher viewing angles.
        const NEAR_PLANE_OFFSET: f32 = 0.1;
        // The projection breaks when the camera is very close to the portal surface.
        const NEAR_CLIP_LIMIT: f32 = 0.1;

        let v_plane_distance = -v_dest_translation.dot(v_dest_normal) + NEAR_PLANE_OFFSET;

        if v_plane_distance.abs() > NEAR_CLIP_LIMIT {
            projection.view_near_plane = v_dest_normal.extend(v_plane_distance);
        } else {
            projection.view_near_plane = Vec3::NEG_Z.extend(-projection.perspective.near);
        }
    }
}

pub fn extract_projections(
    mut commands: Commands,
    projections: Extract<Query<(RenderEntity, &Projection)>>,
) {
    for (render_ent, projection) in projections.iter() {
        commands.entity(render_ent).insert(projection.clone());
    }
}

// Recalculate the projection matrix in the extracted view because it's currently from a frame ago.
// This query is definitely not robust enough... Oh well.
pub fn replace_extracted_projection(projections: Query<(&Projection, &mut ExtractedView)>) {
    for (projection, mut view) in projections {
        view.clip_from_view = projection.get_clip_from_view();
    }
}
