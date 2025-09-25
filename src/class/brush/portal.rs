use bevy::{
    asset::RenderAssetUsages,
    core_pipeline::tonemapping::Tonemapping,
    math::DVec3,
    prelude::*,
    render::{
        Extract, Render, RenderApp, RenderSet,
        camera::{CameraProjection, extract_cameras},
        primitives::Aabb,
        render_resource::{Extent3d, TextureDimension, TextureUsages},
        sync_world::RenderEntity,
        view::{ExtractedView, RenderLayers, ViewTarget},
    },
    window::PrimaryWindow,
};
use bevy_rapier3d::{
    plugin::{RapierTransformPropagateSet, systems::update_character_controls},
    prelude::{Collider, ReadRapierContext, RigidBody, Sensor},
};
use bevy_trenchbroom::{
    anyhow::{self},
    brush::ConvexHull,
    class::QuakeClassSpawnView,
    prelude::*,
};

use crate::{
    PORTAL_RENDER_LAYER_1,
    class::{TargetedBy, Targeting},
    player::{PlayerVelocity, PlayerWorldCameraMarker},
    projections::ObliquePerspectiveProjection,
    special_materials::PortalMaterial,
};

pub struct PortalPlugin;

impl Plugin for PortalPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<PortalClass>()
            .register_type::<PortalSurface>()
            .add_systems(PreUpdate, (update_visibility, update_portal_texture_size))
            .add_systems(
                PostUpdate,
                (
                    setup_portals,
                    portal_teleport
                        .after(update_character_controls)
                        .before(RapierTransformPropagateSet),
                    (update_camera_positions, update_camera_projections)
                        .after(RapierTransformPropagateSet)
                        .before(TransformSystem::TransformPropagate)
                        .chain(),
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
#[component(immutable)]
#[relationship(relationship_target = PortalCameraChild)]
pub struct PortalCameraChildOf(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = PortalCameraChildOf, linked_spawn)]
pub struct PortalCameraChild(Entity);

#[derive(Component, Reflect)]
#[reflect(Component)]
#[component(immutable)]
pub struct PortalSurface {
    pub height: f32,
    pub width: f32,
}

#[derive(Component)]
pub struct OldTranslation(Vec3);

#[solid_class(
    base(
        Transform,
        Target,
        Targetable,
    ),
    hooks(SpawnHooks::new().push(Self::spawn_hook)),
    classname("func_world_portal")
)]
pub struct PortalClass;

impl PortalClass {
    pub fn spawn_hook(view: &mut QuakeClassSpawnView) -> anyhow::Result<()> {
        let src_entity = view.src_entity;
        let brushes = &src_entity.brushes;
        let target_name: String = src_entity.get("targetname")?;

        if brushes.len() > 1 {
            warn!(
                "Portal with targetname \"{}\", has more than one brush. This is undefined behavior.",
                target_name
            );
        }

        let (portal_min, portal_max): (DVec3, DVec3) = brushes
            .get(0)
            .unwrap()
            .as_cuboid()
            .ok_or(format!(
                "Portal with targetname \"{}\", is not a cuboid when it must be.",
                target_name
            ))
            .unwrap();

        let aabb = Aabb::from_min_max(portal_min.as_vec3(), portal_max.as_vec3());

        let mut portal_ent = view.world.entity_mut(view.entity);
        let mut portal_transform = portal_ent.get_mut::<Transform>().unwrap();

        let portal_height =
            aabb.relative_radius(&portal_transform.up().as_vec3().into(), &Mat3A::IDENTITY);
        let portal_width =
            aabb.relative_radius(&portal_transform.right().as_vec3().into(), &Mat3A::IDENTITY);

        let portal_center = (0.5 * (portal_min + portal_max)).as_vec3();

        portal_transform.translation = portal_center;
        portal_ent.insert(PortalSurface {
            height: portal_height,
            width: portal_width,
        });

        Ok(())
    }
}

pub fn setup_portals(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<PortalMaterial>>,
    portals: Populated<(Entity, &PortalSurface), Without<PortalCameraChild>>,
    windows: Query<&Window, With<PrimaryWindow>>,
    player_camera: Single<&Projection, With<PlayerWorldCameraMarker>>,
) -> Result {
    let window = windows.single()?;
    let player_camera_projection = player_camera.into_inner();

    let size = Extent3d {
        width: window.physical_width(),
        height: window.physical_height(),
        ..default()
    };

    for (portal_ent, portal_surface) in portals.iter() {
        let mut texture = Image::new_uninit(
            size,
            TextureDimension::D2,
            ViewTarget::TEXTURE_FORMAT_HDR,
            RenderAssetUsages::default(),
        );

        texture.texture_descriptor.usage = TextureUsages::TEXTURE_BINDING
            | TextureUsages::COPY_DST
            | TextureUsages::RENDER_ATTACHMENT;

        let texture_handle = images.add(texture);
        let portal_material_handle = materials.add(PortalMaterial {
            texture_handle: texture_handle.clone(),
        });

        let (fov, near, aspect_ratio) = match player_camera_projection {
            Projection::Perspective(perspective_projection) => (
                perspective_projection.fov,
                perspective_projection.near,
                perspective_projection.aspect_ratio,
            ),
            _ => panic!("Expected player to have a perspective projection."),
        };

        let near_plane_half_height = near * (fov / 2.0).to_radians().tan();
        let near_plane_half_width = near_plane_half_height * aspect_ratio;
        let near_plane_diagonal_radius =
            Vec3::new(near_plane_half_width, near_plane_half_height, near).length();

        let portal_cuboid = Cuboid {
            half_size: Vec3::new(
                portal_surface.width,
                portal_surface.height,
                near_plane_diagonal_radius,
            ),
        };

        commands.entity(portal_ent).insert((
            Mesh3d(meshes.add(portal_cuboid)),
            MeshMaterial3d(portal_material_handle),
            RenderLayers::layer(PORTAL_RENDER_LAYER_1),
            Collider::cuboid(
                portal_surface.width,
                portal_surface.height,
                near_plane_diagonal_radius,
            ),
            Sensor,
        ));

        commands.spawn((
            Camera {
                target: texture_handle.into(),
                clear_color: ClearColorConfig::None,
                hdr: true,
                ..default()
            },
            // Tonemapping is applied by the world model camera.
            // If it isn't disabled here then it will be applied twice, to undesireable effect.
            Tonemapping::None,
            Camera3d::default(),
            Projection::custom(ObliquePerspectiveProjection {
                perspective: PerspectiveProjection { fov, ..default() },
                ..default()
            }),
            PortalCameraChildOf(portal_ent),
        ));
    }

    Ok(())
}

pub fn update_portal_texture_size(
    player_camera: Single<&Camera, (Changed<Camera>, With<PlayerWorldCameraMarker>)>,
    mut materials: ResMut<Assets<PortalMaterial>>,
    mut images: ResMut<Assets<Image>>,
) -> Result {
    let player_camera = player_camera.into_inner();

    for (_, material) in materials.iter_mut() {
        let portal_image = &mut material.texture_handle;
        let Some(image) = images.get_mut(portal_image) else {
            continue;
        };
        let Some(viewport) = player_camera.physical_viewport_size() else {
            continue;
        };

        let size = Extent3d {
            width: viewport.x,
            height: viewport.y,
            ..Extent3d::default()
        };

        image.texture_descriptor.size = size;
    }

    Ok(())
}

/// Disables the portal's camera if the portal's surface not visible.
pub fn update_visibility(
    mut portal_cameras: Populated<(&mut Camera, &PortalCameraChildOf)>,
    mut portals: Populated<
        &ViewVisibility,
        (Without<PortalCameraChildOf>, With<PortalCameraChild>),
    >,
) -> Result {
    for (mut camera, camera_child_of) in portal_cameras.iter_mut() {
        let view_visibility = portals.get_mut(camera_child_of.0)?;

        let visibility = view_visibility.get();
        if visibility != camera.is_active {
            camera.is_active = visibility;
        }
    }

    Ok(())
}

pub fn update_camera_positions(
    player_camera: Single<&GlobalTransform, With<PlayerWorldCameraMarker>>,
    mut portal_cameras: Populated<(&mut Transform, &PortalCameraChildOf), Without<PortalSurface>>,
    portals: Populated<(&Targeting, &Transform), (With<PortalSurface>, With<PortalCameraChild>)>,
) -> Result {
    let player_camera_transform = player_camera.into_inner().compute_transform();

    for (mut camera_transform, camera_child_of) in portal_cameras.iter_mut() {
        let (source_for, source_transform) = portals.get(camera_child_of.0)?;
        let (_, dest_transform) = portals.get(source_for.0)?;

        camera_transform.translation = player_camera_transform.translation
            + (dest_transform.translation - source_transform.translation);
        camera_transform.rotation = player_camera_transform.rotation;
        camera_transform.rotate_around(
            dest_transform.translation,
            dest_transform.rotation * source_transform.rotation.inverse(),
        );
    }

    Ok(())
}

pub fn update_camera_projections(
    mut portal_cameras: Populated<(&Transform, &PortalCameraChildOf, &mut Projection)>,
    portals: Populated<(&Targeting, &GlobalTransform), With<PortalCameraChild>>,
) -> Result {
    for (camera_transform, camera_child_of, projection) in portal_cameras.iter_mut() {
        let projection: &mut ObliquePerspectiveProjection = match projection.into_inner() {
            Projection::Custom(custom_projection) => custom_projection.downcast_mut().unwrap(),
            _ => panic!("Expected portal camera to have a custom projection."),
        };

        let (portal_source_for, _) = portals.get(camera_child_of.0)?;
        let (_, dest_transform) = portals.get(portal_source_for.0)?;

        let world_from_view = camera_transform.compute_matrix();
        let view_from_world = world_from_view.inverse();
        let adj_view_from_world = view_from_world.determinant() * world_from_view;

        let sign = dest_transform
            .forward()
            .dot(dest_transform.translation() - camera_transform.translation)
            .signum();

        let v_dest_translation = view_from_world.transform_point3(dest_transform.translation());
        let v_dest_normal = adj_view_from_world
            .transpose()
            .transform_vector3(dest_transform.forward().as_vec3())
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

    Ok(())
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

pub fn portal_teleport(
    rapier_context: ReadRapierContext,
    mut commands: Commands,
    portals: Populated<(Entity, &Targeting, &GlobalTransform), With<TargetedBy>>,
    mut rigid_bodies: Query<
        (
            Entity,
            &mut Transform,
            Option<&OldTranslation>,
            Option<&mut PlayerVelocity>,
        ),
        (With<RigidBody>, Changed<Transform>, Without<ChildOf>),
    >,
) -> Result {
    for (body_ent, mut body_transform, old_translation, player_velocity) in rigid_bodies.iter_mut()
    {
        if let Some(old_translation) = old_translation {
            'each_portal: for (portal_ent, portal_source_for, source_transform) in portals.iter() {
                if rapier_context
                    .single()?
                    .intersection_pair(portal_ent, body_ent)
                    != Some(true)
                {
                    continue 'each_portal;
                }

                let (_, _, dest_transform) = portals.get(portal_source_for.0)?;

                let world_from_portal = source_transform.compute_matrix();
                let portal_from_world = world_from_portal.inverse();
                let world_from_dest_portal = dest_transform.compute_matrix();

                let relative_to_portal =
                    portal_from_world.project_point3(body_transform.translation);
                let old_relative_to_portal = portal_from_world.project_point3(old_translation.0);

                let dot_sign = Vec3::NEG_Z.dot(relative_to_portal).signum();
                let old_dot_sign = Vec3::NEG_Z.dot(old_relative_to_portal).signum();

                if dot_sign != old_dot_sign {
                    body_transform.translation = world_from_dest_portal.project_point3(
                        portal_from_world.project_point3(body_transform.translation),
                    );

                    let rotational_offset =
                        source_transform.rotation().inverse() * dest_transform.rotation();

                    body_transform.rotation *= rotational_offset;

                    if let Some(mut player_velocity) = player_velocity {
                        player_velocity.0 = rotational_offset.mul_vec3(player_velocity.0);
                    }
                }

                break 'each_portal;
            }
        }

        commands
            .entity(body_ent)
            .insert(OldTranslation(body_transform.translation));
    }

    Ok(())
}
