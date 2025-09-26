use std::u32;

use bevy::{
    asset::RenderAssetUsages,
    core_pipeline::tonemapping::Tonemapping,
    prelude::*,
    render::{
        Extract, Render, RenderApp, RenderSet,
        camera::{CameraProjection, extract_cameras},
        mesh::{Indices, PrimitiveTopology},
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
    anyhow::anyhow,
    brush::{BrushPlane, ConvexHull},
    bsp::{BspBrush, BspBrushesAsset},
    geometry::Brushes,
    prelude::*,
};
use itertools::Itertools;
use nil::ShortToString;
use try_partialord::TrySort;

use crate::{
    PORTAL_RENDER_LAYER_1,
    class::{TargetedBy, Targeting},
    player::{FOV, PlayerVelocity, PlayerWorldCameraMarker},
    projections::ObliquePerspectiveProjection,
    special_materials::PortalMaterial,
};

pub struct PortalPlugin;

impl Plugin for PortalPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<PortalClass>()
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

#[derive(Component)]
pub struct OldTranslation(Vec3);

#[solid_class(
    base(Transform, Target, Targetable,),
    hooks(SpawnHooks::new()),
    classname("func_world_portal")
)]
pub struct PortalClass;

struct VertexWithPlaneIndices {
    index_1: usize,
    index_2: usize,
    vertex: Vec3,
}

impl VertexWithPlaneIndices {
    fn new(index_1: usize, index_2: usize, vertex: Vec3) -> Self {
        Self {
            index_1,
            index_2,
            vertex,
        }
    }

    fn indices(&self) -> [usize; 2] {
        [self.index_1, self.index_2]
    }
}

impl PartialEq for VertexWithPlaneIndices {
    fn eq(&self, other: &Self) -> bool {
        (self.index_1 == other.index_1 && self.index_2 == other.index_2)
            || (self.index_1 == other.index_2 && self.index_2 == other.index_1)
    }
}

pub fn setup_portals(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<PortalMaterial>>,
    fov: Res<FOV>,
    bsp_brush_assets: Res<Assets<BspBrushesAsset>>,
    mut portals: Populated<
        (
            Entity,
            &Targetable,
            Option<&Brushes>,
            &mut Transform,
            &Children,
        ),
        (
            Without<PortalCameraChild>,
            With<PortalClass>,
            With<Children>,
        ),
    >,
    windows: Query<&Window, With<PrimaryWindow>>,
) -> Result {
    let window = windows.single()?;
    let fov = fov.into_inner();

    let size = Extent3d {
        width: window.physical_width(),
        height: window.physical_height(),
        ..default()
    };

    for (portal_ent, targetable, portal_brushes, mut portal_transform, portal_children) in
        portals.iter_mut()
    {
        let portal_name = format!(
            "{} with targetname `{}`",
            portal_ent,
            targetable
                .targetname
                .clone()
                .unwrap_or("[NO targetname]".s()),
        );

        let Some(portal_brushes) = portal_brushes else {
            error!("Portal {} has no brushes!", portal_name);
            commands.entity(portal_ent).remove::<PortalClass>();
            continue;
        };

        let portal_brush: &BspBrush = match portal_brushes {
            Brushes::Bsp(handle) => {
                let bsp_brushes_asset = bsp_brush_assets.get(handle).ok_or(anyhow!(
                    "Unable to get bsp brushes for portal {}.",
                    portal_name
                ))?;

                let brushes = &bsp_brushes_asset.brushes;

                if brushes.len() == 0 {
                    return Err(anyhow!(
                        "Portal {} has 0 brushes when it should have 1.",
                        portal_name,
                    )
                    .into());
                } else if brushes.len() > 1 {
                    error!(
                        "Portal {} has {} brushes when it should only have 1. This is undefined behavior.",
                        portal_name,
                        brushes.len(),
                    )
                }

                &brushes[0]
            }
            _ => {
                return Err(anyhow!(
                    "Expected portal {}'s brushes to be in BSP format, but they weren't.",
                    portal_name
                )
                .into());
            }
        };

        // Remove all of this portal's brushes.
        for child in portal_children {
            commands.entity(*child).despawn();
        }
        let portal_center = portal_brush.center().as_vec3();

        // Get a every point where the portal's plane intersects with the portal's brush.
        let mut vertices_with_indices: Vec<VertexWithPlaneIndices> = Vec::new();
        let mut smallest_index = usize::MAX;
        let portal_plane = BrushPlane {
            normal: portal_transform.forward().as_dvec3(),
            distance: portal_transform
                .forward()
                .dot(portal_transform.translation - portal_center) as f64,
        };
        // Do this after getting the portal plane or else the portal plane will be wrong.
        portal_transform.translation = portal_center;
        for combination in portal_brush.planes().enumerate().combinations(2) {
            let (plane_1_index, plane_1) = combination[0];
            let (plane_2_index, plane_2) = combination[1];

            let Some(vertex) =
                BrushPlane::calculate_intersection_point([&portal_plane, plane_1, plane_2])
            else {
                continue;
            };

            if !portal_brush.contains_point(vertex) {
                info!("Filtered out point {}", vertex);
                continue;
            }

            let element =
                VertexWithPlaneIndices::new(plane_1_index, plane_2_index, vertex.as_vec3());

            if !vertices_with_indices.contains(&element) {
                vertices_with_indices.push(element);
                smallest_index = smallest_index.min(plane_1_index.min(plane_2_index));
            };
        }

        // PartialOrd sort based on which vertices share planes with one another.
        vertices_with_indices.try_sort_by(|a, b| {
            let mut common_index = None;

            for index in a.indices() {
                if b.indices().contains(&index) {
                    if common_index.is_some() {
                        // Then both indices are the same
                        return Some(false);
                    }

                    common_index = Some(index);
                }
            }

            let common_index = common_index?;
            let all_indices = [a.indices(), b.indices()].concat();
            let uncommon_indices = all_indices
                .iter()
                .filter(|i| **i != common_index)
                .collect_vec();

            if common_index == smallest_index {
                return Some(uncommon_indices[0] > uncommon_indices[1]);
            }

            Some(uncommon_indices[0] < uncommon_indices[1])
        })?;

        let world_from_portal = portal_transform.compute_matrix().inverse();

        let mut indices = Vec::new();
        let vertices = vertices_with_indices
            .iter()
            .map(|v| world_from_portal.project_point3(v.vertex))
            .collect_vec();

        for index in 2..vertices.len() as u32 {
            indices.extend_from_slice(&[0, index - 1, index]);
        }

        let portal_mesh = Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        )
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, vertices)
        .with_inserted_indices(Indices::U32(indices));

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

        commands.entity(portal_ent).insert((
            Mesh3d(meshes.add(portal_mesh)),
            MeshMaterial3d(portal_material_handle),
            RenderLayers::layer(PORTAL_RENDER_LAYER_1),
            // Collider::cuboid(
            //     portal_surface.width,
            //     portal_surface.height,
            //     near_plane_diagonal_radius,
            // ),
            //Sensor,
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
                perspective: PerspectiveProjection {
                    fov: fov.into(),
                    ..default()
                },
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
    mut portal_cameras: Populated<(&mut Transform, &PortalCameraChildOf), Without<PortalClass>>,
    portals: Populated<(&Targeting, &Transform), (With<PortalClass>, With<PortalCameraChild>)>,
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
