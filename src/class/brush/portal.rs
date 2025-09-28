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
        view::{ExtractedView, RenderLayers, ViewTarget, VisibilitySystems},
    },
    window::PrimaryWindow,
};
use bevy_rapier3d::{
    plugin::{RapierTransformPropagateSet, systems::update_character_controls},
    prelude::{Collider, ReadRapierContext, RigidBody, Sensor},
};
use bevy_trenchbroom::{
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
    player::{FOV, PlayerControllerMarker, PlayerVelocity, PlayerWorldCameraMarker},
    projections::ObliquePerspectiveProjection,
    special_materials::PortalMaterial,
};

pub struct PortalPlugin;

impl Plugin for PortalPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<PortalClass>()
            .insert_resource(PortalDepth::default())
            .add_systems(PreUpdate, update_portal_texture_size)
            .add_systems(
                PostUpdate,
                (
                    setup_portals,
                    update_portal_depth,
                    (
                        portal_teleport
                            .after(update_character_controls)
                            .before(RapierTransformPropagateSet),
                        update_mesh_visibility.before(VisibilitySystems::VisibilityPropagate),
                    )
                        .chain(),
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

/// The distance between all portal's centers and their meshes.
#[derive(Resource, Default)]
pub struct PortalDepth(f32);

#[derive(Component)]
pub enum PortalMeshSide {
    Front,
    Back,
}

/// Relationship pointing to the entity (portal) that this entity (camera) renders to.
#[derive(Component)]
#[component(immutable)]
#[relationship(relationship_target = PortalCameraChild)]
pub struct PortalCameraChildOf(pub Entity);

/// Relationship pointing to the entity (camera) that render to this entity (portal).
#[derive(Component)]
#[relationship_target(relationship = PortalCameraChildOf, linked_spawn)]
pub struct PortalCameraChild(Entity);

/// The translation for the previous phyisics step for any entity with a rigidbody.
/// Only updated when the entity is near a portal.
#[derive(Component)]
pub struct OldTranslation(Vec3);

/// Marks whether or not this entity (portal) was attepted to be setup, not necessarily that it was, as the setup can fail.
#[derive(Component)]
pub struct PortalInitializedMarker;

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
    bsp_brush_assets: Res<Assets<BspBrushesAsset>>,
    fov: Res<FOV>,
    portal_depth: Res<PortalDepth>,
    mut portals: Populated<
        (
            Entity,
            &Targetable,
            Option<&Brushes>,
            &mut Transform,
            &Children,
        ),
        (
            Without<PortalInitializedMarker>,
            With<PortalClass>,
            With<Children>,
        ),
    >,
    windows: Query<&Window, With<PrimaryWindow>>,
) -> Result {
    let fov = fov.into_inner();
    let window = windows.single()?;

    let size = Extent3d {
        width: window.physical_width(),
        height: window.physical_height(),
        ..default()
    };

    'each_portal: for (
        portal_ent,
        targetable,
        portal_brushes,
        mut portal_transform,
        portal_children,
    ) in portals.iter_mut()
    {
        commands.entity(portal_ent).insert(PortalInitializedMarker);

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
            continue;
        };

        let portal_brush: &BspBrush = match portal_brushes {
            Brushes::Bsp(handle) => {
                let Some(bsp_brushes_asset) = bsp_brush_assets.get(handle) else {
                    error!("Unable to get bsp brushes for portal {}.", portal_name);
                    continue 'each_portal;
                };

                let brushes = &bsp_brushes_asset.brushes;

                if brushes.len() == 0 {
                    error!(
                        "Portal {} has 0 brushes when it should have 1.",
                        portal_name,
                    );
                    continue 'each_portal;
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
                error!(
                    "Expected portal {}'s brushes to be in BSP format, but they weren't.",
                    portal_name
                );
                continue 'each_portal;
            }
        };

        // Remove all of this portal's brushes.
        for child in portal_children {
            commands.entity(*child).despawn();
        }

        let portal_center = portal_brush.center().as_vec3();
        let portal_plane = BrushPlane {
            normal: portal_transform.forward().as_dvec3(),
            distance: portal_transform
                .forward()
                .dot(portal_transform.translation - portal_center) as f64,
        };

        if !portal_brush.contains_plane(&portal_plane) {
            error!(
                "Portal {}'s plane is not contained within its brush.",
                portal_name
            );
            continue 'each_portal;
        }

        // Do this after getting the portal plane or else the portal plane will be wrong.
        portal_transform.translation = portal_center;

        let world_from_portal = portal_transform.compute_matrix().inverse();
        let portal_mesh_front;
        let portal_mesh_back;
        let portal_collider;
        if let Some((from, to)) = portal_brush.as_cuboid() {
            let half_extents: Vec3A = 0.5 * (to - from).as_vec3a();

            // From bevy_render primitives mod.rs;
            let relative_radius = |normal: Vec3| -> f32 {
                let normal: Vec3A = normal.into();
                Vec3A::new(
                    normal.dot(Mat3A::IDENTITY.x_axis),
                    normal.dot(Mat3A::IDENTITY.y_axis),
                    normal.dot(Mat3A::IDENTITY.z_axis),
                )
                .abs()
                .dot(half_extents)
            };

            let width = relative_radius(*portal_transform.right());
            let height = relative_radius(*portal_transform.up());

            portal_mesh_front = meshes.add(Plane3d::new(Vec3::NEG_Z, Vec2::new(width, height)));
            portal_mesh_back = meshes.add(Plane3d::new(Vec3::Z, Vec2::new(width, height)));
            portal_collider = Collider::cuboid(width, height, 2.0);
        } else {
            // Get a every point where the portal's plane intersects with the portal's brush.
            let mut vertices_with_indices: Vec<VertexWithPlaneIndices> = Vec::new();
            let mut smallest_index = usize::MAX;
            for combination in portal_brush.planes().enumerate().combinations(2) {
                let (plane_1_index, plane_1) = combination[0];
                let (plane_2_index, plane_2) = combination[1];

                let Some(vertex) =
                    BrushPlane::calculate_intersection_point([&portal_plane, plane_1, plane_2])
                else {
                    continue;
                };

                if !portal_brush.contains_point(vertex) {
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
            match vertices_with_indices.try_sort_by(|a, b| {
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
            }) {
                Ok(_) => (),
                Err(e) => {
                    error!("{}", e);
                    continue 'each_portal;
                }
            }

            // ConvexPolygon uses a generic const in order to determine side count.
            // TODO: Change in bevy 0.17.0 where the above is no longer the case.
            let mut indices = Vec::new();
            let vertices = vertices_with_indices
                .iter()
                .map(|v| world_from_portal.project_point3(v.vertex))
                .collect_vec();

            for index in 2..vertices.len() as u32 {
                indices.extend_from_slice(&[0, index - 1, index]);
            }

            let mut portal_mesh = Mesh::new(
                PrimitiveTopology::TriangleList,
                RenderAssetUsages::default(),
            )
            .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, vertices)
            .with_inserted_indices(Indices::U32(indices));

            // No idea if this works <3
            portal_mesh_front = meshes.add(portal_mesh.clone());
            portal_mesh.invert_winding()?;
            portal_mesh_back = meshes.add(portal_mesh);
            portal_collider = todo!();
        }

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
        let material = MeshMaterial3d(portal_material_handle);
        let render_layer = RenderLayers::layer(PORTAL_RENDER_LAYER_1);

        commands
            .entity(portal_ent)
            .insert((portal_collider, Sensor))
            .with_children(|portal| {
                portal.spawn((
                    Mesh3d(portal_mesh_front),
                    material.clone(),
                    render_layer.clone(),
                    PortalMeshSide::Front,
                    Transform::from_xyz(0.0, 0.0, portal_depth.0),
                ));
                portal.spawn((
                    Mesh3d(portal_mesh_back),
                    material.clone(),
                    render_layer.clone(),
                    PortalMeshSide::Back,
                    Transform::from_xyz(0.0, 0.0, -portal_depth.0),
                ));
            });

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
// Currently not working as the camera needs to be set as active 1 frame early. Which I don't know how to do.
pub fn _update_camera_active(
    mut portal_cameras: Populated<(&mut Camera, &PortalCameraChildOf), Changed<Transform>>,
    portal_meshes: Populated<&ViewVisibility, (With<PortalMeshSide>, With<ChildOf>)>,
    mut portals: Populated<&Children, (Without<PortalCameraChildOf>, With<PortalCameraChild>)>,
) -> Result {
    for (mut camera, camera_child_of) in portal_cameras.iter_mut() {
        let visibility = portals
            .get_mut(camera_child_of.0)?
            .iter()
            .map(|portal_mesh_ent| {
                let view_visibility = portal_meshes.get(portal_mesh_ent).unwrap();
                view_visibility.get()
            })
            .fold(false, |a, b| a || b);

        if visibility != camera.is_active {
            camera.is_active = visibility;
        }
    }

    Ok(())
}

/// Hide and unhide portal meshes based on which side of the portal the player is on.
pub fn update_mesh_visibility(
    player: Single<
        &Transform,
        (
            Changed<Transform>,
            With<PlayerControllerMarker>,
            Without<PortalClass>,
        ),
    >,
    portals: Populated<
        (&Transform, &Children),
        (With<PortalClass>, Without<PlayerControllerMarker>),
    >,
    mut portal_meshes: Populated<(&mut Visibility, &PortalMeshSide), With<ChildOf>>,
) -> Result {
    let player_transform = player.into_inner();

    for (portal_transform, portal_children) in portals.iter() {
        for portal_mesh_ent in portal_children {
            let (mut mesh_visibility, mesh_side) = portal_meshes.get_mut(*portal_mesh_ent)?;

            if portal_transform
                .forward()
                .dot(player_transform.translation - portal_transform.translation)
                .is_sign_positive()
            {
                match mesh_side {
                    PortalMeshSide::Front => *mesh_visibility = Visibility::Inherited,
                    PortalMeshSide::Back => *mesh_visibility = Visibility::Hidden,
                }
            } else {
                match mesh_side {
                    PortalMeshSide::Front => *mesh_visibility = Visibility::Hidden,
                    PortalMeshSide::Back => *mesh_visibility = Visibility::Inherited,
                }
            }
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

/// Modify the portal's camera's projections such that the projection's near clipping planes
/// align with the portal's meshes. This fixes an issue where objects can lie between the portal's destination
/// and its camera causing the object to be appear on the portal's mesh when it shouldn't.
pub fn update_camera_projections(
    mut portal_cameras: Populated<
        (&Transform, &PortalCameraChildOf, &mut Projection),
        (Changed<Transform>, Without<PlayerWorldCameraMarker>),
    >,
    portals: Populated<(&Targeting, &GlobalTransform), With<PortalCameraChild>>,
    portal_depth: Res<PortalDepth>,
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

        let v_plane_distance = -v_dest_translation.dot(v_dest_normal);

        if v_plane_distance.abs() > portal_depth.0 {
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

/// Recalculate the projection matrix in the extracted view because it's currently from a frame ago.
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

/// Recalculate the distance need between a portal's center and its meshes in order to avoid clipping the near plane.
fn update_portal_depth(
    mut portal_depth: ResMut<PortalDepth>,
    player_camera: Single<&Projection, (Changed<Projection>, With<PlayerWorldCameraMarker>)>,
    mut portal_meshes: Query<(&mut Transform, &PortalMeshSide)>,
) {
    let player_camera_projection = player_camera.into_inner();

    let (fov, near, aspect_ratio) = match player_camera_projection {
        Projection::Perspective(perspective_projection) => (
            perspective_projection.fov,
            perspective_projection.near,
            perspective_projection.aspect_ratio,
        ),
        _ => panic!("Expected player to have a perspective projection."),
    };

    let near_plane_half_height = near * (fov / 2.0).tan();
    let near_plane_half_width = near_plane_half_height * aspect_ratio;
    portal_depth.0 = Vec3::new(near_plane_half_width, near_plane_half_height, near).length();

    for (mut transform, side) in portal_meshes.iter_mut() {
        match side {
            PortalMeshSide::Front => transform.translation = Vec3::new(0.0, 0.0, portal_depth.0),
            PortalMeshSide::Back => transform.translation = Vec3::new(0.0, 0.0, -portal_depth.0),
        }
    }
}
