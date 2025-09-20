use bevy::{
    asset::RenderAssetUsages,
    math::NormedVectorSpace,
    prelude::*,
    render::{
        camera::{CameraProjection, ViewportConversionError},
        primitives::{Aabb, Frustum, HalfSpace},
        render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages},
        view::{self, RenderLayers, VisibilitySystems},
    },
    scene::ron::de,
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
                    (
                        update_portal_cameras
                            .after(RapierTransformPropagateSet)
                            .before(TransformSystem::TransformPropagate),
                        update_portal_camera_frusta.after(VisibilitySystems::UpdateFrusta),
                    )
                        .chain(),
                ),
            );
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

        let portal_rotation = euler_deg_to_quat(source_portal_geometry.normal);
        let dest_portal_rotation = euler_deg_to_quat(dest_portal_geometry.normal);
        let source_transform = Transform::from_rotation(portal_rotation)
            .with_translation(source_portal_aabb.center.into());
        let destination_transform = Transform::from_rotation(dest_portal_rotation)
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
                dest_transform: destination_transform,
            },
        ));

        commands.spawn((
            PortalSurface { visible: false },
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

pub fn update_portal_cameras(
    player_camera: Single<&GlobalTransform, With<WorldCameraMarker>>,
    mut portal_cameras: Populated<
        (&mut Transform, &PortalCamera, &mut Projection, &Camera),
        Without<WorldCameraMarker>,
    >,
) -> Result {
    let player_camera_transform = player_camera.into_inner().compute_transform();

    for (mut transform, portal_camera, mut projection, camera) in portal_cameras.iter_mut() {
        transform.translation = player_camera_transform.translation
            + (portal_camera.dest_transform.translation
                - portal_camera.source_transform.translation);
        transform.rotation = player_camera_transform.rotation;
        transform.rotate_around(
            portal_camera.dest_transform.translation,
            portal_camera.dest_transform.rotation
                * portal_camera.source_transform.rotation.inverse(),
        );

        let projection: &mut ObliquePerspectiveProjection = match projection.into_inner() {
            Projection::Custom(custom_projection) => custom_projection.downcast_mut().unwrap(),
            _ => panic!("Expected portal camera to have a custom projection."),
        };

        let clip_from_view = projection.get_clip_from_view_pre_modification();

        let mut near_plane_normal = Vec3::NEG_Z;

        if near_plane_normal.dot(Vec3::NEG_Z).is_sign_negative() {
            near_plane_normal = -near_plane_normal;
        }

        let Ok(mut near_plane_translation) = world_to_view_port_with_depth(
            camera,
            &transform,
            portal_camera.dest_transform.translation,
            clip_from_view,
        ) else {
            continue;
        };

        // I give up.
        near_plane_translation.z -= 1.5;
        let near_plane_distance = -near_plane_normal.dot(near_plane_translation);

        projection.view_near_plane = near_plane_normal.extend(near_plane_distance);

        // The original world_to_view_port_with_depth gets the computed clip_from_view matrix
        // from the camera which is being modified by this very function. So this performs
        // the same task but instead uses the clip_from_view matrix from before it's modified.
        fn world_to_view_port_with_depth(
            camera: &Camera,
            camera_transform: &Transform,
            world_position: Vec3,
            clip_from_view: Mat4,
        ) -> Result<Vec3, ViewportConversionError> {
            let target_rect = camera
                .logical_viewport_rect()
                .ok_or(ViewportConversionError::NoViewportSize)?;
            let mut ndc_space_coords =
                world_to_ndc(camera_transform, world_position, clip_from_view)
                    .ok_or(ViewportConversionError::InvalidData)?;
            if ndc_space_coords.z < 0.0 {
                return Err(ViewportConversionError::PastNearPlane);
            }
            if ndc_space_coords.z > 1.0 {
                return Err(ViewportConversionError::PastFarPlane);
            }

            let depth = -depth_ndc_to_view_z(ndc_space_coords.z, clip_from_view);

            ndc_space_coords.y = -ndc_space_coords.y;

            let viewport_position = (ndc_space_coords.truncate() + Vec2::ONE) / 2.0
                * target_rect.size()
                + target_rect.min;
            Ok(viewport_position.extend(depth))
        }

        fn world_to_ndc(
            camera_transform: &Transform,
            world_position: Vec3,
            clip_from_view: Mat4,
        ) -> Option<Vec3> {
            let clip_from_world: Mat4 =
                clip_from_view * camera_transform.compute_matrix().inverse();
            let ndc_space_coords: Vec3 = clip_from_world.project_point3(world_position);

            (!ndc_space_coords.is_nan()).then_some(ndc_space_coords)
        }

        pub fn depth_ndc_to_view_z(ndc_depth: f32, clip_from_view: Mat4) -> f32 {
            let near = clip_from_view.w_axis.z;
            -near / ndc_depth
        }
    }

    Ok(())
}

pub fn update_portal_camera_frusta(
    mut portal_cameras: Populated<(&PortalCamera, &mut Frustum, &Transform)>,
) {
    for (portal_camera, mut frustum, transform) in portal_cameras.iter_mut() {
        let mut near_plane_normal = portal_camera.dest_transform.forward();

        if near_plane_normal
            .dot(transform.translation - portal_camera.dest_transform.translation)
            .is_sign_positive()
        {
            near_plane_normal = -near_plane_normal;
        }

        let near_plane_distance = portal_camera
            .dest_transform
            .translation
            .dot(near_plane_normal.as_vec3());

        frustum.half_spaces[4] = HalfSpace::new(near_plane_normal.extend(-near_plane_distance))
    }
}

fn euler_deg_to_quat(euler: Vec3) -> Quat {
    Quat::from_euler(
        EulerRot::YXZ,
        euler.x.to_radians(),
        euler.y.to_radians(),
        euler.z.to_radians(),
    )
}
