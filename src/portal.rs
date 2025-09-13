use bevy::{
    asset::RenderAssetUsages,
    prelude::*,
    render::{
        render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages},
        view::visibility,
    },
    window::PrimaryWindow,
};
use bevy_trenchbroom::{geometry::MapGeometry, prelude::*};

use crate::{materials::PortalMaterial, player::FOV};

pub struct PortalPlugin;

impl Plugin for PortalPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, (update_visible_portals, update_hidden_portals));
        app.register_type::<Worldportal>();
    }
}

#[derive(Component)]
struct PortalVisibleMarker;

#[derive(Component)]
struct PortalCameraMarker;

#[solid_class]
#[derive(Default, Clone)]
struct Worldportal {
    id: String,
    destination_id: String,
}

fn update_visible_portals(
    mut commands: Commands,
    visible_portals: Query<(Entity, &Worldportal), With<PortalVisibleMarker>>,
    map_geometry_query: Query<(Entity, &ViewVisibility, &ChildOf), With<MapGeometry>>,
    all_portals: Query<(Entity, &Worldportal)>,
    portal_cameras: Query<Entity, (With<PortalCameraMarker>, With<ChildOf>)>,
) {
    let no_longer_visible =
        visible_portals
            .iter()
            .filter_map(|(source_portal_ent, source_portal)| {
                let Some(brush_entity) = map_geometry_query
                    .iter()
                    .find(|(_, visibility, child_of)| {
                        child_of.parent() == source_portal_ent && !visibility.get()
                    })
                    .map(|(brush_entity, _, _)| brush_entity)
                else {
                    return None;
                };

                Some((source_portal_ent, source_portal, brush_entity))
            });

    no_longer_visible.for_each(|(source_portal_entity, source_portal, brush_entity)| {
        commands
            .entity(source_portal_entity)
            .remove::<PortalVisibleMarker>();

        commands.entity(brush_entity).remove::<GenericMaterial3d>();

        let Some(dest_portal_entity) = get_destination_portal(source_portal, all_portals) else {
            return;
        };

        commands
            .entity(dest_portal_entity)
            .remove_children(&portal_cameras.iter().collect::<Vec<Entity>>());
    });
}

fn update_hidden_portals(
    mut commands: Commands,
    hidden_portals: Query<(Entity, &Worldportal), Without<PortalVisibleMarker>>,
    geometry_brushes: Query<(Entity, &ViewVisibility, &ChildOf), With<MapGeometry>>,
    all_portals: Query<(Entity, &Worldportal)>,
    fov: Res<FOV>,
    mut images: ResMut<Assets<Image>>,
    windows: Query<&Window, With<PrimaryWindow>>,
    mut materials: ResMut<Assets<PortalMaterial>>,
) -> Result {
    let window = windows.single()?;
    let fov = fov.into_inner().into();

    let now_visible = hidden_portals
        .iter()
        .filter_map(|(source_portal_ent, source_portal)| {
            let Some(brush_entity) = geometry_brushes
                .iter()
                .find(|(_, visibility, child_of)| {
                    child_of.parent() == source_portal_ent && visibility.get()
                })
                .map(|(brush_entity, _, _)| brush_entity)
            else {
                return None;
            };

            Some((source_portal_ent, source_portal, brush_entity))
        });

    now_visible.for_each(|(source_portal_entity, source_portal, brush_entity)| {
        let Some(destination_portal) = get_destination_portal(source_portal, all_portals) else {
            return;
        };

        let size = Extent3d {
            width: window.physical_size().x,
            height: window.physical_size().y,
            ..default()
        };

        let mut image = Image::new_fill(
            size,
            TextureDimension::D2,
            &[0, 0, 0, 0],
            TextureFormat::Rgba8UnormSrgb,
            RenderAssetUsages::default(),
        );
        image.texture_descriptor.usage = TextureUsages::TEXTURE_BINDING
            | TextureUsages::COPY_DST
            | TextureUsages::RENDER_ATTACHMENT;
        let image_handle = images.add(image);

        commands
            .entity(source_portal_entity)
            .insert(PortalVisibleMarker);

        commands
            .entity(brush_entity)
            .remove::<GenericMaterial3d>()
            .insert((MeshMaterial3d(materials.add(PortalMaterial {
                texture: image_handle.clone(),
            })),));

        commands.entity(destination_portal).with_child((
            Camera {
                target: image_handle.clone().into(),
                ..default()
            },
            Camera3d::default(),
            Projection::from(PerspectiveProjection { fov, ..default() }),
            PortalCameraMarker,
        ));
    });

    Ok(())
}

fn get_destination_portal(
    source_portal: &Worldportal,
    all_portals: Query<(Entity, &Worldportal)>,
) -> Option<Entity> {
    all_portals
        .iter()
        .find(|(_, dest_portal)| dest_portal.id == source_portal.destination_id)
        .map(|(portal, _)| portal)
}
