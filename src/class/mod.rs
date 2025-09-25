use bevy::prelude::*;
use bevy_trenchbroom::prelude::*;

use crate::class::{brush::BrushPlugin, point::PointPlugin};

pub mod brush;
pub mod point;

pub struct CarrotClassPlugin;

impl Plugin for CarrotClassPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((BrushPlugin, PointPlugin))
            .add_systems(PostUpdate, setup_target_relationships);
    }
}

#[derive(Component)]
#[relationship(relationship_target = TargetedBy)]
pub struct Targeting(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = Targeting)]
pub struct TargetedBy(Vec<Entity>);

pub fn setup_target_relationships(
    mut commands: Commands,
    targeters: Populated<(Entity, &Target), Without<Targeting>>,
    targetables: Populated<(Entity, &Targetable)>,
) {
    for (targeter_ent, target) in targeters.iter() {
        let Some(target) = &target.target else {
            continue;
        };

        for (targetable_ent, targetable) in targetables.iter() {
            let Some(target_name) = &targetable.targetname else {
                continue;
            };

            if target != target_name {
                continue;
            }

            commands
                .entity(targeter_ent)
                .insert(Targeting(targetable_ent));
        }
    }
}
