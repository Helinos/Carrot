use bevy::{
    prelude::*,
    render::camera::{CameraProjection, SubCameraView},
};

#[derive(Debug, Clone, Default)]
pub struct ObliquePerspectiveProjection {
    pub view_near_plane: Vec4,
    pub perspective: PerspectiveProjection,
}

impl ObliquePerspectiveProjection {
    pub fn get_clip_from_view_pre_modification(&self) -> Mat4 {
        Mat4::perspective_infinite_reverse_rh(
            self.perspective.fov,
            self.perspective.aspect_ratio,
            self.perspective.near,
        )
    }
}

impl CameraProjection for ObliquePerspectiveProjection {
    fn get_clip_from_view(&self) -> Mat4 {
        let mut clip_from_view = self.get_clip_from_view_pre_modification();

        let v_camera = Vec4::new(
            (self.view_near_plane.x.signum() - clip_from_view.col(2)[0]) / clip_from_view.col(0)[0],
            (self.view_near_plane.y.signum() - clip_from_view.col(2)[1]) / clip_from_view.col(1)[1],
            -1.,
            clip_from_view.col(2)[2] / clip_from_view.col(3)[2],
        );

        let m = -1. / self.view_near_plane.dot(v_camera);

        clip_from_view.col_mut(0)[2] = m * self.view_near_plane.x;
        clip_from_view.col_mut(1)[2] = m * self.view_near_plane.y;
        clip_from_view.col_mut(2)[2] = m * self.view_near_plane.z - 1.0;
        clip_from_view.col_mut(3)[2] = m * self.view_near_plane.w;

        clip_from_view
    }

    fn get_clip_from_view_for_sub(&self, sub_view: &SubCameraView) -> Mat4 {
        self.perspective.get_clip_from_view_for_sub(sub_view)
    }

    fn update(&mut self, width: f32, height: f32) {
        self.perspective.update(width, height);
    }

    fn far(&self) -> f32 {
        self.perspective.far()
    }

    fn get_frustum_corners(&self, z_near: f32, z_far: f32) -> [Vec3A; 8] {
        self.perspective.get_frustum_corners(z_near, z_far)
    }
}
