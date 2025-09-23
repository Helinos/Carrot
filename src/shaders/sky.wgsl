#import bevy_pbr::{
    forward_io::VertexOutput,
    pbr_fragment::pbr_input_from_vertex_output,
    mesh_view_bindings::{
        globals,
        view,
    }
}

#import bevy_render::maths::PI
#import bevy_render::maths::PI_2 as TAU

const pi = radians(180.0);
const tau = radians(360.0);

@group(2) @binding(0) var texture: texture_2d<f32>;
@group(2) @binding(1) var texture_sampler: sampler;

@fragment 
fn fragment(
    mesh: VertexOutput,
    @builtin(front_facing) is_front: bool,
) -> @location(0) vec4<f32> {
    let pbr_input = pbr_input_from_vertex_output(mesh, is_front, false);
    let normal = normalize(pbr_input.V);

    let pitch = acos(-normal.y) / PI;
    let yaw = (atan2(-normal.x, normal.z) + PI) / TAU;
    let uv = vec2f(yaw, pitch);

    var color = textureSample(texture, texture_sampler, uv);
    return color;
}