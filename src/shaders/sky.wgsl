#import bevy_pbr::forward_io::VertexOutput
#import bevy_pbr::pbr_fragment::pbr_input_from_vertex_output
#import bevy_pbr::mesh_view_bindings::globals

const pi = radians(180.0);
const tau = radians(360.0);

struct SkyMaterial {
    radius: f32,
}

@group(2) @binding(1) var texture: texture_2d<f32>;
@group(2) @binding(2) var texture_sampler: sampler;

@group(2) @binding(0) var<uniform> material: SkyMaterial;

@fragment 
fn fragment(
    mesh: VertexOutput,
    @builtin(front_facing) is_front: bool,
) -> @location(0) vec4<f32> {
    let pbr_input = pbr_input_from_vertex_output(mesh, is_front, false);
    var normal: vec3f = normalize(pbr_input.V);

    let pitch = acos(-normal.y) / pi;
    let yaw = (atan2(-normal.x, normal.z) + pi) / tau;
    let uv = vec2f(yaw, pitch);

    return textureSample(texture, texture_sampler, uv);
}