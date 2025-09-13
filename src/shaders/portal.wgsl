#import bevy_pbr::forward_io::VertexOutput

@group(2) @binding(1) var texture: texture_2d<f32>;
@group(2) @binding(2) var texture_sampler: sampler;

@fragment
fn fragment(
    mesh: VertexOutput,
) -> @location(0) vec4<f32> {
    return textureSample(texture, texture_sampler, mesh.uv);
}