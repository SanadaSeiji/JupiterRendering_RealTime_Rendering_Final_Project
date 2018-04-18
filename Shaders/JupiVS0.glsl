#version 410


layout(location = 0) in vec3 vertex_position;
layout(location = 1) in vec3 vertex_normal;
layout(location = 2) in vec2 texture_coord;
layout(location = 3) in vec4 vtangent;

uniform mat4 model, view, proj;

out vec2 st;
out vec3 normal_view;
out vec3 position_view;

void main() {
	gl_Position = proj * view * model * vec4 (vertex_position, 1.0);
	st = texture_coord;
	normal_view =  (view * model * vec4 (vertex_normal, 1.0)).xyz;
	position_view =  (view * model * vec4 (vertex_position, 1.0)).xyz;

	
}