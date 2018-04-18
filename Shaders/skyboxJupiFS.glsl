#version 330

in vec3 texcoords;
uniform samplerCube cubeTex;
out vec4 fragment_colour;

void main () {

    //casual effect
	
	fragment_colour = texture (cubeTex, texcoords);
}