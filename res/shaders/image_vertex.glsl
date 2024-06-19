in vec3 position;
in vec2 texcoord;

out vec2 uv;
uniform mat4 transform;

void main (void)
{
	uv = texcoord;
	gl_Position = transform * vec4(position.x, position.y, position.z, 1);
}