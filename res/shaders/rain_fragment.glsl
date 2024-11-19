in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec3 worldLight;
uniform vec4 color;

void main (void)
{
	out_Color = vec4(0.1, 0.2, 0.3, 1);
}
