in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec3 worldLight;
uniform vec4 color;

void main (void)
{	
  float l = (dot(worldNormal, -worldLight) + 3) * 0.25;
	out_Color = vec4(color.rgb * texture(texture2d, uv).rgb * l, 1);
}
