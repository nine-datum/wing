in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D map;
uniform vec3 worldLight;
uniform vec4 color;
uniform sampler2D texture1;
uniform sampler2D texture2;
uniform sampler2D texture3;
uniform sampler2D texture4;
uniform sampler2D global;

void main (void)
{	
	vec4 m = texture(global, uv);
	vec3 r = texture(texture1, uv).rgb;
	vec3 g = texture(texture2, uv).rgb;
	vec3 b = texture(texture3, uv).rgb;
	vec3 a = texture(texture4, uv).rgb;
	vec3 col = r * m.r + g * m.g + b * m.b + a * m.a;
	out_Color = vec4(color.rgb * col * (dot(worldNormal, -worldLight) + 1) * 0.5, 1);
}