in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec3 worldLight;
uniform vec4 color;
uniform vec3 time;

void main (void)
{
  vec2 guv = vec2(sin(time.x), cos(time.x));
  vec2 puv = vec2(sin(guv.x), cos(guv.y)) * 0.01;
	out_Color = vec4(color.rgb * texture(texture2d, uv + puv).rgb * (dot(worldNormal, -worldLight) + 1) * 0.5, 1);
}
