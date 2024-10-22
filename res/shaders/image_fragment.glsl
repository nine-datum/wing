in vec2 uv;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec4 color;

void main (void)
{	
	vec4 c = texture(texture2d, uv);
	if(c.a < 0.5)
	{
		discard;
	}
	out_Color = c * color;
}