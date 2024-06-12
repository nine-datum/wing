in vec2 uv;

out vec4 out_Color;

uniform sampler2D texture2d;

void main (void)
{	
	out_Color = vec4(uv.x, uv.y, 0, 1);// texture(texture2d, uv);
}