in vec3 position;
in vec2 texcoord;
in vec3 normal;

out vec2 uv;
out vec3 worldNormal;
uniform mat4 transform;
uniform mat4 projection;
uniform vec3 time;

void main (void)
{
  uv = texcoord;
  worldNormal = normalize((transform * vec4(normal, 0)).xyz);
  vec3 pos0 = position - vec3(0, 0, position.z);
  vec3 pos = (transform * vec4(pos0, 1)).xyz;
  int ind = int(position.z);
  float ang = ind * 3.14 * 0.25;
  float nx = sin(ang);
  float nz = cos(ang);
  pos = pos + vec3(nx, 1, nz) * time.x;

  gl_Position = projection * vec4(pos, 1);
}
