
#version 410

//many thanks to Seed of Andromeda's Star Blog
//https://www.seedofandromeda.com/blogs/49-procedural-gas-giant-rendering-with-gpu-noise

// inputs: texture coordinates, and view and light directions in tangent space
in vec2 st;
in vec3 normal_view;
in vec3 position_view;

// the normal map texture
uniform sampler2D normal_map;
uniform float bumpDegree;
uniform mat4 view;

// output colour
out vec4 frag_colour;


//-----------------perlinNoise--------------------------


//
// Description : Array and textureless GLSL 2D/3D/4D simplex 
//               noise functions.
//      Author : Ian McEwan, Ashima Arts.
//  Maintainer : stegu
//     Lastmod : 20110822 (ijm)
//     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
//               Distributed under the MIT License. See LICENSE file.
//               https://github.com/ashima/webgl-noise
//               https://github.com/stegu/webgl-noise
// 

vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r)
{
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v)
  { 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
  }
  
  //-------multi octave noise--------------------------
  
float noise(vec3 position, int octaves, float frequency, float persistence) {
    float total = 0.0; // Total value so far
    float maxAmplitude = 0.0; // Accumulates highest theoretical amplitude
    float amplitude = 1.0;
    for (int i = 0; i < octaves; i++) {

        // Get the noise sample
        total += snoise(position * frequency) * amplitude;

        // Make the wavelength twice as small
        frequency *= 2.0;

        // Add to our maximum possible amplitude
        maxAmplitude += amplitude;

        // Reduce amplitude according to persistence for the next octave
        amplitude *= persistence;
    }

    // Scale the result by the maximum amplitude
    return total / maxAmplitude;
}

//-------------------------ridged noise--------------------------------------
float ridgedNoise(vec3 position, int octaves, float frequency, float persistence)

{
    float total = 0.0; // Total value so far
    float maxAmplitude = 0.0; // Accumulates highest theoretical amplitude
    float amplitude = 1.0;
    for (int i = 0; i < octaves; i++) {

        // Get the noise sample
        total += ((1.0 - abs(snoise(position * frequency))) * 2.0 - 1.0) * amplitude;

        // Make the wavelength twice as small
        frequency *= 2.0;

        // Add to our maximum possible amplitude
        maxAmplitude += amplitude;

        // Reduce amplitude according to persistence for the next octave
        amplitude *= persistence;
    }

    // Scale the result by the maximum amplitude
    return total / maxAmplitude;
}

//----------------should be controller of how noise pattern looks like------------------------------------------------

vec2 rotate2D(float x, float y, float theta) {
        float newX = st.x * cos(theta) + st.y* sin(theta);
		float newY = st.x*sin(theta) + st.y*cos(theta);
		
		return vec2(newX, newY);
}

vec2 traslate2D(float x, float y, float delta) { 
        float newX = st.x + delta * 10; //encourage x axis translation
		float newY = st.y + delta;
		
		return vec2(newX, newY);
}


//-----------------------------------------------------------------


void main() {
    
	//diffuse color
	
	//vec3 lightPos_world  = vec3 (10.0, -10.0, 0.0); // half jupi
	vec3 lightPos_world  = vec3 (-100.0, 100.0, 100.0); 
	vec3 Ld = vec3 (0.8, 0.8, 0.8); // dull diffuse light colour:roughness of surface
	
	// raise light position to eye space
	vec3 lightPos_view = vec3 (view * vec4 (lightPos_world, 1.0));
	vec3 lightDir = normalize (lightPos_view - position_view);
	float NdotL = clamp( dot (normal_view, lightDir), 0, 1) ; //give surface a direction from surface to light, compare and produce the factor needed

	
	//time factor
	//float bd = bumpDegree *0.01 < 1.0? bumpDegree * 0.001 : 0.0;
	float bd = bumpDegree *0.001;
	
	vec3 position = normal_view + vec3(bd, 0.0, bd);

	
	//-------------------------------
	
	
	 //magic number 100.0 and 0.01 control how large-range influence noise has, 
	 //100.0: increase the frequency of noise happens - otherwise noise might have too large wavelength
	 //0.01: decrease amplitude of noise's influence, because n would be a number [-1, 1], whereas our coord [0,1]
	 //tinkle them and got best result emperically
	 
	 //+ridged
	 float n1 = noise(position , 6, 10.0, 0.8) * 0.01; //similar to simplex noise's magic numbers, 10.0 is frequency
     float n2 = (ridgedNoise(position, 5, 580.0, 0.75) * 0.015 - 0.01 ) * 0.01;
	
	//----------small----storm---------------------------
	// Get the three threshold samples

    float s = 0.75; //number of storms
    float t1 = snoise(position * 10.0) - s; //
    float t2 = snoise((position + 1600.0) * 1.0) - s; // 
    float t3 = snoise((position + 3200.0) * 2.0) - s;

     // Intersect them and get rid of negatives
     float threshold = max(t1 * t2 * t3, 0.0)*5.0; // threshold determined the place
	 
	 // make them noise
	 float n3 = snoise(position*100.0)*0.1; // magic numver frequency
	 

     float n = n1 + n2 + n3; 
	 
	vec2 newTexCoord = vec2(st.x+n, st.y+n);
	 

	 
	//color from texture
	vec3 color = texture (normal_map, newTexCoord).rgb * NdotL; // use diffuse Kd from texture
	
	
//doesn't changed much...
	frag_colour = vec4(color, 1.0) + vec4(n, n/3, n/6, 0.0); // add a red/brown-ish color mostly for red spot

}