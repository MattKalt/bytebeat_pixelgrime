// A collection of effects you can use on _ANY_ variable that changes

//Original t, increments one per sample. The reverb, harmonifier, hihat, and snare need this.
T = t,

t *= r8 = 10 / 48,

//master pitch, CHANGES BELOW
mp = -3.5,
//mp = -5, //for r8=11


// Repeat x beats of y
// SUPER useful if you're writing complex beats/melodies
// Include this or the Fs won't work (or you could replace r(x, y) with Array(x).fill(y))
// r(1,[arrays]) also serves as a replacement for [arrays].flat()
r = repeat = (x, y) => Array( x ).fill( y ).flat( 9 ),

sp = (str, sep='') => str.split( sep ),
j = (arr, sep='') => arr.join( sep ),

//tra = transpose = (arr, amt) => arr.map(x=>x+amt),
tra = transpose = (x, amt) => Array.isArray(x)? x.map( e => e + amt ) : j( sp(x).map( e => e + amt ) ),



// Uses up a lot of chars and isn't /super/ readable, but a major timesaver when creating
// Particularly the NaN handing
m = mix = (x, vol=1, dist=0) => ( ( x * vol * ( 1 + dist ) ) % ( 256 * vol ) ) || 0,

// Waveshaper distortion
// Assumes range is neatly between 0-255; use after limiter
// Negative values make it rounder (though after -.6 it goes beyond {0..255} so there are wraparound artifacts)
ds = (x, amt) => x * (1 - amt) + 127 * ( ( ( x / 127 ) - 1 ) ** 3 + 1 ) * amt,


// F is the FX stack, stores memory for use in effects
// Automatically keeps track of what's stored where
// If you see red (NaNs), raise 26e3 higher, or adjust your reverbs' 'dsp' variable (and limiters' lookahead)
// Works best when effects are not inside conditionals (meaning the number of F in use changes)
// But even then, should only create a momentary click/pop (might be more severe for reverb)
T ? 0 : F = r( 56e3, 0 ),
// Iterator, resets to 0 at every t
I = 0,


//seq = ( arr, spd, t2=t ) => arr[ (t2 >> spd) % arr.length ],
/*version that lerps:
	the 'x' argument controls the speed at which the slides happen (1=very slidy, 99=almost none, 0=none) */
//seq=(r,s,t2=t,x=0)=>(i=t2/2**s,J=i|0,k=(i-J)**x,L=r.length,x?(1-k)*r[J%L]+k*r[(J+1)%L]:r[J%L]),
/*v slight perf boost:*/
seq=(r,s,t2=t,x=0)=>(i=t2/2**s,J=i|0,L=r.length,x?(k=(i-J)**x,(1-k)*r[J%L]+k*r[(J+1)%L]):r[J%L]),



//mseq = ( ...x ) => t * 2 ** ( seq(...x) / 12 ), //original
mseq = ( ...x ) => t * 2 ** ( ( seq(...x) + mp )  / 12 ), //Trucker's Chorus version (specific to this song)
//mseq = (arr, spd, T=t) => t * 2 ** ( ( ((t/9>>17)&3) + arr[ ( ( T >> spd+4 ) + ( 21 & T >> spd ) ) % arr.length] ) / 12 ), //changes all the melodies
//smooth version, works well with seq lerp: (also features Trucker's Chorus)
mseq = ( ...x ) => (
	F[I++] += ( r8 * 2 ** ( ( seq(...x) + mp ) / 12))||0
),



/* The Breakbeat drum machine. This is where the magic happens
It sequences through an array and plays the corresponding number of beats
	(1 = quarter note, 2 = 2 8th notes, etc)
Something interesting happens when you don't use powers of 2, however:
	You get strange and wonderful sounds
the variables 's' and 'h' make it sound like a snare and a hihat, respectively
most sounds are different timbres of the same note
but if you replace 't2' with something other than t, such as any bytebeat melody,
you can apply that timbre to the melody.
Adding / &ing a breakbeat with a melody can also add attack to the notes of the melody
*/
bt = beat = (arr, spd, vel = 2e4, vol = 1, t2 = t, oct = 0) =>
	m(vel / (t2 & (2 ** (spd - oct) / seq( arr, spd ) ) - 1), vol),

ls = sin(T / 9 & T >> 5), // long snare
//s = sin(t>>5), // acoustic-sounding grungy snare
//s = (((t*8/48)>>9) & 1) ? 0 : sin(t / 9 & t >> 5), // Snare
s = seq( [ls, 0], 9), // Snare
S = seq( [ls, 0], 8), // double snare
//s = sin((t | t * .7) >> 4), // quieter snare
//h = 1 & t * 441/480, // long Hihat
h = 1 & T * 441/480, // long Hihat
h = seq( [h,h,h,0], 8), //quieter, faster attack

/*Has a tendency to 'wander', so use a limiter on the final mix
dsp = downsample the bitrate of the reverb, dsp=2 cuts uses half as much space, 3 uses 1/3, etc
	using values besides {1, 2, 3} can produce hi-pitch hum, however
*/
//rv = reverb = ( input, len = 16e3, feedb = .7, dry = .4, wet = 1, dsp = 2, t2=T) => (
//	x = y => I + ( 0|(y % len) / dsp ),
//	bl = t2/dsp - (t2/dsp)|0, //how much to blend between this and next buffer
//	input = input*dry + wet * ( ( 1 - bl ) * F[ x(t2) ] + bl * F[ x( t2 + dsp )] ) || 0,
//	T % dsp ? 0 : F[ x(T) ] = input * feedb,
//	I += 0|(len / dsp),
//	input
//),

/*This reverb uses the lerping seq to save ~20 chars, but the lerp uses ~50 chars
Has a tendency to 'wander', so use a limiter on the final mix
dsp = downsample the bitrate of the reverb, dsp=2 cuts uses half as much space, 3 uses 1/3, etc
	using values besides {1, 2, 3} can produce hi-pitch hum, however
*/
rv = reverb = ( input, len = 16e3, feedb = .7, dry = .4, wet = 1, dsp = 3, t2=T) => (
	input = input*dry + wet * seq( F, 0, I + ( t2 % len ) / dsp, 1 ) || 0,
	T % dsp ? 0 : F[ I + ( (T % len) / dsp )|0 ] = input * feedb,
	//F[ I + ( (T % len) / dsp )|0 ] = input * feedb ** (1/dsp), //higher dsp adds dampening
	I += 0|(len / dsp),
	input
),


lp = lopass = (x, f) => ( // f ~= frequency, but not 1:1
	// F[I] is the value of the last sample
	// You will need to change the 'x % 256' if you're using signed or floatbeat
	F[I] = min( max( x % 256, F[I] - f), F[I++] + f) // Clamp the change since last sample between (-f, f)
),

//For a hi-pass sound, m(x)^lp(x) sounds more harsh; lim(x, 9, <# under 256> ) sounds more natural, but is sosig


/*
The limiter is probably the most advanced effect here
Capable of turning /any/ input into Sosig with minimal distortion, if params are set right
Also eliminates offsets (very helpful for reverb)
Ratio is basically infinity and attack is basically instant
But as long as iters = ~3-8, it won't sound clipped
	(lower lookeahead does produce somewhat of a hi-pass sound)
For a subtler effect, lower speed and/or raise thresh
Latency = lookahead, so use 'wet' if you want parallel compression
If your input only has peaks going upward (like anything involving beat() or synth() ):
	then divide speed by 100 and set bias to 99
	(bias )
'p' changes how lookaheads affect the function:
	0, or very large: lower pitches dominate
	0..1: stacatto and trebly
Hum reduction is only really noticeable when speed > 9 and thresh is low
*/
lim = limiter = (input, speed = .1, lookahead = 512, wet = 1, thresh = 9, bias = 9, iters = 4, saturate = 0, p = 0) => {
	x = y => I + 2 + ( T + y|0 ) % lookahead;
	F[ x(0) ] = input; //newest in buffer, equivalent to F[ x(lookahead) ]
	o = F[ x(1) ]; //oldest in buffer
	mi = mx = o;
	for( i=1; i <= iters; i++) { //older to newest
		y = p ? ( i / (iters+1) ) ** p : 0;
		z = F[ x( ( i + sin(i)/2 ) * lookahead / iters ) ]; //sin(i) is for hum reduction
		mi = min( mi, z * (1-y) + o * y );
		mx = max( mx, z * (1-y) + o * y );
	}
	mi = F[ I ] = min( mi, F[ I ] + speed );
	mx = F[ I+1 ] = max( mx, F[ I+1 ] - speed * ( bias + 1 ), mi + ( t ? thresh : 255 ) ); //could probably be 99
	I += 2 + lookahead;
	return ds( ( o - mi ) * 255/(mx-mi), saturate ) * wet + o * (1-wet)
	//return ds( ( o - mi ) * 2/(mx-mi) - 1, saturate ) * wet + o * (1-wet) //for floatbeat

},

// ~5 chars shorter when minified:
/*
lim = limiter = (input, speed = .1, lookahead = 512, wet = 1, thresh = 9, bias = 9, iters = 4, saturate = 0, p = 0) => (
	x = y => I + 2 + ( T + y|0 ) % lookahead,
	F[ x(0) ] = input,
	o = F[ x(1) ], //oldest in buffer
	mx = mi = o,
	r(iters+1).map((e,i) => (
		y = p ? ( i / (iters+1) ) ** p : 0,
		z = F[ x( ( i + sin(i)/2 ) * lookahead / iters ) ], //sin(i) is for hum reduction
		mi = min( mi, z * (1-y) + o * y ),
		mx = max( mx, z * (1-y) + o * y ),
		e)
	),
	mi = F[ I ] = min( mi, F[ I ] + speed ),
	mx = F[ I+1 ] = max( mx, F[ I+1 ] - speed * ( bias + 1 ), mi + ( t ? thresh : 255 ) ),
	F[ x(0) ] = input,
	I += 2 + lookahead,
	ds( ( o - mi ) * 255/(mx-mi), saturate ) * wet + o * (1-wet)
),
*/

//shorter, worse performance, no 'p', no hum reduction
/*
lim2 = limiter = (input, speed = .1, lookahead = 512, wet = .99, thresh = 9, bias = 0, iters = 4, saturate = 0) => (
	x = y => I + 2 + ( T + y|0 ) % lookahead,
	o = F[ x(0) ], //oldest in buffer
	mx = mi = o,
	r(iters+1).map((e,i) => (
		mi = min( mi, F[ x( i * lookahead / iters) ] ),
		mx = max( mx, F[ x( i * lookahead / iters) ] ),
		e)
	),
	mi = F[ I ] = min( mi, F[ I ] + speed ),
	mx = F[ I+1 ] = max( mx, F[ I+1 ] - speed * ( bias + 1 ), mi + ( t ? thresh : 255 ) ),
	F[ x(0) ] = input,
	I += 2 + lookahead,
	ds( ( o - mi ) * 255/(mx-mi), saturate ) * wet + o * (1-wet)
),
*/


//XORs the input with its harmonics, controlled by the bits of a number ('tone')
//pretends it uses a wavetable, but doesn't
hm = harmonify = (x,tone, waveTableSize = 8) => (
	//waveTableSize *= 64 * T / t | 0,
	waveTableSize *= 64 / r8 | 0, //swing-proofed
	r(8).reduce((o,e,i)=>(
		y = (i+1)/2 * x,
		o ^ ( ( 1 & (tone>>i) ) * (i+1)/2 * x ) % waveTableSize
		//o ^ ( ( 1 & (tone>>i) ) * y ) % waveTableSize ^ ( abs( m( tone>>(i+8) * y ) - 128 ) * 2 ) % waveTableSize
		),0
	)
),

//Version with a for-loop instead of reduce(), ~10 chars more after minification
/*
hm = harmonify = (x,tone, waveTableSize = 8) => {
	waveTableSize *= 64 * T / t | 0;
	o = 0;
	//for (i=0; i < log2(tone) + 1; i++) { //flexible size of 'tone'
	for (i=0; i<8; i++) {
		y = (i+1)/2 * x,
		o ^= ( ( 1 & (tone>>i) ) * y ) % waveTableSize
		//o ^= ( ( 1 & (tone>>i) ) * y ) % waveTableSize ^ ( abs( m( tone>>(i+8) * y ) - 128 ) * 2 ) % waveTableSize
	}
	return o;
},
*/

// Instead of computing on the fly, this version computes a wavetable at the start
// Side effects: you can't start the song at any t, and output is always full-volume
// Might not actually improve performance due to how JS handles memory -- further testing needed
//hm = harmonify = (x, tone, waveTableSize = 4 ) => {
//	waveTableSize *= 64 * T / t | 0;
//	//play from the buffer
//	if( F[I] > waveTableSize ) {
//		o = F[ I + 1 + ( x * T / t & waveTableSize - 1) ];
//		I += 1 + waveTableSize;
//		return o
//	}
//	//fill the buffer
//	for (i=0; i<8; i++) {
//		F[ I + 1 + F[I] ] ^= ( 1 & (tone>>i) ) * (i+1)/2 * F[I] * t / T
//	}
//	F[I]++;
//	I += 1 + waveTableSize;
//	//return x //not strictly necessary unless the wavetable size is large enough to notice silence at the start
//},


// Version 1 of my Synth
// A new version has been developed, but

//Basically just treat this like a black box and fiddle with the knobs at random
//For a more detailed exmplanation:
//	X, and the First 2 hexes of y, are the fun surprise knobs :)
//		Small changes in these values completely change the tone (most of the time)
//	The next 2 hexes of y control the harmonifier
// The next hex controls the *thump*/click/noise of the attack
// The next hex controls the decay
// The next 2 hexes control the lowpass
sy = synth = (melody, velTrack, speed, x, y, ...z)=>
	lp(
		min(
			m(
				hm(
					beat( [x], 10, 6e4, 1, melody, .02* ( (y>>24) & 255 ) )
				, ( y>>16 ) & 255, ...z
				)
			, .9,1
			)
			+ beat( velTrack, speed, 1e3 * ( (y>>12) & 15) )
		, beat( velTrack, speed, 1, 2e4 * ( (y>>8) & 15 ) )
		)
	, y&255
	),


//saw 2 sine
s2s = sinify = x => sin( x*PI/64 ) * 126 + 128,

v = vibrato = sin(T>>10)/2,


//replaces wanted char with '1' and everything else with '0'
on = (str, wanted) =>
	str.replaceAll( RegExp( '[^' + wanted + ']', 'g' ), '0' ).replaceAll( RegExp( wanted, 'g'), '1' ),

ht = halftime = arr => (
	arr = r(1,arr), //flatten
	r(arr.length * 2).map( (e,i) => arr[i/2|0] )
),


//------------------ SEQUENCES -----------------------------------

//Do not take any of this out of the 't ? 0' statement
t ? 0 : (

/*
	The form:
	
	r(1,[
		arrays with nesting
	]),

	can be avoided by simply taking the final usable arrays with array.flat() or r(1,array)
*/

m1 = r(1,[
	r(4, 0), r(2, [0,1,2,3])
]),

v1 = r(1,[
	1,0,1,0, r(12, 1)
]),

f1 = [0,8,1,8],

f2 = [0,7,3,10],

//only one not halftime
f3 = r(1,[
	0,0,12,10, ht([7,6,3,2,0, r(6, -1 ), 8,1,8 ])
]),

l1 = r(1,[
 ht([m1, f1, m1, f2, m1, f1]), f3 
]),

v2 = r(1, [
	//r(3, v1), 1, 2, r(6, 1), 0,0,1,0, 1,1,1,1
	r(3, v1), 1, 2, r(6, 1), 0,0,1,0, r(4,1)
]),

0
),


//----------------- MIXER -----------

[

sy( mseq(l1,10), v2, 11, 1.07, 0x71010599)
,
sy( mseq(l1,10), v2, 11, 1.07, 0x61010599)

]