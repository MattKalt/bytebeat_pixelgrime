
/*

	H 4 X X 3 D    U P    F O R    B A R B E C U E

	using the GAv2 tech and a revamped stereo reverb function partly inspired by Feeshbread's Dead Data

	T U R N    Y O U R    V O L U M E    U P !

*/

//Original t, increments one per sample. The reverb, harmonifier, hihat, and snare need this.
T = t,

t *= r8 = 10 / 48,

t-=t/8&128,		//subtle swang
//t-=t/4&256,		//heavy swang
//t-=t/8&128*7,	//5/4 time
//t-=t/4&128*7,	//3/4 time



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

// F is the FX stack, stores memory for use in effects
// Automatically keeps track of what's stored where
// If you see red (NaNs), raise 26e3 higher, or adjust your reverbs' 'dsp' variable (and limiters' lookahead)
// Works best when effects are not inside conditionals (meaning the number of F in use changes)
// But even then, should only create a momentary click/pop (might be more severe for reverb)
T ? 0 : F = r( 4096, 0 ),
// Iterator, resets to 0 at every t
I = 0,


//seq = ( arr, spd, t2=t ) => arr[ (t2 >> spd) % arr.length ],
/*version that lerps:
	the 'x' argument controls the speed at which the slides happen (1=very slidy, 99=almost none, 0=none) */
//seq=(r,s,t2=t,x=0)=>(i=t2/2**s,J=i|0,k=(i-J)**x,L=r.length,x?(1-k)*r[J%L]+k*r[(J+1)%L]:r[J%L]),
/*v slight perf boost:*/
seq=(r,s,t2=t,x=0)=>(i=t2/2**s,J=i|0,L=r.length,x?(k=(i-J)**x,(1-k)*r[J%L]+k*r[(J+1)%L]):r[J%L]),

mp = seq([0,-2,1,-3],18) - 3.5, //this is where the master pitch changes

//t -= seq([t/4&256,0,t/8&128,t/8&128*7,t/4&128*7],9,t>>9,4),

//t -= seq([t/4&896,0,t/4&896,t/8&128,t/8&128,t/4&896,t/8&128,t/4,t/2,t-1],18,t,seq('19100008',18)), //ultraglitch
//t-= seq([t/4&896,0,0,t/4&896,t/8&128,t/4&896,t/8&128,t/4,t/2,t-1],18,t,seq('10000008',18)), //broken :(


//mseq = ( ...x ) => t * 2 ** ( seq(...x) / 12 ), //original
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
s = seq( [ls, 0], 9), // Snare
h = 1 & T * 441/480, // long Hihat
h = seq( [h,h,h,0], 8), //quieter, faster attack

/*
	single input, outputs an array size 2
	t2 and vibratospeed must have the same length (arbitrary)
	requires old lp(), new hp(), and slidy seq() to function
*/

rvs = reverbStereo = ( input, len = 16e3, vibratoSpeed = [1,2], wet = .7, dry = .4, feedb = 1, dsp = 3, lerpx=4, highpass=.03, compSpeed = .1, compThresh = 64, t2 = [T,T], vibratoDepth = 99 ) => (
	o=[],peak=[],//out=[0,0] //can reuse o
	t2.map( (t2val,i) => (
		t2val += vibratoDepth * 2 +  vibratoDepth * sin(T*vibratoSpeed[i]/3e5),
		o[i] = hp( input*dry + feedb * seq( F, 0, I + t2.length*2 + ( t2val % len ) / dsp, lerpx ) || 0, highpass),
		peak[i] = lp( max( compThresh, abs( o[i] ) ), compSpeed, 99 ),
		o[i] *= wet * compThresh / max( peak[i], compThresh )
	)),
	F[ I + ( (T % len) / dsp )|0 ] = o.reduce((a,e)=>a+=e),
	I += 0|(len / dsp),
	o.map((e,i)=>o[i%2]+=e),o //first 2 voices will be double volume
	//o.map((e,i)=>out[i%2]+=e),out
),


lp = lopass = (x, f, bias=1) => ( // f ~= frequency, but not 1:1
	// F[I] is the value of the last sample
	// You will need to change the 'x % 256' if you're using signed or floatbeat
	F[I] = min( max( x % 256, F[I] - f), F[I++] + f * bias) // Clamp the change since last sample between (-f, f)
),



lp2 = (x,f) =>
	F[I] = F[I++] * (1-f) + x * f
,

hp = (x,f) => x - lp2(x,f),


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



// Version 1 of my Synth
// A new version has been developed, but I lost it lmao

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

//13 beats
m1 = r(1,[
	r(4, 0), r(2, [0,1,2,3]), 0
]),

//16 beats
v1 = r(1,[
	1,0,1,0, r(12, 1)
]),

m2 = [8,1,8],

m3 = [7,3,10],

//doubletime
m4 = r(1,[
	0,0,12,10, ht([7,6,3,2,0, r(6, -1 ) ])
]),

v4 = r(1,[
 1, 2, r(6,1)
]),

m5 = r(1,[
 ht([m1, m2, m1, m3, m1, m2]), m4, ht(m2)
]),

v5 = r(1, [
	//r(3, v1), 1, 2, r(6, 1), 0,0,1,0, 1,1,1,1
	r(3, v1), v4, 0,0,1,0, r(4,1)
]),


//13 beats
b1 = r(1,[
	12, 0, r(11, 0)
]),

//16 beats
vb1 = r(1,[
	1,1, r(5, [1,0] ), r(4,1)
]),

b2 = r(1,[
	ht([b1, m2, b1, m3, b1, m2 ]), m4, ht(m2)
]),

vb2 = r(1,[
	r(3, vb1), v4, 1,0,1,0, r(4,1)
]),

a1 = [12,7,13,8],

a2 = r(1,[
	0,7, a1, -2,0, a1, 0,1,11,6
]),

l1a = [5,7,8,10,1,4,-2,3],
//l1a = [5,7,8,10,11,12,13,14],

tn1 = [0x71010559, 0x610105ff],
ta = r(1, [ r(4, 0x712b2321), r(2, 0xa21b02a6 ), 0x63010201 ] ),

vv=[11,12,13,14,15],

bta = "1 h s h 1 h s1hh",
btb = "1 h s h11 h1s1h1",
btc = "1hhhshhh1h1hs1hh",
btd = "1hhhs h11hh1s1h1",
bte = "11h1s1h1h111s1ss",

btf = j( r(1, [ r(240, "0"), r(7, bta), r(13, bta+btb), r(8, btc), r(9, btc+btd+btc+bte), r(2, bte), r(16, 1 ), r(2, "s1s1"), r(8,"1") ] ) ),

drh = on(btf,"h"),
drs = on(btf,"s"),
drk = on(btf,"1"),


av = '0111111',
l3v = '0289569999',

0
),


//----------------- MIXER -----------

L1 = ch => sy( mseq(m5,10,t,0), v5, 11, 1.07, tn1[ch] ) * (1+ch/8),
L2 = [L1(0),L1(1)], //cacheing for performance

L3 = mseq(l1a,15,t,2)^mseq(l1a,15,t,8),

L3 = L3*2&t>>5&31,

//K = (sin(cbrt(199*(t%1024)))*127+(t/2&127))*bt(drk,10,1)**(1/8),
K = (sin(sqrt(6*(t%1024)))*127+(t/2&127))*bt(drk,10,1)**(1/8),
HH = bt([h],10)*seq(drh,10),
SN = bt([s],10)*seq(drs,10),

DR = HH + SN + K * 3,

//L3 = s2s(mseq(l1a,15,t,4)*8)/8,

A1 = sy( mseq(a2,10),[1],10, 4.6, seq(ta,9,t>>9,1)),
//A1 = sy( mseq(a2,10),[1],10, 4.6, 0xa21b02a6),

B1 = mseq(b2,10,t,0) & 255 * seq(vb2,11),
B2 = s2s(B1),
B3 = (-B1 & B2),

//V = rvs( A1 + L2[0], 20e3, [11,12,13,14,15], 2-(t/512%2), .2, 1, (t>>16)+1, 4, .1, .1, 16, [T,T+11e4,T+13e4,T+17e4,T+19e4], 99 ),

A1 *= seq(av,18),
L3 *= seq(l3v,17,t,1)/9,

vl = 2-(t/512%2),
fb=[vl/2+1,vl+.3,vl/2+1,vl],

//Mute
t>>9>=3074&&(L2=fb=[0.5,0],A1=L3=DR=B3=0),
//t>>9>=3586&&(L2=fb=[0.5,0],A1=L3=DR=B3=0),
//t>>9>=4098&&(L2=fb=[0.5,0],A1=L3=DR=B3=0),



V = rvs( vl * (A1/3 + L2[0]/2) + 2 * L3, 9e3, vv, seq(fb,18), .4, 1, 4, 4, .1, .1, 16, [T,T,T,T,T], 99 ),



//Master = ch => lim(
Master = ch => tanh(hp(

( L2[ch]*.6 + B3 + A1/9 + DR ) * min(1,t/1e6+.5)  +

//rv(L2[0]*2, 24e3, 2.1,.2,.5,4,T,.03,2,99,ch+.5)/3

lp2( V[ch], min(1,t/2e5+.1)) * min(1.3,t/6e5+.3)

//, 1, 512, 1, 150 ),
,.001)/max(150,300-t/8e3))*1.1,

[Master(0), Master(1)]





//,a=()=>{throw(I)},a() //Determine size of memory stack to initialize
