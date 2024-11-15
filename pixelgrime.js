
/*

	H 4 X X 3 D    U P    F O R    B A R B E C U E

	using the GAv2 tech and a revamped stereo reverb/chorus function

	T U R N    Y O U R    V O L U M E    U P !

*/

song = (r8,seq,mseq,mp,sp,tra,bt,ls,rvs,lp,lp2,hp,lim2,hm,sy,s2s,on,ht,m1,m2,m3,m4,v1,v4,b1,vb1,a1,bta,btb,btc,btd,bte,btf,L1,L2,L3,B1,B2,B3,HH,SN,DR,vl,vv,Master,l1a=0,l3v=0,vb2=0,tn1=0,drs=0,drh=0,drk=0) => (


//Original t, increments one per sample. The reverb, harmonifier, hihat, and snare need this.
T = t,

t *= r8 = 10 / 48,


//seq = ( arr, spd, t2=t ) => arr[ (t2 >> spd) % arr.length ],
/*version that lerps:
	the 'x' argument controls the speed at which the slides happen (1=very slidy, 99=almost none, 0=none) */
seq=(r,s,t2=t,x=0)=>(i=t2/2**s,J=i|0,L=r.length,x?(k=(i-J)**x,(1-k)*r[J%L]+k*r[(J+1)%L]):r[J%L]),

//----SONG SETTINGS-----

//master pitch
mp = seq([0,-2,1,-3],18) - 3.5,


//t>>9>3800&&(t=1,mp=-8), //the "Bluescreen"
T>93e5&&(t=1,mp=-8), //the "Bluescreen"


//Changing rhythms:
t-=seq([
	b=t/4&896,
	0,
	a=t/8&128,
	t/2&8, //funny tom
	a,
	b,
	a,
	t-1
],18,t,seq('1120004',18)), //glich 3


//-----TOOLS-----

/*
	Repeat x beats of y
	SUPER useful if you're writing complex beats/melodies
	Include this or the Fs won't work (or you could replace r(x, y) with Array(x).fill(y))
	r(1,[arrays]) also serves as a replacement for [arrays].flat()
*/
r = (x, y) => Array( x ).fill( y ).flat( 9 ),

//sp = (str, sep='') => str.split( sep ),
j = (arr, sep='') => arr.join( sep ),

//tra = transpose = (arr, amt) => arr.map(x=>x+amt),
//tra = (x, amt) => Array.isArray(x)? x.map( e => e + amt ) : j( sp(x).map( e => e + amt ) ),

//pretty much deprecated but used in bt()
m = (x, vol=1, dist=0) => ( ( x * vol * ( 1 + dist ) ) % ( 256 * vol ) ) || 0,

/*
	F is the FX stack, stores memory for use in effects
	Automatically keeps track of what's stored where
	If you see red (NaNs), raise 2032 higher, or adjust your reverbs' 'dsp' variable (and limiters' lookahead)
	Works best when effects are not inside conditionals (meaning the number of F in use changes)
	But even then, should only create a momentary click/pop (might be more severe for reverb)
*/
T ? 0 : F = r( 2048, 0 ),
// Index of F, resets to 0 at every t
I = 0,

//melodic sequences without clicks/pops
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
bt = (arr, spd, vel = 2e4, vol = 1, t2 = t, oct = 0) =>
	m(vel / (t2 & (2 ** (spd - oct) / seq( arr, spd ) ) - 1), vol),

ls = sin(T / 9 & T >> 5), // long snare
//s = sin(t>>5), // acoustic-sounding grungy snare
s = seq( [ls, 0], 9), // Snare
h = 1 & T * 441/480, // long Hihat
h = seq( [h,h,h,0], 8), //quieter, faster attack

/*
	Stereo delay with multiple heads (sorta similar to feeshbread's multitap delay in Dead Data)
	single input, outputs an array size 2
	t2 and vibratospeed must have the same length (arbitrary)
	requires old lp(), new hp(), lim2(), r(), and slidy seq() to function
*/

rvs  = ( input, len, vibratoSpeed, dry, wet, dsp, lerpx, highpass, lowpass, compAtk, compRel, compThresh, vibratoDepth,
vcs = vibratoSpeed.length,
t2 = r(vcs,T)
) => (
	o=[],//out=[0,0] //can reuse o
	t2.map( (t2val,i) => (
		t2val += vibratoDepth*2 +  vibratoDepth * sin(T*vibratoSpeed[i]/3e6),
		o[i] = hp(
			input * dry/2 + wet*300/compThresh/vcs * seq(
				F, 0, I + vcs*2-i + ( t2val % len ) / dsp, lerpx
				//F, 0, I + ( t2val % len ) / dsp, lerpx //glitchy
			) || 0
			, highpass
		)
		//,o[i] = lim2(o[i],compAtk,compRel,compThresh) //lim()ing here creates more feedback
	)),
	F[ I + vcs + 1 + ( (T % len) / dsp )|0 ] = lp2(
		o.reduce((a,e,i)=> a=lim2(
			a+e, compAtk,compRel,compThresh/vcs*(1+i/2)
		)
	), lowpass ),
	I += 0|(len / dsp),
	o.map((e,i)=>o[i%2]+=e),o //first 2 voices will be double volume
	//o.map((e,i)=>out[i%2]+=e),out
),

//bad lopass (turns things into triangles rather than sins) but good for compressor
lp = (input, freq, bias=1) => // f ~= frequency, but not 1:1
	// F[I] is the value of the last sample
	// You will need to change the 'x % 256' if you're using signed or floatbeat
	F[I] = min( max( input, F[I] - freq), F[I++] + freq * bias)||0 // Clamp the change since last sample between (-f, f)
,

//better lopass, especially for hi-pass
lp2 = (input,freq) =>
	F[I] = F[I++] * (1-freq) + input * freq
,

hp = (input,freq) => input - lp2(input,freq),

//simple but bad limiter, uses the bad lopass
//release must be >0
lim2 = (input, atk, release, thresh) => (
	input * thresh / lp(
		max( thresh, abs(input))
	,release, atk/release
	)||0
),

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


//XORs the input with its harmonics, controlled by the bits of a number ('tone')
//pretends it uses a wavetable, but doesn't
hm = (x,tone, waveTableSize = 8) => (
	//waveTableSize *= 64 * T / t | 0,
	waveTableSize *= 64 / r8 | 0, //swing-proofed
	r(8).reduce((o,e,i)=>(
		y = (i+1)/2 * x,
		o ^ ( ( 1 & (tone>>i) ) * (i+1)/2 * x ) % waveTableSize
		//o ^ ( ( 1 & (tone>>i) ) * y ) % waveTableSize ^ ( abs( m( tone>>(i+8) * y ) - 128 ) * 2 ) % waveTableSize
		),0
	)
),


/*
	Version 1 of my Synth
A new version has been developed, but this is the one from GAv2 that I have a lot of tones for

Basically just treat this like a black box and fiddle with the knobs at random
For a more detailed exmplanation:
	X, and the First 2 hexes of y, are the fun surprise knobs :)
		Small changes in these values completely change the tone (most of the time)
	The next 2 hexes of y control the harmonifier
	The next hex controls the thump/click/noise of the attack
	The next hex controls the decay
	The next 2 hexes control the lowpass
*/
sy = (melody, velTrack, speed, x, y, ...z)=>
	lp(
		min(
			m(
				hm(
					bt( [x], 10, 6e4, 1, melody, .02* ( (y>>24) & 255 ) )
				, ( y>>16 ) & 255, ...z
				)
			, .9,1
			)
			+ bt( velTrack, speed, 1e3 * ( (y>>12) & 15) )
		, bt( velTrack, speed, 1, 2e4 * ( (y>>8) & 15 ) )
		)
	, y&255
	),


//saw 2 sine
s2s = x => sin( x*PI/64 ) * 126 + 128,

//replaces wanted char with '1' and everything else with '0'
on = (str, wanted) =>
	str.replaceAll( RegExp( '[^' + wanted + ']', 'g' ), '0' ).replaceAll( RegExp( wanted, 'g'), '1' ),

ht = arr => (
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
m1 = [
	r(4, 0), r(2, [0,1,2,3]), 0
],

//16 beats
v1 = [
	1,0,1,0, r(12, 1)
],

m2 = [8,1,8],

m3 = [7,3,10],

//doubletime
m4 = [
	0,0,12,10, ht([7,6,3,2,0, r(6, -1 ) ])
],

v4 = [
 1, 2, r(6,1)
],

m5 = r(1,[
 ht([m1, m2, m1, m3, m1, m2]), m4, ht(m2)
]),

v5 = r(1, [
	//r(3, v1), 1, 2, r(6, 1), 0,0,1,0, 1,1,1,1
	r(3, v1), v4, 0,0,1,0, r(4,1)
]),


//13 beats
b1 = [
	12, 0, r(11, 0)
],

//16 beats
vb1 = [
	1,1, r(5, [1,0] ), r(4,1)
],

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


vs=r(16,399).map((e,i)=>e/1.618**(i/2)),


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
L3 *= seq(l3v,17,t,1)/5,

K = (sin(sqrt(6*(t%1024)))*127+(t/2&127))*bt(drk,10,1)**(1/8),
HH = bt([h],10)*seq(drh,10),
SN = bt([s],10)*seq(drs,10),
//T*r8>>18>5?K=SN=0:0, //drums silent during the glitch part
T*r8>>18==6?K=SN=0:0, //drums silent during the rewind
//T*r8>>18==6?K/=9:0, //drums quieter during the rewind


DR = HH * 1.2 + SN + K * 3,

A1 = sy( mseq(a2,10),[1],10, 4.6, seq(ta,9,t>>9,1)),
//A1 = sy( mseq(a2,10),[1],10, 4.6, 0xa21b02a6),
A1 *= seq(av,18),

B1 = mseq(b2,10,t,0) & 255 * seq(vb2,11),
B2 = s2s(B1),
B3 = (-B1 & B2),



vl = 2-(t/512%2),
//vv = seq( [vl/2+1, vl+.5, vl/2+1, vl+.2], 18),
vv = seq( [vl*4+9, vl*8+4, vl*4+9, vl*8+2], 18),



V = rvs( vl * (A1/3 + L2[0]/2) + L3, 8e3, vs, .1, t<99?2:1.5, max(4,21-(T>>13)/2), 0, .1, .5, 9, 9, 99, 299 ),


//Master = ch => lim(
Master = ch => tanh(hp(
//		dry
( L2[ch]*.6 + B3 + A1/9 + DR ) * 1 +
//		reverb
lp2( V[ch], min(1,t/2e5+.1)) * vv/6
//, 1, 512, 1, 150 ),
,.001)/max(170,300-t/5e3,600-t/999))*1.1,



[Master(0), Master(1)]

), song()


//,a=()=>{throw(I)},a() //Determine size of memory stack to initialize
