// Pan2/Balance2 generalization
// this is a convenience pseudo-ugen that should "do the right thing" with mono or stereo sounds.
// easier than switching between Pan2 and Balance2 every time the number of channels changes.
B2 : UGen {
	*new {
		| rate=\ar in pos=0 level=1 |
		switch(in.size,
			0, {
				^Message(Pan2, rate, [in, pos, level]).value;
			},
			1, {
				^Message(Pan2, rate, [in[0], pos, level]).value;
			},
			2, {
				^Message(Balance2, rate, [in[0], in[1], pos, level]).value;
			},
		);
		("Wrong number of channels for B2:"+in.size.asString).error;
	}
	*ar {
		| in pos=0 level=1 |
		^this.new(\ar, in, pos, level);
	}
	*kr {
		| in pos=0 level=1 |
		^this.new(\kr, in, pos, level);
	}
}

// oscillators

FMPulse : PureUGen { // FM-able PM-able PWM-able band-limited pulse, based off of Nathaniel Virgo's code at http://sccode.org/1-T
	*ar {
		| freq=440 width=0.5 phase=0 mul=1 add=0 |
		^MulAdd(Clipper8.ar(SinOsc.ar(freq, phase, 100) + ((width*200)-100)), mul, add);
	}
	*kr {
		| freq=440 width=0.5 phase=0 mul=1 add=0 |
		^MulAdd(Clipper8.kr(SinOsc.kr(freq, phase, 100) + ((width*200)-100)), mul, add);
	}
}

FMVarSaw : PureUGen { // FM-able VarSaw
	// based off of a design from Alexandros Drymonitis's "Making voltage controlled oscillators in Pure Data" PDF.
	*ar {
		| freq=440 iphase=0 width=0.5 mul=1 add=0 |
		var saw, le, ri;
		width = width.clip(0.0000001, 0.9999999);
		saw = LFSaw.ar(freq, 0, 0.5, 0.5);
		le = (saw/width)*(saw<width);
		ri = (((1-width) - (saw - width))/(1-width))*(saw>width);
		^(((le+ri) * 2)-1);
	}
	*kr {
		| freq=440 iphase=0 width=0.5 mul=1 add=0 |
		var saw, le, ri;
		width = width.clip(0.0000001, 0.9999999);
		saw = LFSaw.kr(freq, 0, 0.5, 0.5);
		le = (saw/width)*(saw<width);
		ri = (((1-width) - (saw - width))/(1-width))*(saw>width);
		^(((le+ri) * 2)-1);
	}
}

// buffer playing

SndP : PureUGen { // 'end' is only for when rate is negative.
	*ar {
		arg bufnum, rate=1, start=0, end=1, trigger=1, loop=0, doneAction=0, numChannels=2;
		var bufframes, stpos, enpos;
		bufframes = BufFrames.kr(bufnum);
		rate = if(rate.isNumber, {
			DC.kr(rate);
		}, rate);
		numChannels = numChannels??{
			if(bufnum.notNil, {
				bufnum.numChannels;
			}, {
				2
			});
		};
		stpos = start*bufframes;
		enpos = end*bufframes;
		^PlayBuf.ar(numChannels, bufnum, rate*BufRateScale.kr(bufnum), trigger, Select.kr(Latch.kr(rate, trigger)>0, [enpos, stpos]), loop, doneAction: doneAction);
	}
}

// SndP : PureUGen {
// 	*ar {
// 		arg bufnum, rate=1, start=0, end=1, trigger=1, doneAction=0, numChannels=2;
// 		var bufframes, line_dur, startpos, endpos, line;
// 		bufframes = BufFrames.kr(bufnum);
// 		line_dur = (bufframes/BufSampleRate.kr(bufnum)*(abs(start-end)))/abs(rate);
// 		rate = if(rate.isNumber, {
// 			DC.kr(rate);
// 		}, rate);
// 		startpos = ((rate>0)*start)+((rate<0)*end);
// 		endpos = ((rate>0)*end)+((rate<0)*start);
// 		line = Env([startpos*bufframes, startpos*bufframes, endpos*bufframes, endpos*bufframes], [0,line_dur,0]).ar(doneAction, trigger, rate.abs.reciprocal);
// 		^BufRd.ar(numChannels, bufnum, line);
// 	}
// }

// SndP : PureUGen {
// 	*ar {
// 		arg bufnum, rate, start=0, end=1, trigger=1, doneAction=0, numChannels=2;
// 		var bufframes, stpos, enpos, line, free;
// 		bufframes = BufFrames.kr(bufnum);
// 		rate = if(rate.isNumber, {
// 			DC.kr(rate);
// 		}, rate);
// 		stpos = start*bufframes;
// 		enpos = end*bufframes;
// 		line = Sweep.ar(trigger, BufSampleRate.ir(bufnum)*rate)+stpos;
// 		// line = Select.ar(Latch.ar(rate, trigger)>0, [enpos-line, stpos+line]);
// 		line = Select.ar(line>0, [enpos-line, stpos+line]);
// 		free = (line<=enpos)*(line>=stpos);
// 		line = line*free;
// 		line = Clip.ar(line, stpos, enpos);
// 		DetectSilence.ar(HPZ1.ar(line)+(free), 0.0001, 0.001, doneAction);
// 		^BufRd.ar(numChannels, bufnum, line);
// 	}
// }

// feedback

Fb1 : UGen { // single-sample feedback
	*new { arg func, maxdelaytime, delaytime = maxdelaytime, numChannels, default=0;
		var buffers, in, out, write;
		var maxdelaysamples, delaysamples, readoffset, writeoffset;
		var sr = SampleRate.ir;
		
		if (maxdelaytime.isNil) {
			maxdelaysamples = 1;
			delaysamples = 0;
			readoffset = 0;
			writeoffset = 0;
		} {
			maxdelaysamples = sr * maxdelaytime - 1;
			delaysamples = { sr * delaytime.value - 1 };
			readoffset = { Dseries(0, 1, inf) % maxdelaysamples };
			writeoffset = { Dseries(0, 1, inf) + delaysamples.value % maxdelaysamples };
		};
	
		numChannels = numChannels ?? { func.value(Silent.ar(1)).asArray.size };
		numChannels = numChannels max: maxdelaytime.asArray.size max: delaytime.asArray.size;
		
		buffers = { LocalBuf(maxdelaysamples + 1).set(default) } ! numChannels;
		in = Dbufrd(buffers, readoffset ! numChannels);
		out = func.value(in.unbubble);
		write = Dbufwr(out, buffers, writeoffset ! numChannels);
		^Duty.ar(SampleDur.ir, 0, write);
	}
}

// analog/drift stuff

Analog0 : PureUGen {
	*new {
		| drift=0.001 |
		^Rand(-1*drift, drift);
	}
}

Analog : Analog0 {
}

Analog1 : PureUGen {
	*new {
		| drift=0.001 |
		^Rand(1-(drift*0.01), 1+(drift*0.01));
	}
}

Analog2 : Analog1 {
}

Drift0 : PureUGen {
	*new {
		| drift=0.001 |
		^LFNoise1.kr(0.001, drift);
	}
}

Drift : Drift0 {
}

Drift1 : PureUGen {
	*new {
		| drift=0.001 |
		^LFNoise1.kr(0.01, drift, 1);
	}
}

// Anas : PureUGen { // 'analog spread' - see http://zynaddsubfx.sourceforge.net/doc_0.html
// 	*new {
// 		| freq spread | // freq is base freq, spread is the Hz spread at 440.
// 		LFNoise1.kr(0.1).exprange((
// 	}
// }

Stereo : PureUGen {
	*ar {
		| input stereo=0 |
		^DelayC.ar(input, 1, [stereo.min(0).abs, stereo.max(0)]);
	}
}

// effects

Reverb1 : PureUGen {
	*ar {
		| in cutoff=3000 wet=0.3 |
		var output = in;
		6.do({
			output = LPF.ar(AllpassN.ar(output, 0.05, 0.05.rand, 1), cutoff);
		});
		^((output * wet) + (in * (1 - wet)));
	}
}

HardGate : PureUGen {
	*ar {
		arg in=0, level=0.5;
		^(in*(in.abs>level));
	}
	*kr {
		arg in, level;
		^this.ar(in, level);
	}
}

AntiClip : PureUGen {
	*ar {
		arg in=0, lo=(-1), hi=1;
		^in*(in>lo)*(in<hi);
	}
	*kr {
		arg in=0, lo=(-1), hi=1;
		^this.kr(in, lo, hi);
	}
}

Skip : UGen {
	*ar {
		| in rate=0 mix=0 maxdelaytime=2 |
		var lb, ns, writing, wenv, reading, pb;
		if(rate.isSequenceableCollection, { rate = rate[0] });
		if(mix.isSequenceableCollection, { mix = mix[0] });
		lb = LocalBuf(maxdelaytime*SampleRate.ir, 2).clear;
		ns = maxdelaytime*BufSampleRate.ir(lb);
		writing = BinaryOpUGen('==', mix, 0);
		wenv = Env.line(0, BufFrames.kr(lb), maxdelaytime).ar(0, mix);
		// wenv = Phasor.ar(writing, writing*BufRateScale.kr(lb), 0, BufFrames.kr(lb));
		reading = BinaryOpUGen('!=', mix, 0);
		// RecordBuf.ar(in, lb, 0, writing, reading, 1, 1);
		BufWr.ar(in, lb, wenv);
		// startPos for PlayBuf was: Latch.kr((wenv-((1/rate)*BufSampleRate.kr(lb))), reading)
		pb = PlayBuf.ar(2, lb, 1*BufRateScale.kr(lb), Impulse.ar(mix.round*rate));
		^Select.ar(mix, [in, pb]);
	}
}

Skip2 : UGen {
	*ar {
		| in trig rate=1 mix=0 maxdelaytime=2 |
		var lb, ns, writing, wenv, reading, pb;
		if(rate.isSequenceableCollection, { rate = rate[0] });
		if(mix.isSequenceableCollection, { mix = mix[0] });
		lb = LocalBuf(maxdelaytime*SampleRate.ir, 2).clear;
		ns = maxdelaytime*BufSampleRate.ir(lb);
		writing = BinaryOpUGen('==', mix, 0);
		wenv = Env.line(0, BufFrames.kr(lb), maxdelaytime).ar(0, mix);
		// wenv = Phasor.ar(writing, writing*BufRateScale.kr(lb), 0, BufFrames.kr(lb));
		reading = BinaryOpUGen('!=', mix, 0);
		// RecordBuf.ar(in, lb, 0, writing, reading, 1, 1);
		BufWr.ar(in, lb, wenv);
		// startPos for PlayBuf was: Latch.kr((wenv-((1/rate)*BufSampleRate.kr(lb))), reading)
		pb = PlayBuf.ar(2, lb, rate*BufRateScale.kr(lb), trig);
		^Select.ar(mix, [in, pb]);
	}
}

RateShift : PureUGen {
	*ar {
		arg in=0, rate=1, maxdelaytime=0.2;
		^DelayC.ar(in, maxdelaytime, Phasor.ar(1, 1-rate, 0, SampleRate.ir*maxdelaytime)/SampleRate.ir);
	}
	*kr {
		arg in=0, rate=1, maxdelaytime=0.2;
		^DelayC.kr(in, maxdelaytime, Phasor.kr(1, 1-rate, 0, SampleRate.ir*maxdelaytime)/SampleRate.ir);
	}
}

PDCos : PureUGen {
	*ar {
		arg in;
		^(in * 2pi).cos;
	}
	*kr {
		arg in;
		^this.ar(in);
	}
}

// LADSPA wrappers!

TransientMangler : UGen {
	*ar {
		arg in, attack=1, sustain=1;
		/*
			> k: Attack speed (-1 to 1)
			> k: Sustain time (-1 to 1)
		*/
		^LADSPA.ar(1, 1206, attack, sustain, in);
	}
}

MultivoiceChorus : UGen {
	*ar {
		arg in, num=4, delayBase=20, voiceSeparation=1, detune=2, lfoFreq=5, outputDB=(-2*num);
		/*
			> k: Number of voices (1 to 8)
			> k: Delay base (ms) (10 to 40)
			> k: Voice separation (ms) (0 to 2)
			> k: Detune (%) (0 to 5)
			> k: LFO frequency (Hz) (2 to 30)
			> k: Output attenuation (dB) (-20 to 0)
		*/
		^LADSPA.ar(1, 1201, num, delayBase, voiceSeparation, detune, lfoFreq, outputDB, in);
	}
}

Flanger : UGen {
	*ar {
		arg in, feedback=0.1, delayBase=10, maxSlowdown=5, lfoFreq=3;
		/*
			> k: Delay base (ms) (0.1 to 25)
			> k: Max slowdown (ms) (0 to 10)
			> k: LFO frequency (Hz) (0.05 to 100)
			> k: Feedback (-1 to 1)
		*/
		^LADSPA.ar(1, 1191, delayBase, maxSlowdown, lfoFreq, feedback, in);
	}
}

Sifter : UGen {
	*ar {
		arg in, size=300;
		/*
			> k: Sift size (1 to 1000)
		*/
		^LADSPA.ar(1, 1210, size, in);
	}
}

Declipper : UGen {
	*ar {
		arg in;
		^LADSPA.ar(1, 1195, in);
	}
}

Divider : UGen {
	*ar {
		arg in, denominator=3;
		/*
			> k: Denominator (1 to 8)
		*/
		^LADSPA.ar(1, 1186, denominator, in);
	}
}

Satan : UGen {
	*ar {
		arg in, decay=5, knee=(-30);
		/*
			> k: Decay time (samples) (2 to 30)
			> k: Knee point (dB) (-90 to 0)
		*/
		^LADSPA.ar(1, 1408, decay, knee, in);
	}
}

Pointercast : UGen {
	*ar {
		arg in, cutoff=1000;
		/*
			# 1910 Pointer cast distortion
			> k: Effect cutoff freq (Hz) (0.0001 to 0.3)
			> k: Dry/wet mix (0 to 1)
		*/
		^LADSPA.ar(1, 1910, cutoff, 1, in);
	}
}

TAPChorusFlanger : UGen {
	*ar {
		arg in, freq=2, depth=50, delay=10, contour=500, phaseShift, dry=0, wet=1;
		/*
			# 2159 TAP Chorus/Flanger
			> k: Frequency [Hz] (0 to 5)
			> k: L/R Phase Shift [deg] (0 to 180)
			> k: Depth [%] (0 to 100)
			> k: Delay [ms] (0 to 100)
			> k: Contour [Hz] (20 to 20000)
			> k: Dry Level [dB] (-90 to 20)
			> k: Wet Level [dB] (-90 to 20)
			> a: Input_L
			> a: Input_R
		*/
		var left, right;
		if(in.isSequenceableCollection, {
			left = in[0];
			right = in[1];
		}, {
			left = in;
			right = in;
		});
		^LADSPA.ar(2, 2159, freq, phaseShift?Rand(0.0, 180.0), depth, delay, contour, dry.remap([0,1],[-90,0]), wet.remap([0,1],[-90,0]), left, right);
	}
}

// these are commented out cuz they seem to crash the server...

// PitchScaler : UGen {
// 	*ar {
// 		arg in, coef=1;
// 		/*
// 			# 1193 Pitch Scaler
// 			> k: Pitch co-efficient (0.5 to 2)
// 		*/
// 		^LADSPA.ar(1, 1193, coef);
// 	}
// }

// PitchScalerHQ : UGen {
// 	*ar {
// 		arg in, coef=1;
// 		/*
// 			# 1194 Higher Quality Pitch Scaler
// 			> k: Pitch co-efficient (0.5 to 2)
// 		*/
// 		^LADSPA.ar(1, 1194, coef);
// 	}
// }

// TAPVibrato : UGen {
// 	*ar {
// 		arg in, freq=4, depth=10, dry=(-90), wet=0;
// 		/*
// 			> k: Frequency [Hz] (0 to 30)
// 			> k: Depth [%] (0 to 20)
// 			> k: Dry Level [dB] (-90 to 20)
// 			> k: Wet Level [dB] (-90 to 20)
// 		*/
// 		^LADSPA.ar(1, 2148, freq, depth, dry, wet, in);
// 	}
// }

// AMPitchShifter : UGen { 
// 	*ar {
// 		arg in, shift=0.75, bufsize=5;
// 		/*
// 			> k: Pitch shift (0.25 to 4)
// 			> k: Buffer size (1 to 7)
// 		*/
// 		^LADSPA.ar(1, 1433, shift, bufsize, in);
// 	}
// }

/*
	# 2156 TAP Fractal Doubler
	> k: Time Tracking (0 to 1)
	> k: Pitch Tracking (0 to 1)
	> k: Dry Level [dB] (-90 to 20)
	> k: Dry Left Position (0 to 1)
	> k: Dry Right Position (0 to 1)
	> k: Wet Level [dB] (-90 to 20)
	> k: Wet Left Position (0 to 1)
	> k: Wet Right Position (0 to 1)
	> a: Input_L
	> a: Input_R
*/

/*
	# 1220 Harmonic generator
	> k: Fundamental magnitude (-1 to 1)
	> k: 2nd harmonic magnitude (-1 to 1)
	> k: 3rd harmonic magnitude (-1 to 1)
	> k: 4th harmonic magnitude (-1 to 1)
	> k: 5th harmonic magnitude (-1 to 1)
	> k: 6th harmonic magnitude (-1 to 1)
	> k: 7th harmonic magnitude (-1 to 1)
	> k: 8th harmonic magnitude (-1 to 1)
	> k: 9th harmonic magnitude (-1 to 1)
	> k: 10th harmonic magnitude (-1 to 1)
*/

/*
	# 1423 Plate reverb
	> k: Reverb time (0.01 to 8.5)
	> k: Damping (0 to 1)
	> k: Dry/wet mix (0 to 1)
*/

/*
	# 1215 GSM simulator
	> k: Dry/wet mix (0 to 1)
	> k: Number of passes (0 to 10)
	> k: Error rate (bits/block) (0 to 30)
*/

/*
	# 2142 TAP Reverberator
	> k: Decay [ms] (0 to 10000)
	> k: Dry Level [dB] (-70 to 10)
	> k: Wet Level [dB] (-70 to 10)
	> k: Comb Filters
	> k: Allpass Filters
	> k: Bandpass Filter
	> k: Enhanced Stereo
	> k: Reverb Type (0 to 42.1)
	> a: Input Left
	< a: Output Left
	> a: Input Right
	< a: Output Right
*/

/*
	# 1208 Retro Flanger
	> k: Average stall (ms) (0 to 10)
	> k: Flange frequency (Hz) (0.5 to 8)
*/

/*
	# 2979 Rubber Band Mono Pitch Shifter
	< k: latency
	> k: Cents (-100 to 100)
	> k: Semitones (-12 to 12)
	> k: Octaves (-3 to 3)
	> k: Crispness (0 to 3)
	> k: Formant Preserving (0 to 1)
	> k: Faster (0 to 1)
	> a: Input
	< a: Output

	# 9792 Rubber Band Stereo Pitch Shifter
	< k: latency
	> k: Cents (-100 to 100)
	> k: Semitones (-12 to 12)
	> k: Octaves (-3 to 3)
	> k: Crispness (0 to 3)
	> k: Formant Preserving (0 to 1)
	> k: Faster (0 to 1)
	> a: Input L
	< a: Output L
	> a: Input R
	< a: Output R
*/

/*
	# 1227 Lo Fi
	> a: In (Left)
	> a: In (Right)
	< a: Out (Left)
	< a: Out (Right)
	> k: Crackling (%) (-0.1 to 100.1)
	> k: Powersupply Overloading (%) (0 to 100)
	> k: Opamp Bandwidth Limiting (Hz) (1 to 10000)
*/

/*
	# 1437 Giant flange
	> k: Double delay
	> k: LFO frequency 1 (Hz) (0 to 30)
	> k: Delay 1 range (s) (0 to 10.5)
	> k: LFO frequency 2 (Hz) (0 to 30)
	> k: Delay 2 range (s) (0 to 10.5)
	> k: Feedback (-100 to 100)
	> k: Dry/Wet level (0 to 1)
	> a: Input
	< a: Output
*/

/*
	# 1431 Bode frequency shifter
	> k: Frequency shift (0 to 5000)
	> a: Input
	< a: Down out
	< a: Up out
*/

