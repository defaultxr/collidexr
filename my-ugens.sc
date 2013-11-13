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

RateShift : PureUGen {
	*ar {
		arg in=0, rate=1, maxdelaytime=2;
		^DelayC.ar(in, maxdelaytime, Phasor.ar(1, 1-rate, 0, SampleRate.ir*maxdelaytime)/SampleRate.ir);
	}
	*kr {
		arg in=0, rate=1, maxdelaytime=2;
		^DelayC.kr(in, maxdelaytime, Phasor.kr(1, 1-rate, 0, SampleRate.ir*maxdelaytime)/SampleRate.ir);
	}
}

SndP : PureUGen {
	*ar {
		arg bufnum, rate=1, start=0, end=1, trigger=1, doneAction=0, numChannels=2;
		var bufframes, line_dur, startpos, endpos, line;
		bufframes = BufFrames.kr(bufnum);
		line_dur = (bufframes/BufSampleRate.kr(bufnum)*(abs(start-end)))/abs(rate);
		// startpos = (BinaryOpUGen('>', rate, 0)*start)+(BinaryOpUGen('<', rate, 0)*end);
		// endpos = (BinaryOpUGen('>', rate, 0)*end)+(BinaryOpUGen('<', rate, 0)*start);
		startpos = ((rate>0)*start)+((rate<0)*end);
		endpos = ((rate>0)*end)+((rate<0)*start);
		line = Env([startpos*bufframes, startpos*bufframes, endpos*bufframes, endpos*bufframes], [0,line_dur,0]).ar(doneAction, trigger);
		^BufRd.ar(numChannels, bufnum, line);
	}
}
