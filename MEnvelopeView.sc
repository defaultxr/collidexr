MEnvelopeView : QEnvelopeView { // FIX: should be able to accept envelopes with levels out of the 0..1 range
	var curves;
	curves_ {
		| curve |
		curves = curve;
		^super.curves_(curves);
	}
	curves {
		^curves;
	}
	levels {
		^this.value[1];
	}
	times {
		^this.value[0].differentiate.drop(1);
	}
	env {
		^Env(this.levels, this.times, this.curves);
	}
	bipolarEnv {
		^Env((this.levels*2)-1, this.times, this.curves);
	}
}
