SWModule { // abstract class
	var params;
	sdString {
		^"";
	}
	view {
		^StaticText().object_(this);
	}
}

SWEnv : SWModule {
	classvar allowedTypes;
	var <type, <>attack=0.01, <>decay=0.5, <>sustainLevel=0.5, <>sustainTime=1, <>release=1, <>level=1, <>curves, <>envGen=true, <>doneAction=0, <>gate;
	*initClass {
		allowedTypes = [\adsr, \adr, \asr, \perc];
	}
	*new {
		arg type=\adsr;
		^super.new.init(type);
	}
	init {
		| type |
		this.type_(type);
		curves = [0, 0, 0];
		gate = "gate";
	}
	type_ {
		| envType |
		if(allowedTypes.includes(envType), {
			type = envType;
		}, {
			"SWEnv does not yet support this kind of envelope spec. Note that you must provide a symbol.".error;
		});
	}
	envString {
		^(
			"Env." ++
			type.asString ++
			"(" ++
			switch(type,
				\adsr, {
					[attack, decay, sustainLevel, release, level, curves].collect(_.asString).join(", ");
				},
				\adr, {
					[attack, decay, sustainLevel, release, level, curves].collect(_.asString).join(", ");
				},
				\asr, {
					[attack, sustainLevel, release, curves].collect(_.asString).join(", ");
				},
				\perc, {
					[attack, release, level, curves].collect(_.asString).join(", ");
				}) ++
			")";
		);
	}
	sdString {
		^(
			this.envString ++ if(envGen, {
				".kr(" ++ doneAction.asString ++ ", " ++ gate ++ ")";
			}, "");
		);
	}
	view {
		| parent |
		var change, cChange, view, pum, envv, nbx, nba, nbd, nbs, nbr, cbx, cb1, cb2, cb3;
		change = {
			| thing val |
			Message(this, (thing++\_).asSymbol, [val]).value;
			envv.setEnv(this.envString.interpret);
		};
		cChange = {
			| index val |
			this.curves[index] = val;
			envv.setEnv(this.envString.interpret);
		};
		view = UserView(parent).background_(Color.red).layout_(VLayout()).canFocus_(false);
		pum = PopUpMenu(view)
		.toolTip_("Env type")
		.items_(allowedTypes.collect(_.asString))
		.value_(allowedTypes.indexOfEqual(type))
		.action_({|w|change.(\type, allowedTypes[w.value])});
		view.layout.add(pum, 1);
		envv = EnvelopeView(view)
		.resize_(5)
		.background_(Color.red)
		.setEnv(this.envString.interpret)
		.setEditable(0, false)
		.keepHorizontalOrder_(true)
		.action_({
			| env |
			var levels = env.value[1];
			var times = env.value[0].differentiate.drop(1)*this.envString.interpret.duration;
			// Env( levels, times.differentiate.drop(-1), this.curves )
			nba.valueAction_(times[0]);
			nbd.valueAction_(times[1]);
			nbs.valueAction_(levels[2]);
			nbr.valueAction_(times[2]);
			// e.curves.cs.postln;
		});
		view.layout.add(envv);
		nbx = View(view).layout_(HLayout()).canFocus_(false);
		nba = NumberBox(nbx)
		.toolTip_("Attack time")
		.clipLo_(0)
		.clipHi_(30)
		.value_(attack)
		.action_({|w|change.(\attack, w.value)});
		nbx.layout.add(nba);
		nbd = NumberBox(nbx)
		.toolTip_("Decay time")
		.clipLo_(0)
		.clipHi_(30)
		.value_(decay)
		.action_({|w|change.(\decay, w.value)});
		nbx.layout.add(nbd);
		nbs = NumberBox(nbx)
		.toolTip_("Sustain level")
		.clipLo_(-1)
		.clipHi_(1)
		.value_(sustainLevel)
		.action_({|w|change.(\sustainLevel, w.value)});
		nbx.layout.add(nbs);
		nbr = NumberBox(nbx)
		.toolTip_("Release time")
		.clipLo_(0)
		.clipHi_(30)
		.value_(release)
		.action_({|w|change.(\release, w.value)});
		nbx.layout.add(nbr);
		view.layout.add(nbx);
		cbx = View(view).layout_(HLayout()).canFocus_(false);
		cb1 = NumberBox(cbx)
		.toolTip_("Curve 1")
		.clipLo_(-8)
		.clipHi_(8)
		.value_(curves[0])
		.action_({|w|cChange.(0, w.value)});
		cbx.layout.add(cb1);
		cb2 = NumberBox(cbx)
		.toolTip_("Curve 2")
		.clipLo_(-8)
		.clipHi_(8)
		.value_(curves[1])
		.action_({|w|cChange.(1, w.value)});
		cbx.layout.add(cb2);
		cb3 = NumberBox(cbx)
		.toolTip_("Curve 3")
		.clipLo_(-8)
		.clipHi_(8)
		.value_(curves[2])
		.action_({|w|cChange.(2, w.value)});
		cbx.layout.add(cb3);
		view.layout.add(cbx);
		^view;
	}
}

SWOsc : SWModule {
	classvar allowedTypes;
	var <type, <freq, <>freqInput, <>oct=0, <>semi=0, <>cent=0, <>width=0.5;
	*initClass {
		allowedTypes = [SinOsc, VarSaw, LFTri, Pulse];
	}
	*new {
		arg type=SinOsc;
		^super.new.init(type);
	}
	init {
		| type |
		this.type_(type);
	}
	type_ {
		| osc |
		if(allowedTypes.includes(osc), {
			type = osc;
		}, {
			"SWOsc does not yet support this kind of oscillator. Note that you must provide the class name.".error;
		});
	}
	typeString {
		^type.asString;
	}
	freqString {
		if(freq.isNil, {
			^(
				"freq" ++
				if(((oct.notNil and: {oct != 0}) or:
					(semi.notNil and: {semi != 0}) or:
					(cent.notNil and: {cent != 0})), {
						".transpose(" ++
						semi.asString ++
						if((cent.notNil and: {cent != 0}), {
							"+(" ++ cent.asString ++ "/100)";
						}, "") ++
						if((oct.notNil and: {oct != 0}), {
							", " ++ oct.asString;
						}, "") ++
						")";
					}, "") ++
				if(freqInput.notNil, {
					"+" ++ freqInput.asString;
				}, "");
			);
		}, {
			^freq;
		});
	}
	widthString {
		^(
			width.asString; // FIX: may need to check what kind of oscillator this is and scale the range.
		);
	}
	sdString {
		^(
			this.typeString ++ ".ar(" ++ this.freqString ++
			if([VarSaw, Pulse].includes(type), {
				if(width.notNil, {
					", " ++ switch(type,
						VarSaw, {
							"0, " ++ this.widthString;
						},
						Pulse, {
							this.widthString;
						});
				}, "");
			}, "") ++
			")"
		);
	}
	view {
		| parent |
		var change, view, freqs, pum, slider, nb1, nb2, nb3, vslider;
		change = {
			| thing val |
			Message(this, (thing++\_).asSymbol, [val]).value;
		};
		view = UserView(parent).background_(Color.green).layout_(VLayout()).canFocus_(false);
		freqs = View(view).layout_(HLayout());
		pum = PopUpMenu(freqs)
		.toolTip_("Osc type")
		.items_(allowedTypes.collect(_.asString))
		.value_(allowedTypes.indexOfEqual(type))
		.action_({|w|change.(\type, allowedTypes[w.value])});
		freqs.layout.add(pum);
		nb1 = NumberBox(freqs)
		.toolTip_("Octave")
		.clipLo_(-3)
		.clipHi_(3)
		.value_(oct)
		.action_({|w|change.(\oct, w.value)});
		freqs.layout.add(nb1);
		nb2 = NumberBox(freqs)
		.toolTip_("Semitones")
		.clipLo_(-12)
		.clipHi_(12)
		.value_(semi)
		.action_({|w|change.(\semi, w.value)});
		freqs.layout.add(nb2);
		nb3 = NumberBox(freqs)
		.toolTip_("Cents")
		.clipLo_(-100)
		.clipHi_(100)
		.value_(cent)
		.action_({|w|change.(\cent, w.value)});
		freqs.layout.add(nb3);
		slider = Slider(view, Rect(0,0,4,2))
		.toolTip_("Width")
		.value_(width)
		.action_({|w|change.(\width, w.value)});
		view.layout.add(slider);
		^view;
	}
}

SWMod : SWModule { // modulation controls (will need to be "linked" to the oscillators it controls)
	sdString {
		^"0";
	}
}

SWMix : SWModule { // can be used for prefilt, etc.
	classvar allowedTypes;
	var <type, <inputs, <levels;
	*initClass {
		allowedTypes = [\manual, \normalized];
	}
	*new {
		arg type=\normalized, num=3;
		^super.new.init(type, num);
	}
	init {
		| type num |
		var names;
		this.type_(type);
		names = num.collect {
			| n |
			"osc" ++ (n+1);
		};
		this.inputs_(names);
		this.levels_(1);
	}
	type_ {
		| thetype |
		if(allowedTypes.includes(thetype), {
			type = thetype;
		}, {
			"SWMix has no mix mode with this name. Note that you must provide a symbol.".error;
		});
	}
	inputs_ {
		| inpts |
		switch(inpts.class,
			Array, {
				inputs = inpts;
			},
			String, {
				inputs = [inpts];
			});
	}
	levels_ {
		| lvls |
		case(
			{ lvls.class == Array }, {
				levels = lvls;
			},
			{ lvls.isNumber }, {
				levels = lvls.dup(inputs.size);
			}
		);
	}
	sdString {
		^(
			"((" ++
			inputs.collect({
				| inpt n |
				"(" ++ inpt ++ "*" ++ levels.wrapAt(n).asString ++ ")";
			}).join("+") ++
			")" ++
			switch(type,
				\manual, {
					"/" ++ inputs.size.asString;
				},
				\normalized, {
					"/" ++ inputs.collect({|i,n|levels.wrapAt(n)}).sum.asString;
				}
			) ++
			")";
		);
	}
	view {
		| parent |
		var change, view, pum, sliders, sarray = [];
		change = {
			| thing val |
			levels[thing] = val;
		};
		view = UserView(parent).background_(Color.blue).layout_(VLayout()).canFocus_(false);
		pum = PopUpMenu(view)
		.toolTip_("Mode")
		.items_(allowedTypes.collect(_.asString))
		.value_(allowedTypes.indexOfEqual(type))
		.action_({|w|this.type = allowedTypes[w.value]});
		view.layout.add(pum);
		sliders = View(view).layout_(HLayout());
		inputs.do {
			| inpt n |
			var ccc = Slider(view, Rect(0,0,2,4))
			.toolTip_(inpt + "volume")
			.value_(levels[n])
			.action_({|w|change.(n, w.value)});
			sarray = sarray.add(ccc);
			sliders.layout.add(ccc);
		};
		^view;
	}
}

SWFilt : SWModule {
	classvar allowedTypes;
	var <type, <>input, <freq, <>freqInput, <>oct=0, <>semi=0, <>cent=0, <>rq=0;
	*initClass {
		allowedTypes = [\none, LPF, RLPF, MoogFF, RLPFD, BPF, BRF, HPF, RHPF];
	}
	*new {
		arg type=LPF;
		^super.new.init(type);
	}
	init {
		| type |
		this.type_(type);
		input = "prefilt";
	}
	type_ {
		| filtType |
		if(allowedTypes.includes(filtType), {
			type = filtType;
		}, {
			"SWFilt does not yet support this kind of filter. Note that you must provide the class name.".error;
		});
	}
	freqString {
		if(freq.isNil, {
			^(
				"freq" ++
				if(((oct.notNil and: {oct != 0}) or:
					(semi.notNil and: {semi != 0}) or:
					(cent.notNil and: {cent != 0})), {
						".transpose(" ++
						semi.asString ++
						if((cent.notNil and: {cent != 0}), {
							"+(" ++ cent.asString ++ "/100)";
						}, "") ++
						if((oct.notNil and: {oct != 0}), {
							", " ++ oct.asString;
						}, "") ++
						")";
					}, "") ++
				if(freqInput.notNil, {
					"+" ++ freqInput.asString;
				}, "");
			);
		}, {
			^freq;
		});
	}
	paramsString {
		case(
			{ [\none].includes(type) }, {
				^"";
			},
			{ [LPF, HPF].includes(type) }, { // only 'freq'.
				^"";
			},
			{ [RLPF, BPF, BRF, RHPF].includes(type) }, { // 'rq'
				^rq.asString;
			},
			{ [MoogFF].includes(type) }, { // "gain" (use 'rq' * 4)
				^(rq*4).asString;
			},
			{ [RLPFD].includes(type) }, { // 'rq', and 'dist'
				^rq.asString;
			},
		);
	}
	sdString {
		^(
			if(type == \none, {
				"MulAdd.new(" ++ input ++ ")";
			}, {
				type.asString ++
				".ar(" ++
				input ++ ", " ++
				this.freqString ++ ", " ++ 
				this.paramsString ++
				")";
			});
		);
	}
	view {
		| parent |
		var change, view, freqs, pum, nb1, nb2, nb3, slider, finpt;
		change = {
			| thing val |
			Message(this, (thing++\_).asSymbol, [val]).value;
		};
		view = UserView(parent).background_(Color.yellow).layout_(VLayout()).canFocus_(false);
		freqs = View(view).layout_(HLayout());
		pum = PopUpMenu(freqs)
		.toolTip_("Filter type")
		.items_(allowedTypes.collect(_.asString))
		.value_(allowedTypes.indexOfEqual(type))
		.action_({|w|this.type = allowedTypes[w.value]});
		freqs.layout.add(pum);
		nb1 = NumberBox(freqs)
		.toolTip_("Octave")
		.clipLo_(-3)
		.clipHi_(3)
		.value_(oct)
		.action_({|w|change.(\oct, w.value)});
		freqs.layout.add(nb1);
		nb2 = NumberBox(freqs)
		.toolTip_("Semitones")
		.clipLo_(-12)
		.clipHi_(12)
		.value_(semi)
		.action_({|w|change.(\semi, w.value)});
		freqs.layout.add(nb2);
		nb3 = NumberBox(freqs)
		.toolTip_("Cents")
		.clipLo_(-100)
		.clipHi_(100)
		.value_(cent)
		.action_({|w|change.(\cent, w.value)});
		freqs.layout.add(nb3);
		slider = Slider(view, Rect(0,0,4,2))
		.toolTip_("RQ")
		.value_(rq)
		.action_({|w|change.(\rq, w.value)});
		view.layout.add(slider);
		finpt = TextField(view)
		.toolTip_("Freq input")
		.value_(freqInput)
		.action_({|w|change.(\freqInput, w.value)});
		view.layout.add(finpt);
		^view;
	}
}

SWEffect : SWModule {
	sdString {
		^"Silence.ar";
	}
}

SWSynth : SWModule { // abstract class
	var <>name, <pattern;
	*initClass {
		Pdef(\_swTest, Pbind(
			\dur, 1,
			\midinote, Pseries(53, 2, 12) ++ Pseries(54, 2, 12),
		)).condition_(_.isNil);
	}
	init {
		pattern = \_swTest;
	}
	pattern_ {
		| pat |
		pat = pat.asSymbol;
		if(Pdef.all.keys.includes(pat), {
			pattern = pat;
		}, {
			"SWSynth: No pattern with that name.".warn;
		});
	}
	test {
		| play=true |
		if(play, { // start playing
			Pdef(\_swTestHolder, Pchain(
				Pbind(\instrument, this.name.asSymbol),
				Psym(this.pattern),
			)).play;
			Tdef(\_swTestTask, {
				loop {
					this.add;
					2.wait;
				}
			}).play;
		}, {
			Pdef(\_swTestHolder).stop;
			Tdef(\_swTestTask).stop;
		});
	}
	add {
		(this.sdString ++ ".add;").interpret;
	}
	play {
		(this.sdString ++ ".play;").interpret;
	}
	view {
		| parent |
		var preview, sbar;
		preview = {
			("{var thesynth = " ++ this.sdString ++ ".play;5.wait;thesynth.release;}.fork;").interpret;
		};
		sbar = View(parent).background_(Color.gray(0.5)).layout_(HLayout());
		sbar.layout.add(
			TextField(parent)
			.string_(name) // FIX
			.action_({
				| tf |
				name = tf.value;
			})
		);
		sbar.layout.add(
			Button(parent)
			.states_([["Pattern:"], ["Stop:", Color.black, Color.red]])
			.toolTip_("Start/stop test pattern")
			.action_({
				| btn |
				this.test(btn.value.asBoolean);
			})
		);
		sbar.layout.add(
			~ctf = CompletingTextField(sbar)
			.string_(this.pattern.asString)
			.toolTip_("Test pattern name")
			.completions_(Pdef.all.keys.asArray)
			.action_({
				| ctf |
				if(Pdef.all.keys.includes(ctf.string.asSymbol), {
					this.pattern_(ctf.string);
				}, {
					ctf.string_(this.pattern.asString);
				});
				ctf.completions_(Pdef.all.keys.asArray);
			})
		);
		sbar.layout.add(
			Button(parent)
			.states_([["Preview"]])
			.action_({
				preview.();
			})
		);
		sbar.layout.add(
			Button(parent)
			.states_([["Post"]])
			.action_({
				this.sdString.postln;
			})
		);
		sbar.layout.add(
			Button(parent)
			.states_([["Add"]])
			.action_({
				(this.sdString ++ ".add;").interpret;
			})
		);
		^sbar;
	}
	// gui {
	// 	var win, view;
	// 	win = Window(this.class.asString);
	// 	view = this.view(win).resize_(5).bounds_(win.view.bounds);
	// 	win.front;
	// }
}

DrumSynth : SWSynth {
	var <>o1freq=440, <>o1sweep=0, <>o1band=0.1, <>o1harm=0, <>o1dec=0.5, <>o1noiseTone=\tone, <>o1inv=false, <>o1amp=0.5, <>ring=0, <>o2freq=440, <>o2band=0.2, <>o2atk=0, <>o2dec=0.2, <>o2curve=0, <>o2amp=0.5, <>rel=0.5, <>tCount=1, <>tRate=5, <>tempoSync=false;
	/*
		FIX:
		implement o1harm,
		use "freq" as synth arg,
		implement tempoSync,
		change osc2 to pitched noise,
		remove the main env,
		remove DetectSilence?
	*/
	*new {
		arg name="drumSynth1";
		^super.newCopyArgs(name).init(name);
	}
	init {
		| nme |
		super.init;
		name = nme;
	}
	argsString {
		^"| freq=440 tempo=1 amp=0.5 pan=0 out=0 |";
	}
	impString {
		^"Impulse.kr(" ++ this.tRate.asString ++ ");";
	}
	pcString {
		^"PulseCount.kr(imp);"
	}
	retrigString {
		^"imp * (pc<=num);";
	}
	envString {
		^"Env.perc(0.001, " ++ this.rel.asString ++ ").kr(" ++
		if(tCount > 1, "0", "2") ++ ", retrig);"
	}
	o1string {
		^(
			"(" ++
			switch(o1noiseTone,
				\noise, {
					"Normalizer.ar(BPF.ar(WhiteNoise.ar, " ++
					this.o1freq.asString ++ ", " ++
					this.o1band.asString ++ "))";
				},
				\tone, {
					"SinOsc.ar(" ++ this.o1freq.asString ++ "+Sweep.kr(retrig, " ++ this.o1sweep.asString ++ "))";
				}
			) ++ if(this.o1inv, " * (-1)", "") ++
			if(this.ring > 0, " * (osc2*" ++ this.ring.asString ++ ")", "") ++
			" * Env.perc(0, " ++ this.o1dec.asString ++ ").kr(0, retrig)" ++
			").clip2;";
		);
	}
	o2string {
		^(
			"Normalizer.ar(BPF.ar(WhiteNoise.ar, " ++
			this.o2freq.asString ++ ", " ++
			this.o2band.asString ++ ")) * Env.perc(" ++
			this.o2atk.asString ++ ", " ++ this.o2dec.asString ++ ", 1, " ++ this.o2curve ++ ").kr(0, retrig);";
		);
	}
	outputString {
		^(
			"Pan2.ar(((osc2*" ++ this.o2amp.asString ++ ") + (osc1*" ++ this.o1amp.asString ++ ")).clip2, pan, env * amp);" ++
			if(tCount > 1, "\n\tDetectSilence.ar(output, time: 2, doneAction: 2);", "");
		);
	}
	sdString {
		^(
			"SynthDef(\\" ++ this.name ++ ", {" ++
			"\n\t" ++ this.argsString ++
			"\n\tvar num, imp, pc, retrig, env, osc2, osc1, output;" ++
			"\n\tnum = " ++ this.tCount.asString ++ ";"
			"\n\timp = " ++ this.impString ++
			"\n\tpc = " ++ this.pcString ++
			"\n\tretrig = " ++ this.retrigString ++
			"\n\tenv = " ++ this.envString ++
			"\n\tosc2 = " ++ this.o2string ++
			"\n\tosc1 = " ++ this.o1string ++
			"\n\toutput = " ++ this.outputString ++
			"\n\tOut.ar(out, output);\n})";
		);
	}
	view {
		| parent |
		var change, view, sbar, main, osc1, osc2;
		change = {
			| thing val |
			Message(this, (thing++\_).asSymbol, [val]).value;
		};
		view = UserView(parent).background_(Color.gray).layout_(VLayout()).canFocus_(false);
		sbar = super.view(view);
		view.layout.add(sbar);
		main = View(view).layout_(HLayout()).canFocus_(false);
		[
			NumberBox(main)
			.action_({
				| nbx |
				change.(\tCount, nbx.value);
			})
			.decimals_(0).clipLo_(1).clipHi_(30)
			.value_(this.tCount)
			.toolTip_("Retrigger count"),
			Slider(main, Rect(0,0,4,2))
			.action_({
				| slider |
				change.(\tRate, [0.1,30].asSpec.map(slider.value));
			})
			.toolTip_("Retrigger rate")
			.value_([0.1,30].asSpec.unmap(this.tRate)),
			Knob(main)
			.action_({
				| knob |
				change.(\ring, [0,1].asSpec.map(knob.value));
			})
			.mode_(\vert).value_(this.ring).toolTip_("Ring modulation"),
			Slider(main, Rect(0,0,4,2))
			.action_({
				| slider |
				change.(\rel, [0.01,7].asSpec.map(slider.value));
			})
			.toolTip_("Release time").value_([0.01,7].asSpec.unmap(this.rel)),
		].do({
			| obj |
			main.layout.add(obj);
		});
		view.layout.add(main);
		osc1 = View(view).layout_(HLayout()).canFocus_(false);
		[
			Slider(osc1, Rect(0,0,4,2))
			.action_({
				| slider |
				change.(\o1freq, \freq.asSpec.map(slider.value));
			})
			.toolTip_("Osc 1 frequency")
			.value_(\freq.asSpec.unmap(this.o1freq)),
			Slider(osc1, Rect(0,0,2,4))
			.action_({
				| slider |
				change.(\o1sweep, [-5000,5000].asSpec.map(slider.value));
			})
			.toolTip_("Osc 1 sweep")
			.value_([-5000,5000].asSpec.unmap(this.o1sweep)),
			Slider(osc1, Rect(0,0,2,4))
			.action_({
				| slider |
				change.(\o1band, [0.0001,1,\exp].asSpec.map(slider.value));
			})
			.toolTip_("Osc 1 band")
			.value_([0.0001,1,\exp].asSpec.unmap(this.o1band)),
			Knob(osc1)
			.action_({
				| knob |
				change.(\o1dec, [0,7].asSpec.map(knob.value));
			})
			.toolTip_("Osc 1 decay").mode_(\vert)
			.value_([0,7].asSpec.unmap(this.o1dec)),
			Button(osc1)
			.action_({
				| btn |
				change.(\o1noiseTone, [\noise, \tone][btn.value]);
			})
			.states_([["Noise"],["Tone"]])
			.value_([\noise,\tone].indexOf(this.o1noiseTone)),
			CheckBox(osc1)
			.action_({
				| cbx |
				change.(\o1inv, cbx.value);
			})
			.toolTip_("Osc 1 phase invert")
			.value_(this.o1inv),
			Knob(osc1)
			.action_({
				| knob |
				change.(\o1amp, [0,5].asSpec.map(knob.value));
			})
			.toolTip_("Osc 1 amp").mode_(\vert)
			.value_([0,5].asSpec.unmap(this.o1amp))
		].do({
			| obj |
			osc1.layout.add(obj);
		});
		view.layout.add(osc1);
		osc2 = View(view).layout_(HLayout()).canFocus_(false);
		[
			Slider(osc2, Rect(0,0,4,2))
			.action_({
				| slider |
				change.(\o2freq, \freq.asSpec.map(slider.value));
			})
			.toolTip_("Osc 2 frequency")
			.value_(\freq.asSpec.unmap(this.o2freq)),
			Slider(osc2, Rect(0,0,2,4))
			.action_({
				| slider |
				change.(\o2band, [0.0001,1,\exp].asSpec.map(slider.value));
			})
			.toolTip_("Osc 2 band")
			.value_([0.0001,1,\exp].asSpec.unmap(this.o2band)),
			Knob(osc2)
			.action_({
				| knob |
				change.(\o2atk, [0,7].asSpec.map(knob.value));
			})
			.toolTip_("Osc 2 attack").mode_(\vert)
			.value_([0,7].asSpec.unmap(this.o2atk)),
			Knob(osc2)
			.action_({
				| knob |
				change.(\o2dec, [0,7].asSpec.map(knob.value));
			})
			.toolTip_("Osc 2 decay").mode_(\vert)
			.value_([0,7].asSpec.unmap(this.o2dec)),
			NumberBox(osc2)
			.action_({
				| nbx |
				change.(\o2curve, nbx.value);
			})
			.toolTip_("Osc 2 env curve").clipHi_(8).clipLo_(-8)
			.value_(this.o2curve),
			Knob(osc2)
			.action_({
				| knob |
				change.(\o2amp, [0,5].asSpec.map(knob.value));
			})
			.toolTip_("Osc 2 amp").mode_(\vert)
			.value_([0,5].asSpec.unmap(this.o2amp))
		].do({
			| obj |
			osc2.layout.add(obj);
		});
		view.layout.add(osc2);
		^view;
	}
}

SWMicron : SWSynth {
	classvar <modules, <varnames;
	var <mods;
	*initClass {
		modules = [SWEnv, SWEnv, SWEnv, SWOsc, SWOsc, SWOsc, SWMod, SWMix, SWFilt, SWFilt, SWEffect, SWEffect];
		varnames = ["env1", "env2", "env3", "osc3", "osc2", "osc1", "depth", "prefilt", "filt1", "filt2", "fx1", "fx2"];
	}
	*new {
		arg name="synth";
		^super.new.init(name);
	}
	init {
		| nme |
		super.init;
		mods = modules.collect(_.new);
		this[SWMix] = SWMix(\normalized, 3);
		this[0].doneAction_(2);
		name = nme;
		this["prefilt"].inputs_(["osc3", "osc2", "osc1"]);
		this["filt1"].freqInput_("env2*50");
		this["filt2"].freqInput_("env2*50");
		this["filt2"].type_(\none);
		this["filt2"].input_("filt1");
	}
	at {
		| index |
		switch(index.class,
			Integer, {
				^mods[index];
			},
			String, {
				^mods[varnames.indexOfEqual(index)];
			},
			Class, {
				^mods[modules.indexOfEqual(index)];
			}
		);
	}
	put {
		| index module |
		switch(index.class,
			Integer, {
				if(modules[index] == module.class, {
					mods[index] = module;
				}, {
					"Can't change the type of module.".error;
				});
			},
			String, {
				var ioe = varnames.indexOfEqual(index);
				if(modules[ioe] == module.class, {
					mods[ioe] = module;
				}, {
					"Can't change the type of module.".error;
				});
			},
			Class, {
				var ioe = modules.indexOfEqual(index);
				if(modules[ioe] == module.class, {
					mods[ioe] = module;
				}, {
					"Can't change the type of module.".error;
				});
			},
		);
	}
	varString {
		| num |
		^(
			varnames[num];
		);
	}
	varsString {
		^(
			"var " ++ varnames.join(", ") ++ ", output;";
		);
	}
	argsString {
		^(
			"| gate=1 freq=440 amp=0.5 pan=0 out=0 |"
		);
	}
	outputString {
		^(
			"output = filt2;";
		);
	}
	finalOutputString {
		^(
			"Out.ar(out, Pan2.ar(output, pan, env1*amp));";
		);
	}
	sdString {
		^(
			"SynthDef(\\" ++ this.name ++ ", {\n\t" ++
			this.argsString ++ "\n\t" ++
			this.varsString ++ "\n\t" ++
			this.mods.collect({
				| mod n |
				this.varString(n) ++ " = " ++ mod.sdString ++ ";\n\t";
			}).join ++
			this.outputString ++ "\n\t" ++
			this.finalOutputString ++ "\n})"
		);
	}
	view {
		| parent |
		var view, preview, sbar, envs, oscs, mix, filters;
		view = UserView(parent).background_(Color.black).layout_(VLayout()).canFocus_(false);
		sbar = super.view(view);
		view.layout.add(sbar);
		envs = View(view).background_(Color.red(0.5)).layout_(HLayout());
		3.do {
			| n |
			envs.layout.add(this[n].view);
		};
		oscs = View(view).background_(Color.green(0.5)).layout_(HLayout());
		3.do {
			| n |
			oscs.layout.add(this[n+3].view);
		};
		mix = View(view).background_(Color.blue(0.5)).layout_(HLayout());
		2.do {
			| n |
			mix.layout.add(this[n+6].view);
		};
		filters = View(view).background_(Color.yellow(0.5)).layout_(HLayout());
		2.do {
			| n |
			filters.layout.add(this[n+8].view);
		};
		^view;
	}
}

