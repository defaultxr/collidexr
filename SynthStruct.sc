KSlider : SCViewHolder { // see also: CKnob in Ctl.sc
	var <parent, <bounds, <layout, <statictext, <slider, <numberbox, <spec, <>action, <label;
	*new {
		| parent bounds layout=\horizontal |
		^super.newCopyArgs(nil, parent, bounds??{parent.bounds}, layout).init; // no idea why 'nil' is necessary for the first arg here.
	}
	init {
		this.view_(UserView(parent, bounds).layout_(if(layout==\horizontal, {HLayout()}, {VLayout()})));
		statictext = StaticText(this.view, this.view.bounds).string_("KSlider");
		this.view.layout.add(statictext, 1);
		slider = Slider(this.view, Rect(0,0,4,2));
		this.view.layout.add(slider, 6);
		numberbox = NumberBox(this.view, this.view.bounds);
		this.view.layout.add(numberbox, 1);
		// configure the views...
		// slider.step_(1); // as a default, until the spec is set.
		slider.shift_scale_(0.01); // because Pure Data!
		// slider.alt_scale_(100);
		// numberbox.step_(1);
		// numberbox.scroll_step_(1);
		numberbox.shift_scale_(0.01); // because Pure Data!
		numberbox.alt_scale_(100);
		// set up the actions...
		slider.action_({
			| sl |
			var real = this.spec.map(sl.value);
			numberbox.value_(real);
			this.action.value(this);
		});
		numberbox.action_({
			| nb |
			slider.value_(this.spec.unmap(nb.value));
			this.action.value(this);
		});
		this.spec_(ControlSpec());
	}
	spec_ {
		| cspec |
		var nstep = cspec.step;
		spec = cspec;
		if(nstep==0, {
			nstep = cspec.guessNumberStep;
		});
		numberbox.step_(nstep);
		// slider.step_(nstep);
		numberbox.clipLo_(cspec.minval);
		numberbox.clipHi_(cspec.maxval);
	}
	label_ {
		| text |
		this.statictext.string_(text);
	}
	value_ {
		| num |
		this.numberbox.valueAction_(num);
	}
	value {
		^this.numberbox.value;
	}
	toolTip_ {
		| text |
		this.statictext.toolTip_(text);
		this.slider.toolTip_(text);
		this.numberbox.toolTip_(text);
	}
}

SynthStruct { // base (abstract) class...
	// classvar allowedTypes;
	var <>name, <controls, <guihints, <values, <pattern, <>autoReAdd=false, genString;
	*initClass {
		Spec.add(\time, [0.01, 30, \exponential, 0, 1, "sec"]);
		Pdef(\_ssTest, Pbind(
			\dur, 1,
			\midinote, Pseries(53, 2, 12) ++ Pseries(54, 2, 12),
		)).condition_(_.isNil);
	}
	*new {
		^super.new.init;
	}
	init {
		controls = [];
		this.name_(this.class.asString.toLower ++ "1");
	}
	params {
		^controls.collect(_[\name]);
	}
	param {
		| name |
		^controls.detect({
			| thing |
			thing[\name] == name;
		});
	}
	setDefaults {
		values = ();
		controls.do({
			| ctrl |
			this.setValue(ctrl[\name], ctrl[\default]?1);
		});
		if(this.name.isNil, {
			this.name_(this.class.asString.toLower ++ "1");
		});
		if(this.pattern.isNil, {
			this.pattern_(\_ssTest);
		});
	}
	getValue {
		| thing |
		thing = thing.asSymbol;
		^if(thing == \name, this.name, {
			if(this.param(thing)[\spec].class == Array, {
				this.param(thing)[\spec][values[thing]];
			}, {
				values[thing];
			});
		});
	}
	at {
		| thing |
		^this.getValue(thing);
	}
	setValue {
		| thing value |
		if(value.isNumber, {
			values[thing] = value;
		}, {
			"Cannot set a parameter to a non-numeral value yet...".error;
		});
		if(this.autoReAdd, {
			this.add;
		});
	}
	put {
		| thing value |
		this.setValue(thing, value);
	}
	gene {
		// FIX
	}
	genString {
		^genString.split2("%%").collect({
			| thing index |
			if(index.even, {
				thing
			}, {
				this.getValue(thing.asSymbol).asString;
			});
		}).reduce('++');
	}
	add {
		(this.genString++".add;").interpret;
	}
	play {
		^((this.genString++".play;").interpret);
	}
	pattern_ {
		| pat |
		pat = pat.asSymbol;
		if(Pdef.all.keys.includes(pat), {
			pattern = pat;
		}, {
			"SynthStruct: No pattern with that name.".warn;
		});
	}
	preview {
		fork {
			var thesynth = this.play;
			var nw = NodeWatcher.register(thesynth);
			5.wait;
			if(thesynth.isRunning, {
				thesynth.release;
			});
			nw.stop;
		}
	}
	test {
		| play=true |
		if(play, { // start playing
			this.add;
			Pdef(\_ssTestHolder, Pchain(
				Pbind(\instrument, this.name.asSymbol),
				Psym(this.pattern),
			)).play;
			this.autoReAdd = true;
		}, { // stop playing
			Pdef(\_ssTestHolder).stop;
		});
	}
	doesNotUnderstand {
		| selector ... args |
		if(selector.asString.wrapAt(-1) == $_, { // SET!
			var param = selector.asString.drop(-1).asSymbol;
			if(this.param(param).notNil, {
				^this.setValue(param, args[0]);
			});
		}, {
			if(this.param(selector).notNil, {
				^this.getValue(selector);
			});
		});
		// "Does not understand.".postln; // FIX
	}
	prGenerateControl {
		| parent bounds param |
		var penv = this.param(param.asSymbol);
		param = param.asSymbol;
		^if(penv[\spec].class == Array, {
			PopUpMenu(parent, bounds)
			.items_(penv[\spec])
			.value_(penv[\spec].indexOf(this.getValue(penv[\name])))
			.toolTip_(penv[\name].asString)
			.action_({
				| pum |
				this.setValue(param, pum.value);
			});
		}, {
			KSlider(parent, bounds, \horizontal)
			// .orientation_(\horizontal)
			.label_(penv[\name].asString)
			.spec_(penv[\spec].asSpec)
			.value_(this.getValue(penv[\name]))
			// .value_(penv[\spec].asSpec.unmap(this.getValue(penv[\name])))
			.toolTip_(penv[\name].asString ++ ": " ++ this.getValue(penv[\name]).asString)
			.action_({
				| slider |
				// var realvalue = penv[\spec].asSpec.map(slider.value);
				this.setValue(param, slider.value);
				slider.toolTip_(penv[\name].asString ++ ": " ++ slider.value.asString);
			});
		});
	}
	toolbarView {
		| parent |
		var sbar;
		sbar = View(parent).background_(Color.gray(0.5)).layout_(HLayout());
		sbar.layout.add(
			TextField(parent)
			.string_(this.name)
			.toolTip_("Name of this SynthDef")
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
			.onClose_({ // if the pattern is playing and the view closes, we probably want to stop the pattern.
				| btn |
				this.test(false);
			});
		);
		sbar.layout.add(CompletingTextField(sbar)
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
			ctf.completions_({ Pdef.all.keys.asArray });
		}).view);
		sbar.layout.add(
			Button(parent)
			.states_([["Preview"]])
			.toolTip_("Play the SynthDef with its current parameters")
			.action_({
				this.preview;
			})
		);
		sbar.layout.add(
			Button(parent)
			.states_([["Post"]])
			.toolTip_("Post the SynthDef code to the SuperCollider console")
			.action_({
				this.genString.postln;
			})
		);
		sbar.layout.add(
			Button(parent)
			.states_([["Add"]])
			.toolTip_("Add the SynthDef")
			.action_({
				this.add;
			})
		);
		^sbar;
	}
	controlsView { // subclasses should override this method to provide their own custom GUIs for editing the synth.
		| parent |
		var cview;
		cview = UserView(parent, parent.view.bounds).layout_(VLayout()).canFocus_(false).resize_(5);
		if(guihints.isNil, {
			controls.do({
				| ctl |
				cview.layout.add(this.prGenerateControl(cview, cview.bounds, ctl[\name]));
			});
		}, {
			var foreach = {
				| what view num=0 |
				what.do({
					| thing |
					if(thing.class == Symbol, {
						var ctl = this.prGenerateControl(view, view.bounds, thing);
						view.layout.add(if(ctl.class == KSlider, {ctl.view}, {ctl}));
					}, {
						// var cls = if(num.isEven, HLayout, VLayout);
						var uv = UserView(view, view.view.bounds).canFocus_(false).resize_(5);
						if(num.even, {
							uv.layout_(HLayout());
						}, {
							uv.layout_(VLayout());
						});
						foreach.(thing, uv, num+1);
					});
				});
			};
			foreach.(guihints, cview);
			// guihints.do({
			// 	| what |
			// 	foreach.(what, cview);
			// });
		});
		^cview; // subclasses should be sure to return a UserView containing the controls!
	}
	view {
		| parent |
		var view;
		view = UserView(parent, parent.view.bounds).layout_(VLayout()).canFocus_(false).resize_(5);
		view.keyDownAction_({
			| view char modifiers unicode keycode |
			if(modifiers.isCtrl && (keycode == 25), { // ctrl+w
				view.parent.close;
			});
		});
		view.layout.add(this.toolbarView(view), 1);
		view.layout.add(this.controlsView(view), 8);
		^view;
	}
}

DrumSS : SynthStruct {
	init {
		controls = [
			(name:\startFreq, spec:\freq, default:200),
			(name:\endFreq, spec:\freq, default:20),
			(name:\time, spec:\time, default:0.1),
		];
		genString = "SynthDef(\\%%name%%, {
\t| amp=0.5 pan=0 out=0 |
\tvar env, fenv, output;
\tenv = Env.perc(0.01, %%time%%).kr(2);
\tfenv = Env([%%startFreq%%, %%endFreq%%], [%%time%%]).kr;
\toutput = SinOsc.ar(fenv);
\tOut.ar(out, Pan2.ar(output, pan, env * amp));
})";
		this.setDefaults;
	}
	// view {
	// 	| parent |
	// 	var view, preview, sbar, envs, oscs, mix, filters;
	// 	view = UserView(parent, parent.view.bounds).background_(Color.black).layout_(VLayout()).canFocus_(false).resize_(5);
	// 	sbar = super.toolbarView(view);
	// 	view.layout.add(sbar);
	// 	view.layout.add(super.view(view));
	// }
}

MicronSS : SynthStruct {
	*initClass {
		Spec.add(\transpose, [-48, 48, \lin, 1, 0, "Semitones"]);
	}
	init {
		var stp = [\SinOsc, \FMVarSaw, \FMPulse];
		controls = [
			(name:\fmtype, spec:[\lin2_1, \lin23_1, \lin3_2_1, \exp2_1, \exp23_1, \exp3_2_1], default:3),
			(name:\fmdepth, spec:[0, 100].asSpec, default:0),
			(name:\osc1type, spec:stp, default:0),
			(name:\osc1amp, spec:\amp, default:1),
			(name:\osc1shape, spec:\unipolar, default:0.5),
			(name:\osc1trans, spec:\transpose, default:0),
			(name:\osc2type, spec:stp, default:0),
			(name:\osc2amp, spec:\amp, default:0),
			(name:\osc2shape, spec:\unipolar, default:0.5),
			(name:\osc2trans, spec:\transpose, default:0),
			(name:\osc3type, spec:stp, default:0),
			(name:\osc3amp, spec:\amp, default:0),
			(name:\osc3shape, spec:\unipolar, default:0.5),
			(name:\osc3trans, spec:\transpose, default:0),
		];
		guihints = [
			[\fmtype, \fmdepth],
			[
				[\osc1type, \osc1amp, \osc1shape, \osc1trans],
				[\osc2type, \osc2amp, \osc2shape, \osc2trans],
				[\osc3type, \osc3amp, \osc3shape, \osc3trans],
			]
		];
		this.setDefaults;
	}
	genString {
		var oscGen = {
			| num |
			var numstr = num.asString;
			var osctype = this.getValue(("osc" ++ numstr ++ "type"));
			var shp = this.getValue(("osc" ++ numstr ++ "shape"));
			var fmtype = this.getValue(\fmtype);
			var fmdepth = this.getValue(\fmdepth);
			osctype.asString ++ ".ar(o" ++ num.asString ++ "f" ++
			if([\exp2_1, \exp23_1, \exp3_2_1].includes(fmtype) and: {fmdepth>0}, {
				switch(num,
					3, "",
					2, {
						if(fmtype==\exp3_2_1, {
							"+(osc3*" ++ fmdepth.asString ++ "*o2f)";
						}, "");
					},
					1, {
						if([\exp3_2_1, \exp2_1].includes(fmtype), {
							"+(osc2*" ++ fmdepth.asString ++ "*o1f)";
						}, {
							"+(((osc2+osc3)/2)*" ++ fmdepth.asString ++ "*o1f)";
						});
					},
				);
			}, "") ++
			switch(osctype,
				\SinOsc, {
					if(shp != 0.5, {
						").pow(" ++ [0,2].asSpec.map(shp).asString;
					}, "");
				},
				\FMVarSaw, {
					", 0, " ++ shp.asString;
				},
				\FMPulse, {
					", " ++ shp.asString;
				}) ++ ")";
		};
		var name = this.name.asString;
		var osc1amp = this.getValue(\osc1amp).asString;
		var osc2amp = this.getValue(\osc2amp).asString;
		var osc3amp = this.getValue(\osc3amp).asString;
		^("SynthDef(\\" ++ name ++ ", {
\t| gate=1 freq=440 amp=0.5 pan=0 out=0 |
\tvar env1, o3f, o2f, o1f, osc3, osc2, osc1, prefilt, output;
\tenv1 = Env.adsr(0.1, 0.3, 0.5, 1).kr(2, gate);
\to3f = freq" ++ if(this.osc3trans != 0, { ".transpose(" ++ this.osc3trans.asString ++ ")" }, "") ++ ";
\to2f = freq" ++ if(this.osc2trans != 0, { ".transpose(" ++ this.osc2trans.asString ++ ")" }, "") ++ ";
\to1f = freq" ++ if(this.osc1trans != 0, { ".transpose(" ++ this.osc1trans.asString ++ ")" }, "") ++ ";
\tosc3 = " ++ oscGen.(3) ++ ";
\tosc2 = " ++ oscGen.(2) ++ ";
\tosc1 = " ++ oscGen.(1) ++ ";
\tprefilt = ((osc1*" ++ osc1amp ++ ")+(osc2*" ++ osc2amp ++ ")+(osc3*" ++ osc3amp ++ "));
\toutput = prefilt;
\tOut.ar(out, Pan2.ar(output, pan, amp * env1));
})");
	}
}