CKnob : SCViewHolder { // see also: KSlider in SynthStruct.sc
	// FIX: rename the instance vars (label -> labels, spec -> specs, action -> actions, etc)
	// FIX: make it Ctl-compatible
	var <parent, <bounds, label, spec, <>action, <vals, <knob, <viz, <numberbox;
	var <mouseDown=false;
	var <xy;
	*new { // FIX: should make it possible to use without a spec being required.
		| parent bounds label spec action |
		^super.newCopyArgs(parent, bounds??{if(parent.notNil, {parent.bounds}, nil)}).init(label, spec, action);
	}
	init {
		| ilabel ispec iaction |
		this.view_(UserView(parent, bounds).layout_(VLayout().spacing_(0).margins_(0)));
		xy = [0, 0];
		numberbox = NumberBox();
		spec = [];
		label = [];
		vals = [];
		this.spec_(ispec);
		this.label_(ilabel);
		this.action_(iaction);
		viz = UserView().background_(Color.black)
		.drawFunc_({
			| view |
			var string = nil.dup(spec.size);
			Pen.color_(Color.white);
			// Pen.stringAtPoint(this.label??{this.spec(0).asString}, 0@0);
			spec.do({
				| the_spec num |
				string[num] = this.label(num)??{this.spec(num).asString} ++ ": "
				++ this.value(num)
				++ if(the_spec.isNil, {
					" (no spec)";
				}, "");
			});
			string.do({
				| str n |
				Pen.stringAtPoint(str, Point(0, 20*n));
			});
			vals.do({
				| v n |
				var percent = spec[n].unmap(v);
				var x = percent * Message(view.bounds, [\width, \height].at(n)).value;
				if(n.even, {
					Pen.line(x@0, x@view.bounds.height);
				}, {
					var inv = (view.bounds.height-x);
					Pen.line(0@inv, view.bounds.width@inv);
				});
			});
			Pen.stroke;
			Pen.draw;
		})
		.mouseDownAction_({
			| view x y modifiers buttonNumber clickCount |
			switch(buttonNumber,
				0, {
					mouseDown = true;
					xy = [x, y];
					{ // this has to be a routine so that it is still updated when relative mode is in use.
						while({ mouseDown == true }, {
							this.change(xy[0]/viz.bounds.width);
							this.change(1-(xy[1]/viz.bounds.height), 1);
							0.01.wait;
						});
					}.fork(AppClock);
				},
				1, {
					"RANGE SET".postln;
					// set the range within the spec.. (or edit the number?)
				},
			);
		})
		.mouseMoveAction_({
			| view x y modifiers |
			xy = [x, y];
		})
		.mouseUpAction_({
			| view x y modifiers buttonNumber |
			if(buttonNumber == 0, { // released left mouse
				mouseDown = false;
			});
		});
		// knob = Knob().mode_(\vert)
		// .action_({
		// 	| kn |
		// 	this.set(spec.map(kn.value));
		// })
		// .shift_scale_(0.01);
		this.view.layout.add(viz);
		numberbox.action_({
			| nb |
			this.set(nb.value);
		})
		.shift_scale_(0.01)
		.alt_scale_(100);
		this.view.layout.add(numberbox);
	}
	change { // change the value, taking into account the mode, spec, etc.
		| by which=0 | // 'by' should be a number from 0-1
		if(spec[which].isNil, { // relative mode
			this.value_(this.value + (by*2-1), which);
		}, {
			this.value_(this.spec(which).map(by), which);
		});
	}
	label {
		| which=0 |
		^label[which];
	}
	labels {
		^label;
	}
	label_ {
		| text which=0 |
		if(text.isKindOf(Array), {
			text.do({
				| t n |
				this.label_(t, n);
			});
		}, {
			if(label.size <= which, {
				this.prGrow(which+1);
			});
			label[which] = text;
		});
		this.refresh;
	}
	spec {
		| which=0 |
		^spec[which];
	}
	specs {
		^spec;
	}
	spec_ {
		| cspec which=0 |
		if(cspec.isKindOf(Array), {
			cspec.do({
				| sp n |
				this.spec_(sp, n);
			});
		}, {
			if(spec.size <= which, {
				this.prGrow(which+1);
			});
			if(this.labels.size <= which or: {this.label(which).isNil}, {
				this.label_(cspec.asString, which);
			});
			// numberbox.step_(cspec.step?{cspec.guessNumberStep});
			// numberbox.clipLo_(cspec.minval);
			// numberbox.clipHi_(cspec.maxval);
			spec[which] = cspec.asSpec;
		});
		this.refresh;
	}
	prGrow {
		| newsize |
		vals = vals.growClear(newsize-vals.size).collect({
			| x |
			if(x.isNil, 0, x);
		});
		spec = spec.growClear(newsize-spec.size);
		label = label.growClear(newsize-label.size);
	}
	set {
		| value which=0 |
		// knob.value_(spec.unmap(value));
		numberbox.value_(value);
		if(vals.size <= which, {
			this.prGrow(which+1);
		});
		vals[which] = value;
		action.value(value, which);
		this.refresh;
	}
	value {
		| which=0 |
		^this.vals[which];
	}
	value_ {
		| value which=0 |
		^this.set(value, which);
	}
}

Ctl : Pattern { // FIX: make the 'spec' global also?
	classvar <>all;
	var <key, value, <spec;
	*initClass {
		all = IdentityDictionary.new;
	}
	*new {
		| key value |
		var res = Ctl.all.at(key);
		if(res.isNil, {
			res = Ctl.realNew(key, value?1);
			Ctl.all.put(key, res);
		}, {
			if(value.notNil, {
				Ctl.all.at(key).value_(value);
			});
		});
		^Ctl.all.at(key);
	}
	*realNew {
		| key value |
		^super.newCopyArgs(key, value).init;
	}
	*at {
		| key |
		^Ctl.all.at(key);
	}
	*allView {
		| parent bounds |
		var view, num;
		view = UserView(parent, bounds)
		.layout_(GridLayout());
		num = Ctl.all.keys.size.sqrt.floor;
		Ctl.all.keys.asArray.sort.do { // try also: the .clump method for SequenceableCollections.
			| key n |
			view.layout.add(Ctl(key).view, (n/num).floor, n%num);
		};
		^view;
	}
	*allGui {
		var win;
		win = Window("Ctls");
		win.front;
	}
	init {
		this.spec_(\unipolar.asSpec);
	}
	value {
		| val |
		if(val.isNil, {
			^value;
		}, {
			^this.value_(val);
		});
	}
	value_ {
		| val |
		if(val.isNumber, {
			value = val;
		}, {
			"Ctl values must be numbers.".error;
		});
	}
	+= {
		| num |
		^this.value_(this.value+num);
	}
	-= {
		| num |
		^this.value_(this.value-num);
	}
	*= {
		| num |
		^this.value_(this.value*num);
	}
	/= {
		| num |
		^this.value_(this.value/num);
	}
	embedInStream {
		| event |
		while {
			this.value.notNil;
		} {
			this.value.yield;
		};
	}
	next {
		^this.value;
	}
	spec_ {
		| cspec |
		spec = cspec.asSpec;
	}
	set {
		| value |
		^this.value_(value);
	}
	storeOn {
		| stream |
		stream << "Ctl('" << this.key.asString << "', " << this.value.asString << ")";
	}
	printOn {
		| stream |
		^this.storeOn(stream);
	}
	view {
		/*
			TODO:
			* make it possible to set the spec (or just the range) via GUI
			* make relative interface (i.e. an interface for +=, -=, *=, /=, etc)
		*/
		| parent bounds |
		var view, tf, kn;
		view = View(parent, bounds)
		.layout_(VLayout());
		tf = TextField(view)
		.value_(this.value.asString);
		kn = Knob(view)
		.value_(this.value)
		.mode_(\vert)
		.resize_(5)
		.shift_scale_(0.01)
		.action_({
			| knob |
			this.value_(this.spec.map(knob.value));
			tf.value_(this.value.asString);
		})
		.toolTip_(this.key);
		view.layout.add(kn);
		view.layout.add(tf);
		^view;
	}
	gui {
		var win, view;
		win = Window(this.key + "Ctl");
		view = this.view(win).resize_(5).bounds_(win.view.bounds);
		win.front;
		^win;
	}
}

+ Pattern {
	findCtls {
		var res = [];
		case(
			{ this.respondsTo(\patternpairs) }, {
				this.patternpairs.do({
					| pv |
					if(pv.isKindOf(Ctl), {
						res = res ++ [pv];
					}, {
						if(pv.isKindOf(Pattern), {
							res = res ++ pv.findCtls;
						});
					});
				});
			},
			{ this.respondsTo(\source) and: { this.source != this } }, {
				res = res ++ this.source.findCtls;
			},
			{ this.respondsTo(\pattern) and: { this.pattern.respondsTo(\findCtls) } }, {
				res = res ++ this.pattern.findCtls;
			},
			{ this.respondsTo(\list) }, {
				this.list.do({
					| pv |
					if(pv.isKindOf(Ctl), {
						res = res ++ [pv];
					}, {
						if(pv.isKindOf(Pattern), {
							res = res ++ pv.findCtls;
						});
					});
				});
			},
		);
		^res;
	}
}