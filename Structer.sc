/*
	Structer.sc - Structer and relevant extensions...
	* Structer
	* Row
	* StepSequence
	* ststop, stplay, lookView, inspectView for SynthDef, Pdef, Ndef
	* estimateBeats for Pbind, Pchain, Pdef
	* estimateLength for Pbinop (FIX: submit as pull request to ddwPatterns)
CURRENT GOAL:
* fix EventHistory and EventHistView (now in EventHistView.sc)
*/

Structer {
	var <>rows;
	var <>elements, <>eevents;
	var <>ehs;
	var <>ehvs, <>lvs, <>ivs; // EventHistViews, EventHistView views, lookViews, and inspectViews
	var <>subViewModes; // FIX: remove this, make it per-element.
	var <>keymap;
	var <>toolbarview;
	var <>tempoview, <>statusview;
	var <>mainview;
	var <>ctf;
	var <>inspectorview;
	var <>messagequeue;
	var <>events;
	var <>playing;
	var <>selected;
	var <isOpen;
	*initClass {
		Spec.add(\amp, [0, 1, \lin, 0, 0.5, ""]); // normally this has a default of 0 and a warp of 'amp'
		Spec.add(\out, [0, Server.default.options.numOutputBusChannels-1, \lin, 1, 0, ""]);
	}
	*new {
		^super.new.init;
	}
	init {
		rows = [];
		events = ();
		ehs = (); // EventHistorys
		ehvs = (); // EventHistViews
		lvs = (); // lookViews
		ivs = (); // inspectViews
		subViewModes = ();
		playing = []; // list of names of things that are playing
		elements = ();
		eevents = ();
		selected = [];
		isOpen = false;
		messagequeue = [];
		keymap = Keymap((
			'C-z': {
				"stumpish emacs".unixCmd;
			},
			'C-c C-c': {
				this.message("typed C-c C-c");
			},
			'C-g': {
				this.select;
			},
			'C-f': { // 'focus'
				var values = this.eevents;
				this.query("Select", values.collect(_[\keyname]) ++ values.collect(_[\name]), {
					| val |
					this.select(val.asSymbol);
				});
			},
			'p': { // FIX: should change these "global" per-element functions to be M- prefixed
				this.playElement;
			},
			's': {
				this.stopElement;
			},
			'l': {
				this.message("lookview");
				this.setSubViewMode(this.selected[0], \lv);
			},
			'e': {
				this.message("eventhistview");
				this.setSubViewMode(this.selected[0], \ehv);
			},
			'C-a': {
				var patterns = Pdef.list;
				var synths = SynthDef.list;
				var ndefs = Ndef.list;
				var items;
				patterns = patterns.collect({|i|"P:"+i.asString});
				synths = synths.collect({|i|"S:"+i.asString});
				ndefs = ndefs.collect({|i|"N:"+i.asString});
				items = patterns ++ synths ++ ndefs;
				this.query("Add", items, {
					| val |
					var type = val.at(0);
					var name = val[3..];
					switch(type.asSymbol,
						\P, {
							this.add(Pdef(name.asSymbol));
						},
						\S, {
							this.add(name.asSymbol.def);
						},
						\N, {
							this.add(Ndef(name.asSymbol));
						},
					);
				});
			},
			'C-d': { // FIX - make this.
				this.message("DELETE...");
			},
			'M-:': {
				this.query("Exec:", [], {
					| val |
					this.message(val.interpret, raw:true);
					// this.message(val.replace("pate", "getElementEvent(~icontext)").interpret, raw:true);
				});
			},
			[\A, \B, \C, \D, \E, \F, \G, \H, \I, \J, \K, \L, \M, \N, \O, \P, \Q, \R, \S, \T, \U, \V, \W, \X, \Y, \Z]: {
				| key |
				this.select(key.asString[0]);
			},
		));
	}
	makeWindow {
		var win;
		win = Window("Structer");
		win.layout_(VLayout().spacing_(0).margins_(0));
		win.layout.add(this.makeToolbarView(win.view));
		win.layout.add(this.makeMainView(win.view), 12);
		win.layout.add(this.makeInspectorView(win.view), 4);
		isOpen = true;
		win.view.keyDownAction_({
			| view char modifiers unicode keycode |
			var stringified = Keymap.stringifyKey(modifiers, keycode);
			keymap.keyDown(stringified).value(stringified);
		});
		^win.front;
	}
	makeToolbarView {
		| parent |
		toolbarview = UserView(parent, parent.view.bounds).resize_(5);
		toolbarview.layout_(HLayout().spacing_(0).margins_(0));
		toolbarview.layout.add(StaticText().string_("Structer").background_(Color.black).stringColor_(Color.white).font_(Font("ProFont", 20)), 16);
		tempoview = StaticText().align_(\center).font_(Font("ProFont", 20));
		tempoview.mouseWheelAction_({
			| view x y modifiers xdelta ydelta |
			TempoClock.default.tempo_(((TempoClock.tempo*60)+(0.1*(ydelta/15))).round(0.1)/60);
		});
		toolbarview.layout.add(tempoview, 1);
		statusview = StaticText();
		{
			block {
				| break |
				loop {
					tempoview.background_(Color.yellow((1-TempoClock.default.beats+(Server.default.latency.timebeats(TempoClock.default))%1).squared.squared/2+0.5));
					tempoview.string_((TempoClock.default.tempo*60).round(0.01));
					statusview.string = " cpu:".scatArgs(Server.default.avgCPU.truncString(2)++"%", "   u:", Server.default.numUGens, "s:", Server.default.numSynths, "g:", Server.default.numGroups, "d:", Server.default.numSynthDefs, " ");
					0.1.wait;
					if(this.isOpen.not, {
						break.value;
					});
				}
			}
		}.fork(AppClock);
		toolbarview.layout.add(statusview, 4);
		^toolbarview;
	}
	makeEH {
		| name |
		var eh = EventHistory().keepBeats_(4);
		this.ehs[name.asSymbol] = eh;
		^eh;
	}
	makeEHV {
		| name |
		var ehv = EventHistView(mainview, Rect(0,0,0,0))
		.beats_(4).lineWidth_(1).radiusSpec_([1,5]).accountForServerLatency_(true)
		.eventHistory_(this.ehs[name.asSymbol]);
		this.ehvs[name.asSymbol] = ehv;
		^ehv;
	}
	makeLV {
		| name |
		var element = this.getElement(name);
		if(element.respondsTo(\lookView), {
			var lv = element.lookView(mainview, Rect(0,0,0,0), this);
			this.lvs[name.asSymbol] = lv;
			^lv;
		}, {
			^nil;
		});
	}
	makeIV {
		| name |
	}
	getRect {
		| n row bwidth=200 bheight=100 insetPx=2 |
		^Rect((if(n==0,n,n+1)*bwidth), (row*bheight), bwidth*if(n==0,2,1), bheight).insetBy(insetPx)
	}
	getVRect {
		| n row |
		^(this.getRect(n, row).resizeBy(-4, -23).moveBy(2, 21));
	}
	makeMainView {
		| parent |
		var font;
		mainview = UserView(parent, parent.view.bounds).resize_(5).background_(Color.yellow);
		font = Font("Fixed [Misc]", 16);
		isOpen = true;
		mainview.background_(Color.fromHexString("#AAAAAA"));
		mainview.drawFunc_({
			| view |
			Pen.color_(Color.black);
			Pen.font_(font);
			rows.do({
				| row r |
				row.array.do({
					| element n |
					var name, eevent, rect, irect, vrect, ccc;
					name = element[\element].name.asSymbol;
					eevent = this.getElementEvent(name);
					rect = this.getRect(n, r);
					irect = Rect(rect.left+2, rect.top+2, 18, 18);
					vrect = this.getVRect(n, r);
					Pen.fillColor_(Color.black);
					Pen.stringAtPoint(name.asString, rect.leftTop.translate(Point(22, 5)));
					Pen.fill;
					// indicator box
					ccc = 0.5+(
						if(this.ehs[name].notNil, {
							var beatdiffs, diff;
							beatdiffs = this.ehs[name].events.reject({
								| e |
								(e[\type] == \end) or: { e.isRest };
							})
							.collect({
								| e |
								// the difference between the current beat and the event's beat is calculated
								TempoClock.default.beats-(e[\plotBeat]+(Server.default.latency.timebeats(TempoClock.default)));
							});
							beatdiffs = beatdiffs.reject(_<0); // filter out events that have "played" but have not sounded yet (because of server latency)
							if(beatdiffs.isNil, { beatdiffs = []; });
							diff = beatdiffs.reduce(\min); // the most recent beat's time difference
							if(diff.isNil, 0, {
								((0.5-diff.clip(0, 0.5)).abs*2).squared/2;
							});
						}, 0);
					);
					Pen.fillColor_(Message(Color, if(playing.includes(name), \green, \red), [ccc]).value);
					Pen.addRect(irect);
					Pen.fill;
					// keyname
					if(eevent[\keyname].notNil, {
						Pen.color_(Color.green);
						Pen.stringInRect(eevent[\keyname].asString.toUpper, irect);
					});
					Pen.color_(if(this.selected.includes(name), Color.blue, Color.black));
					Pen.width_(if(this.selected.includes(name), 3, 1));
					Pen.addRect(rect);
					Pen.stroke;
					Pen.width_(1);
					if(subViewModes[name].notNil, {
						switch(subViewModes[name],
							\ehv, {
								if(this.ehvs[name].notNil, {
									this.ehvs[name].view.bounds_(vrect);
								}, {
									this.makeEHV(name);
									// Pen.stringAtPoint("NO EHV", rect.leftTop.translate(22@22));
								});
							},
							\lv, {
								if(this.lvs[name].notNil, {
									this.lvs[name].bounds_(vrect);
								}, {
									this.makeLV(name);
									this.lvs[name].bounds_(vrect);
									// Pen.stringAtPoint("NO LV", rect.leftTop.translate(22@22));
								});
							},
						);
					});
					// if(element.respondsTo(\lookView), {
					// 	if(~eviews[name].notNil, {
					// 		// ~eview[name].bounds_(vrect);
					// 	}, {
					// 		~eviews[name] = element.lookView(view, vrect);
					// 		switch(~eviews[name].class,
					// 			EventHistView, {
					// 				~eviews[name].beats_(4).lineWidth_(1).radiusSpec_([1,5]).accountForServerLatency_(true);
					// 			},
					// 		);
					// 	});
					// }, {
					// 	Pen.stringAtPoint("Element has no lookView", rect.leftTop.translate(22@22));
					// });
				});
			});
			// messages - FIX
			messagequeue = messagequeue.select({
				| item |
				(item[\time]-(Date.localtime.rawSeconds-item[\added]))>0;
			});
			messagequeue.select({|x|x[\text].notNil or: { x[\obj].notNil }}).reverse.do({
				| item num |
				var text = if(item[\text].notNil, {
					item[\text];
				}, {
					item[\obj].cs;
				});
				Pen.stringAtPoint(text, 10@(view.bounds.height-(15*(num+2))-10), font, item[\color]??Color.black);
			});
		});
		mainview.onClose_({
			isOpen = false;
		});
		mainview.refresh;
		mainview.animate_(true);
		mainview.frameRate_(20);
		^mainview;
	}
	setSubViewMode {
		| name mode |
		name = name.asSymbol;
		subViewModes[name] = mode;
		if(isOpen, {
			var xy = this.getXYOfElement(name), nbounds = this.getVRect(xy[0], xy[1]);
			switch(mode,
				\lv, {
					this.makeLV(name);
					lvs[name].bounds_(nbounds);
					ehvs[name].remove;
				},
				\ehv, {
					this.makeEHV(name);
					lvs[name].remove;
					lvs[name] = nil;
				},
			);
		});
	}
	makeInspectorView {
		| parent |
		inspectorview = UserView(parent, parent.view.bounds.height_(200)).resize_(8).background_(Color.gray(0.1)).drawFunc_({
			var pate = this.getElementEvent(this.selected[0]);
			Pen.font_(Font("ProFont", 20));
			Pen.color_(Color.white);
			Pen.stringAtPoint(this.selected[0].asString, 2@2);
			Pen.stringAtPoint(pate.cs, 2@25);
		});
		^inspectorview;
	}
	eventRecv {
		| event name beat |
		name = name.asSymbol;
		// if(event.isRest.not and: { event.notNil }, {
		// 	if(events[name].isNil, {
		// 		events[name] = ();
		// 	});
		// 	events[name][\lastEvents] = events[name][\lastEvents].reject({
		// 		| e |
		// 		(TempoClock.default.beats-e[\beat]) > 2; // we don't need a long buffer...
		// 	}) ++ [(event: event, beat: beat)];
		// });
		if(ehs[name].notNil, {
			ehs[name].add(event, name, beat);
		});
	}
	add {
		| item |
		var name = item.name.asSymbol;
		elements[name] = item;
		eevents[name] = (
			name:name,
			keyname:[\a, \b, \c, \d, \e, \f, \g, \h, \i, \j, \k, \l, \m, \n, \o, \p, \q, \r, \s, \t, \u, \v, \w, \x, \y, \z].choose,
			vals:(),
		);
		subViewModes[name] = \lv;
		rows = rows ++ [Row(item)];
		this.makeEH(name);
		if(isOpen, {
			this.setSubViewMode(name, \ehv);
		});
	}
	del {
		| name |
		var item = this.getElementEvent(name);
		
	}
	select {
		| name |
		if(name.isNil, {
			selected = [];
			this.message("Deselected all");
		}, {
			var event = this.getElementEvent(name);
			if(selected.includes(event[\name]).not, {
				selected = [event[\name]];
				this.message("Selected"+event[\name].asString);
			});
			// this.inspect(event[\name]); // FIX
		});
	}
	getElement {
		| name |
		^elements[name];
	}
	getElementEvent {
		| name |
		^switch(name.class,
			Symbol, {
				eevents[name];
			},
			String, {
				this.getElementEvent(name.asSymbol);
			},
			Char, {
				this.getElementEventByKeyName(name);
			},
		);
	}
	getElementEventByKeyName {
		| key |
		var res = this.eevents.keys.select({
			| e |
			((this.eevents[e][\keyname].toLower.asSymbol)==(key.toLower.asSymbol));
		}).asArray;
		^if(res.size > 0, {
			this.eevents[res[0]];
		}, nil);
	}
	getXYOfElement {
		| name |
		^block {
			| break |
			rows.do({
				| r n |
				if(r.names.includes(name), {
					break.value([r.names.indexOf(name), n]);
				});
			});
		}
	}
	playElement {
		| name |
		if(name.isNil, {
			if(this.selected.size > 0, {
				^this.playElement(this.selected[0]);
			}, {
				this.message("Cannot play nil.");
			});
		}, {
			if(playing.includes(name).not, {
				playing = playing ++ [name];
				this.getElement(name).stplay((), this); // FIX - should include parameters
			}, {
				("already playing"+name.asString).postln;
			});
		});
	}
	stopElement {
		| name |
		if(name.isNil and: {this.selected.size > 0}, {
			^this.stopElement(this.selected[0]);
		}, {
			var element = this.getElement(name);
			playing.remove(name);
			if(element.respondsTo(\ststop), {
				element.ststop((), this);
			});
		});
	}
	message {
		| obj time=5 raw=false |
		if(raw, {
			messagequeue = messagequeue ++ [(obj:obj, time:time, added:Date.localtime.rawSeconds)];
		}, {
			if(obj.notNil, {
				messagequeue = messagequeue ++ if(obj.isKindOf(String), {
					[(text:obj, time:time, added:Date.localtime.rawSeconds)];
				}, {
					[(obj:obj, time:time, added:Date.localtime.rawSeconds)];
				});
			});
		});
	}
	query { // TODO: add history for each query by name ('text' arg).
		| text completions action cancelAction |
		ctf = CompletingTextField(mainview, Rect(mainview.bounds.width/2-100, 0, 200, 25));
		ctf.completions = completions.collect(_.asString);
		cancelAction = case(
			{ cancelAction.isNil }, { { this.message("Cancelled."); } },
			{ cancelAction == false }, { nil },
			{ true }, { cancelAction },
		);
		ctf.keyDownAction_({
			| view char modifiers unicode keycode |
			if(keycode == 9 /* esc */ or: {modifiers.isCtrl and: {unicode == 7} /* C-g */}, {
				cancelAction.value;
				ctf.remove;
				mainview.focus;
				true;
			});
		});
		ctf.action_({
			| ctfv |
			action.value(ctfv.string);
			ctfv.remove;
			mainview.focus;
		});
		ctf.focus;
	}
}

Row {
	var <>array;
	*new {
		| ... args |
		^super.new.init(*args);
	}
	init {
		| ... args |
		array = [];
		args.do({
			| thearg |
			this.add(thearg);
		});
	}
	at {
		| index | // index can be integer or symbol
		if(index.isKindOf(Number), {
			^array[index];
		}, {
			"NOT DONE YET".error; // FIX
		});
	}
	set {
		| index item value |
		this.at(index)[item] = value;
	}
	add {
		| thing params pos=(-1) |
		var ev = if(thing.isKindOf(Event), {
			if(thing[\name].isNil, {
				thing = (thing ++ (name:thing[\element].name));
			});
			thing;
		}, {
			var res = ((element:thing, name:thing.name) ++ (params?()));
			res;
		});
		if(pos == (-1), {
			array = array ++ [ev];
		}, {
			array = array.insert(ev, pos);
		});
	}
	del {
	}
	names {
		^array.collect({|e|e[\name].asSymbol});
	}
	elements {
		^array.collect(_[\element]);
	}
}

StepSequence {
	var <>steps, <>numSteps, <>stepLength, <>name, <>protoEvent;
	*new {
		| ... steps |
		^super.new.init(*steps);
	}
	init {
		| ... argsteps |
		steps = nil.dup(argsteps.size);
		argsteps.do({
			| step n |
			this.set(n, step);
		});
		numSteps = steps.size;
		stepLength = 1/4;
		name = (\unnamed++10000.rand).asSymbol;
		protoEvent = (instrument:\default);
	}
	off {
		| x |
		steps[x] = steps[x] ++ (type:\rest);
	}
	on {
		| x |
		steps[x] = steps[x] ++ (type:\note);
	}
	set {
		| x v |
		case(
			{ [Symbol, String].includes(v.class); }, {
				if([\, \rest].includes(v), {
					this.off(x);
				}, {
					steps[x] = (instrument: v);
				});
			},
			{ v.isKindOf(Number) }, {
				if(v > 0, {
					this.on(x);
				}, {
					this.off(x);
				});
			},
			{ v.class == Event }, {
				steps[x] = v;
			},
			{ v.isNil }, {
				this.off(x);
			},
			{ v == false }, {
				this.off(x);
			},
			{ v == true }, {
				this.on(x);
			},
		);
	}
	at {
		| x |
		^steps[x];
	}
	get {
		| x |
		^this.at(x);
	}
	toggle {
		| x |
		if(this.isActive(x), {
			this.off(x);
		}, {
			this.on(x);
		});
	}
	isActive {
		| x |
		var step;
		if(x.isKindOf(Number), {
			step = this.get(x);
		}, {
			step = x;
		});
		if(step.isNil or: { step.class == Event and: { step[\type] == \rest } }, {
			^false;
		}, {
			^true;
		});
	}
	activeSequence {
		^this.numSteps.collect({
			| step |
			this.isActive(step);
		});
	}
	asPattern {
		^Prout({
			var ev, n = 0;
			while({ ev = this.at(n); ev.notNil }, {
				(ev++(dur:this.stepLength)++this.protoEvent).yield;
				n = n + 1;
			});
		});
	}
	lookView {
		| parent bounds |
		var view;
		view = UserView(parent, bounds);
		view.drawFunc_({
			| view |
			var bw, bh;
			bw = view.bounds.width/(this.numSteps.min(16));
			bh = view.bounds.height/((this.numSteps/16).max(1)).ceil;
			this.steps.do({
				| v n |
				var rect = Rect(n*bw, (n/16).floor, bw, bh).insetBy(1);
				Pen.addRect(rect);
				if(this.isActive(n), {
					var inst;
					Pen.fillColor_(Color.blue);
					Pen.fill;
					inst = (v[\instrument]??{this.protoEvent[\instrument]}).asString;
					Pen.color_(Color.black);
					Pen.stringInRect(inst, rect);
					Pen.stroke;
				}, {
					Pen.strokeColor_(Color.black);
					Pen.stroke;
				});
			});
			Pen.fill;
		});
		view.mouseDownAction_({
			| view x y mod |
			var div, rX, ydiv, rY;
			div = (view.bounds.width/16);
			rX = (x/div).floor;
			ydiv = (view.bounds.height/(this.numSteps/16).max(1));
			rY = (y/ydiv).floor;
			this.toggle((rX + (rY*16)));
			view.refresh;
		});
		^view;
	}
}

+ Pdef {
	name {
		^this.key.asSymbol;
	}
	stplay { // FIX...
		| params structer |
		var play, prout;
		play = Pfwd(this, Message(structer, \eventRecv), this.name);
		prout = Prout({
			| inevent |
			while({ structer.playing.includes(this.name); }, {
				var stream, out;
				stream = play.asStream;
				while({ out = stream.next(inevent); out.notNil; }, {
					out.yield;
				});
			});
		});
		^prout.play(quant:params[\quant]);
	}
	ststop {
		^this;
	}
	lookView {
		| parent bounds |
		^UserView(parent, bounds).background_(Color.yellow);
	}
	inspectView {
		| parent bounds |
		^UserView(parent, bounds).background_(Color.green);
	}
	withFwd { // FIX
		^nil; // ^Pfwd(
	}
}

+ Ndef {
	name {
		^this.key.asSymbol;
	}
	stplay {
		| params structer |
		^this.play;
	}
	ststop {
		| params structer |
		^this.stop;
	}
	lookView {
		| parent bounds |
		var uv = UserView(parent, bounds);
		var ctrls = this.controlKeysValues.asEvent;
		uv.layout_(HLayout());
		this.controlKeys.do({
			| name |
			if(name == \bufnum, { // TODO: make CKnob do this instead of having special cases for it.
				uv.layout.add(
					VLayout()
					.add(StaticText().string_(name.asString))
					.add(
						NumberBox()
						.value_(ctrls[name])
						.action_({
							| nb |
							this.setNow(name, nb.value);
						})
						.step_(1)
					)
				);
			}, {
				uv.layout.add(CKnob(label:name.asString, spec:name, action:{
					| v |
					this.setNow(name, v);
				}).value_(ctrls[name]).view);
				// uv.layout.add(VLayout().add(StaticText().string_(name.asString)).add(Knob()));
			});
		});
		^uv;
	}
}

+ SynthDef {
	stplay {
		| params structer |
		var name = this.name.asSymbol;
		var synth = Synth(name, structer.eevents[name][\vals].asArray);
		if(this.hasGateControl, {
			structer.eevents[name][\synth] = synth;
		}, {
			structer.playing.remove(name);
		});
		^synth;
	}
	ststop {
		| params structer |
		var name = this.name.asSymbol;
		if(structer.eevents[name][\synth].notNil, {
			structer.eevents[name][\synth].release;
		});
	}
	lookView { // FIX: de-duplicate this and the Ndef method
		| parent bounds structer |
		var uv = UserView(parent, bounds);
		var ctrls = this.allControlNames;
		uv.layout_(HLayout());
		ctrls.do({
			| ctrl |
			var name = ctrl.name.asSymbol, val = ctrl.defaultValue;
			if(name == \bufnum, { // TODO: make CKnob do this instead of having special cases for it.
				uv.layout.add(
					VLayout()
					.add(StaticText().string_(name.asString))
					.add(
						NumberBox()
						.value_(val)
						.action_({
							| nb |
							structer.eevents[this.name.asSymbol][\vals][name] = nb.value;
						})
						.step_(1)
					)
				);
			}, {
				uv.layout.add(CKnob(label:name.asString, spec:name, action:{
					| v |
					structer.eevents[this.name.asSymbol][\vals][name] = v;
				}).value_(val).view);
				// uv.layout.add(VLayout().add(StaticText().string_(name.asString)).add(Knob()));
			});
		});
		^uv;
	}
	inspectView {
		| parent bounds |
		^UserView(parent, bounds).background_(Color.blue);
	}
}

+ Pbind {
	estimateBeats {
		var subpatterns = this.patternpairs.reject({
			| e i |
			i.even or: { e.isKindOf(Pattern).not };
		});
		var min = if(subpatterns.size == 0,
			inf,
			{ subpatterns.collect(_.estimateLength).reduce(\min); }
		);
		if(min == inf, {
			^inf;
		}, {
			^this.asStream.nextN(min, ()).reject(_.isNil).collect(_[\dur]).sum;
		});
	}
}

+ Pdef {
	estimateBeats {
		^this.pattern.estimateBeats;
	}
}

+ Pchain {
	estimateBeats {
		^this.patterns.collect(_.estimateBeats).reduce(\min);
	}
}

+ Pbinop { // TODO: submit this as a pull request to ddwPatterns
	estimateLength {
		var patterns = [this.a, this.b].reject({
			| e |
			e.isKindOf(Pattern).not;
		});
		if(patterns.size == 0, {
			^inf;
		}, {
			^patterns.collect(_.estimateLength).reduce(\min);
		});
	}
}