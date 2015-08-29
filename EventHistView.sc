/*
	CURRENT GOAL:
	* make EventHistory and EventHistView separate (or just separate the functionality?)
*/

EventHistory {
	var <>keepBeats=nil, <events; // number of beats of history to keep, or nil to keep all.

	*new {
		| events |
		^super.new.init(events);
	}

	init {
		| argevents |
		argevents.do({
			| ev |
			this.add(ev);
		});
	}

	startBeat {
		^(
			TempoClock.default.beats -
			Server.default.latency.timebeats(TempoClock.default) -
			keepBeats
		);
	}

	add { // add an event
		| event name beat |
		events = if(keepBeats.isNil, events, {
			events.reject({
				| e |
				var sustain = (e.use({ \sustain.envirGet.value; })?0);
				((e[\plotBeat] + sustain) < this.startBeat);// or:
				// { e[\plotBeat] > (this.startBeat+windowBeats+4) }; // sometimes removes events that haven't occurred yet!
			})}) ++
		[
			(if(event.isNil, {
				(type: \end);
			}, event) ++ (plotBeat:(beat??{TempoClock.default.beats})));
		];
	}

	pattern_ {
		| pattern |
		var beats = pattern.estimateDur;
		var cbeats = 0;
		var collection = [];
		var stream = pattern.asStream;
		var event;
		if(beats == inf, {
			"Infinite length pattern detected! Only finding the first 16 beats.".warn;
			keepBeats = 16;
		});
		while({ event = stream.next(()); event.notNil and: { if(keepBeats.notNil, { cbeats < keepBeats }, true); } }, {
			collection = collection ++ [event];
			this.add(event, \plot, cbeats);
			cbeats = cbeats + event[\dur];
		});
	}
	
	events_ {
		| array |
		if(array.class == Array, {
			array.do({
				| e |
				this.add(e, beat:e[\plotBeat]);
			});
		});
	}

	view {
		| parent bounds |
		var ehv = EventHistView(parent, bounds).eventHistory_(this);
		if(this.keepBeats.isNil, {
			ehv.startBeat_(0);
		});
		^ehv;
	}

}

EventHistView : SCViewHolder {
	var <parent, <bounds;
	var <eventHistory;
	var startBeat;
	var <radiusSpec, <lineWidth=1, <diagnostic=false, <accountForServerLatency=false; // remove these
	var <mode=\scroll; // \scroll or \plot // FIX - implement this.
	var <beats=16;
	var <yField=\freq, <cField=\amp;
	var <diagnostic=false;

	*new {
		| parent bounds events |
        ^super.new.init(parent, bounds, events);
    }

    init {
		| argparent argbounds argevents |
		var getValForEvent;
		eventHistory = EventHistory();
		getValForEvent = {
			| event key |
			event.use {
				key.envirGet.value;
			};
		};
		parent = argparent;
		bounds = argbounds;
		view = UserView(parent, (bounds ? Rect(0, 0, 500, 500)).asRect).background_(Color.black);
		this.events_(argevents?[]);
		view.drawFunc_({
			| view |
			var sb = this.startBeat;
			var pxPerBeat = (view.bounds.width/beats);
			if(diagnostic, {
				Pen.color_(Color.white);
				Pen.stringAtPoint("startBeat:"+sb.asString, 2@2);
				Pen.stringAtPoint("events.size:"+(this.events.size).asString, 2@12);
			});
			this.events.reject({|e|e.isRest}).do({
				| event |
				var xpos = (event[\plotBeat]-sb)*pxPerBeat;
				if(event[\type] == \end, {
					Pen.width_(1);
					Pen.color_(Color.red);
					Pen.line(Point(xpos, 0), Point(xpos, view.bounds.height));
				}, {
					var yval = getValForEvent.(event, yField);
					var ypos = yval.remap(yField, [view.bounds.height, 0, 1]);
					var radius = getValForEvent.(event, cField).remap(cField, radiusSpec);
					Pen.color_(Color.white);
					Pen.addOval(Rect(xpos-radius, ypos-radius, radius*2, radius*2));
					if(lineWidth.notNil, {
						Pen.width_(lineWidth);
					}, {
						Pen.width_((radius/2).max(1)); // FIX - improve this then make lineWidth default to nil.
					});
					Pen.line(Point(xpos, ypos), Point(xpos+(getValForEvent.(event, \sustain)*pxPerBeat), ypos));
				});
				Pen.stroke;
			});
		});
		this.view_(view);
		this.radiusSpec_([2, 10]);
		view.animate_(true).frameRate_(20);
	}

	add { // add an event
		| event name beat |
		this.eventHistory.add(event, name, beat);
		if(thisThread.clock != AppClock, {
			{ this.refresh; }.defer;
		}, {
			this.refresh;
		});
	}

	pattern_ {
		| pattern |
		var ebeats = pattern.estimateDur;
		this.eventHistory.pattern_(pattern);
		if(ebeats == inf, {
			"Infinite length pattern detected! Only showing the first 16 beats.".warn;
			beats = 16;
		}, {
			beats = ebeats;
		});
		this.animate_(false);
		this.refresh;
	}

	events {
		^this.eventHistory.events;
	}

	events_ {
		| array |
		this.eventHistory.events_(array);
		this.refresh;
	}

	eventHistory_ {
		| hist |
		if(hist.isKindOf(EventHistory), {
			eventHistory = hist;
			this.refresh;
		});
	}
	
	startBeat {
		^(
			startBeat??{TempoClock.default.beats -
				(if(accountForServerLatency, {Server.default.latency.timebeats(TempoClock.default)}, 0)) -
				beats}
		);
	}

	startBeat_ {
		| val |
		startBeat = val;
		this.refresh;
	}

	beats_ {
		| val |
		beats = val;
		this.refresh;
	}

	yField_ {
		| val |
		yField = val;
		this.refresh;
	}

	cField_ {
		| val |
		cField = val;
		this.refresh;
	}

	radiusSpec_ {
		| val |
		radiusSpec = val;
		this.refresh;
	}

	lineWidth_ {
		| val |
		lineWidth = val;
		this.refresh;
	}

	diagnostic_ {
		| val |
		diagnostic = val;
		this.refresh;
	}

	accountForServerLatency_ {
		| val |
		accountForServerLatency = val;
		this.refresh;
	}
}

OldEventHistView : SCViewHolder {
	var <parent, <bounds, <events, <keepEvents=false, startBeat, <windowBeats=16, <yField=\freq, <cField=\amp, <radiusSpec, <lineWidth=1, <diagnostic=false, <accountForServerLatency=false;

	*new {
		| parent bounds events |
        ^super.new.init(parent, bounds, events);
    }

    init {
		| argparent argbounds argevents |
		this.makeView;
		this.events = argevents?[];
		this.radiusSpec_([2, 10]);
		view.animate_(true).frameRate_(30);
	}

	makeView {
		| argparent argbounds |
		var getValForEvent;
		getValForEvent = {
			| event key |
			event.use {
				key.envirGet.value;
			};
		};
		parent = argparent;
		bounds = argbounds;
		view = UserView(parent, (bounds ? Rect(0, 0, 500, 500)).asRect).background_(Color.black);
		view.drawFunc_({
			| view |
			var sb = this.startBeat;
			var pxPerBeat = (view.bounds.width/windowBeats);
			if(diagnostic, {
				Pen.color_(Color.white);
				Pen.stringAtPoint("startBeat:"+sb.asString, 2@2);
				Pen.stringAtPoint("events.size:"+(events.size).asString, 2@12);
			});
			events.reject({|e|e.isRest}).do({
				| event |
				var xpos = (event[\plotBeat]-sb)*pxPerBeat;
				if(event[\type] == \end, {
					Pen.width_(1);
					Pen.color_(Color.red);
					Pen.line(Point(xpos, 0), Point(xpos, view.bounds.height));
				}, {
					var yval = getValForEvent.(event, yField);
					var ypos = yval.remap(yField, [view.bounds.height, 0, 1]);
					var radius = getValForEvent.(event, cField).remap(cField, radiusSpec);
					Pen.color_(Color.white);
					Pen.addOval(Rect(xpos-radius, ypos-radius, radius*2, radius*2));
					if(lineWidth.notNil, {
						Pen.width_(lineWidth);
					}, {
						Pen.width_((radius/2).max(1)); // FIX - improve this then make lineWidth default to nil.
					});
					Pen.line(Point(xpos, ypos), Point(xpos+(getValForEvent.(event, \sustain)*pxPerBeat), ypos));
				});
				Pen.stroke;
			});
		});
		this.view_(view);
		^view;
	}

	// view {
	// 	if(view.isNil, {
	// 		"view is nil".postln;
	// 		^nil;
	// 	}, {
	// 		^view;
	// 	});
	// }

	// remove {
	// 	view = nil;
	// }

	add { // add an event
		| event name beat |
		events = if(keepEvents, events, {
			events.reject({
				| e |
				var sustain = (e.use({ \sustain.envirGet.value; })?0);
				((e[\plotBeat] + sustain) < this.startBeat);// or:
				// { e[\plotBeat] > (this.startBeat+windowBeats+4) }; // sometimes removes events that haven't occurred yet!
			})}) ++
		[
			(if(event.isNil, {
				(type: \end);
			}, event) ++ (plotBeat:(beat??{TempoClock.default.beats})));
		];
	}

	pattern_ {
		| pattern |
		var beats = pattern.estimateDur;
		var cbeats = 0;
		var collection = [];
		var stream = pattern.asStream;
		if(beats == inf, {
			"Infinite length pattern detected! Only showing the first 16 beats.".warn;
			windowBeats = 16;
		}, {
			windowBeats = beats;
		});
		this.startBeat_(0);
		while({ cbeats < windowBeats }, {
			var event = stream.next(());
			collection = collection ++ [event];
			this.add(event, \plot, cbeats);
			cbeats = cbeats + event[\dur];
		});
		this.animate_(false);
		this.refresh;
	}

	events_ {
		| array |
		if(array.class == Array, {
			array.do({
				| e |
				this.add(e, beat:e[\plotBeat]);
			});
		});
	}
	
	keepEvents_ {
		| val |
		keepEvents = val;
		this.refresh;
	}

	startBeat {
		^(
			startBeat??{TempoClock.default.beats -
				(if(accountForServerLatency, {Server.default.latency.timebeats(TempoClock.default)}, 0)) -
				windowBeats}
		);
	}

	startBeat_ {
		| val |
		startBeat = val;
		this.refresh;
	}

	windowBeats_ {
		| val |
		windowBeats = val;
		this.refresh;
	}

	yField_ {
		| val |
		yField = val;
		this.refresh;
	}

	cField_ {
		| val |
		cField = val;
		this.refresh;
	}

	radiusSpec_ {
		| val |
		radiusSpec = val;
		this.refresh;
	}

	lineWidth_ {
		| val |
		lineWidth = val;
		this.refresh;
	}

	diagnostic_ {
		| val |
		diagnostic = val;
		this.refresh;
	}

	accountForServerLatency_ {
		| val |
		accountForServerLatency = val;
		this.refresh;
	}
}

+ Pattern {
	plot {
		var win, ehv;
		win = Window("Pattern plot");
		ehv = EventHistView(win, win.view.bounds).resize_(5).pattern_(this);
		win.front;
		^ehv;
	}
}