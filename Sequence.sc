// IDEA: maybe use EventListPlayer from the Mx quark to play the list of events?
// TODO: make shift method to shift notes left or right, wrapping them to current length

Sequence { // represents a sequence of notes.
	var <>list, <protoEvent, >dur, <>yType=\midinote; // dur is the duration in beats
	*new {
		| ... notes |
		^super.new.init(*notes);
	}
	init {
		| ... notes |
		this.protoEvent_(nil);
		list = [];
		notes.do({
			| note |
			this.add(note);
		});
	}
	storeOn {
		| stream |
		stream << "Sequence(*" << this.rawList.cs << ")"
		<< if(this.protoEvent.isNil, "", ".protoEvent_(" ++ this.protoEvent.cs ++ ")")
		<< if(dur.isNil, "", ".dur_(" ++ this.dur.cs ++ ")")
		<< if(this.yType == \midinote, "", ".yType_(" ++ this.yType.cs ++ ")");
	}
	actualDur {
		var res = list.reject(_.isNil).collect({
			| evt |
			evt[\beat]+(evt[\sustain]?1)
		});
		^res.maxItem;
	}
	collect {
		| function |
		^this.list.collect(function);
	}
	dur {
		^if(dur.isNil, { // if dur is nil, find the last playing note in the sequence and use that as the end point.
			var len = this.actualDur.ceil;
			if(len == 0, {
				1;
			}, {
				len;
			});
		}, dur);
		// FIX: might want to apply a quant to the dur, i.e. to round it to the nearest beat?
	}
	dur2 { // dur, counting only when notes begin.
		var res = list.reject(_.isNil).collect({
			| evt |
			evt[\beat];
		});
		^if(res.size == 0, 4, { res.maxItem.ceil; })
	}
	add { // use the 'beat' key in an event to specify the beat that the note starts, and 'sustain' for how long it lasts.
		| note |
		if(note.isKindOf(Event), {
			if(note[\beat].notNil, {
				note[\beat] = note[\beat].max(0);
			});
			note.removeAt(\id);
			note.removeAt(\server);
			note.removeAt(\isPlaying);
			note.removeAt(\msgFunc);
			list = list.add(note);
			^(list.size-1);
		}, {
			"notes added to Sequence must be Events!".error;
		});
	}
	adds {
		| ... notes |
		notes.do {
			| note |
			this.add(note);
		};
	}
	remove {
		| note |
		list = list.reject({
			| item |
			(item[\beat] == note[\beat]) and: { item[yType] == note[yType] };
		});
	}
	removeAt {
		| beat |
		var events = this.eventsAt(beat);
		events.do({
			| event |
			this.remove(event);
		});
	}
	removeIn {
		| start end |
		var events = this.eventsIn(start, end);
		events.do({
			| event |
			this.remove(event);
		});
	}
	size {
		^list.select(_.notNil).size;
	}
	eventsAt { // all events that start on a certain beat.
		| beat |
		^this.asList.select({
			| event |
			event.notNil && { event[\beat] == beat };
		});
	}
	eventsIn { // all the events that begin within a range.
		| start end |
		^this.rawList.select({
			| event |
			event.notNil and: { (event[\beat] >= start) and: {event[\beat] < end} };
		});
	}
	protoEvent_ { // FIX: a protoEvent should be provided to the stream, not the pattern itself?
		| event |
		protoEvent = event?(instrument:\default);
	}
	rawList {
		^list.reject(_.isNil);
	}
	asList { // NEW version
		var resAry = [];
		var ary = this.rawList.sortBy(\beat).reject({|e|e[\beat].isNil});
		ary.do({
			| event index |
			var res = event.deepCopy;
			var last = resAry.last;
			var next = ary[index+1];
			var between = res[\beat] - if(last.isNil, 0, {(last[\dur]+last[\beat])});
			res[\dur] = res[\sustain];
			if(index == 0 and: {between > 0}, {
				resAry = resAry ++ [(type:\rest, dur:between, beat:0)];
			});
			// FIX: don't use \delta
			if(next.notNil, {
				res[\delta] = (next[\beat] - event[\beat]);
			});
			resAry = resAry ++ [res];
		});
		if(resAry.size == 0, {
			resAry = [(type:\rest, dur:1)];
		}, {
			if(dur.notNil, {
				if(this.actualDur < dur, { // if the specified dur is longer than the actual dur, pad the list with a rest at the end to sync.
					var last = resAry.last;
					var d_or_s = last[\sustain];
					resAry = resAry ++ [(type:\rest, dur:(dur-this.actualDur), beat:(d_or_s+last[\beat]))];
				});
				if(this.actualDur > dur, { // FIX
					"dur smaller than actual dur is not yet supported.".error;
				});
			});
			if(this.actualDur < this.actualDur.ceil, {
				var last = resAry.last;
				resAry = resAry ++ [(type:\rest, dur:(this.actualDur.ceil-this.actualDur), beat:(this.actualDur))];
			});
		});
		^resAry;
	}
	asPattern {
		var list = this.asList;
		if(list.size == 0, {
			list = [(type:\rest, dur:1)];
		});
		^Pchain(protoEvent, Pseq(list));
	}
	play {
		| clock protoEvent quant |
		this.asPattern.play(clock, protoEvent, quant);
	}
	fork {
		| clock quant protoEvent |
		^this.play(clock, protoEvent, quant);
	}
	startRecording { // just re-initializes the Sequence. calling this is obviously not required if you just created the Sequence.
		this.init;
	}
	record { // record a note.
		| event clock=(TempoClock.default) |
		var cbeat, lat = Server.default.latency.timebeats(clock);
		if(this.protoEvent[\tempoClockStartBeat].isNil, {
			this.protoEvent_(this.protoEvent++(tempoClockStartBeat:clock.beats-lat));
		});
		cbeat = (clock.beats-this.protoEvent[\tempoClockStartBeat]-lat);
		this.add(event++(beat:cbeat));
	}
	recordFree { // record a note being freed.
		| event clock=(TempoClock.default) |
		var cbeat = (clock.beats-this.protoEvent[\tempoClockStartBeat])-Server.default.latency.timebeats(clock);
		var filteredlist = this.list.select({
			| e |
			e[\sustain].isNil and: { var cmp = e.deepCopy;cmp.removeAt(\beat);cmp.trueCompare(event); };
		});
		filteredlist[0][\sustain] = (cbeat-filteredlist[0][\beat]);
	}
	stopRecording { // end recording. basically just removes the 'tempoClockStartBeat' key from the protoEvent. not required, but keeps things cleaner.
		this.protoEvent.removeAt(\tempoClockStartBeat);
	}
	recordGui {
		^this.recorder.makeWindow;
	}
	recorder {
		^UserView().background_(Color.red);
	}
	reverse { // reverse the Sequence, keeping the same duration.
	}
	shift { // shift note onsets left (negative) or right (positive), wrapping them to the length of the Sequence.
		| beats |
	}
}

+ Pattern {
	asSequence {
		| dur force=false | // 'force' is to force creation of a Sequence even if its estimateDur method returns inf.
		// FIX
	}
}
