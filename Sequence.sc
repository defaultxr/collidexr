// IDEA: maybe use EventListPlayer from the Mx quark to play the list of events?

Sequence { // represents a sequence of notes.
	var <>list, <protoEvent, >length, <>yType=\midinote; // length is the length in "beats"
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
		stream << "Sequence(*" << this.asList.cs << ")"
		<< if(this.protoEvent.isNil, "", ".protoEvent_(" ++ this.protoEvent.cs ++ ")")
		<< if(length.isNil, "", ".length_(" ++ this.length.cs ++ ")")
		<< if(this.yType == \midinote, "", ".yType_(" ++ this.yType.cs ++ ")");
	}
	actualLength {
		var res = list.reject(_.isNil).collect({
			| evt |
			evt[\beat]+(evt[\sustain]?1)
		});
		^if(res.size == 0, 1, { res.maxItem.ceil; }); // don't allow a sequence of length 0 (to make it "loop-safe"). otherwise, return the length.
		// FIX: remove loop safety (it should be in the looper's code, not here).
		// FIX: might want to apply a quant to the length, i.e. to round it to the nearest beat?
	}
	length {
		^if(length.isNil, { // if length is nil, find the last playing note in the sequence and use that as the end point.
			this.actualLength;
		}, length);
	}
	length2 { // length, counting only when notes begin.
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
		^this.asList.select({
			| event |
			event.notNil && { (event[\beat] >= start) and: {event[\beat] < end} };
		});
	}
	protoEvent_ { // FIX: a protoEvent should be provided to the stream, not the pattern itself?
		| event |
		protoEvent = event?(instrument:\default);
	}
	rawList {
		^list.reject(_.isNil);
	}
	asList {
		var resAry = [];
		var ary = this.rawList.sortBy(\beat);
		ary.do({ // FIX: should detect "silent" sections and use rests instead of a long dur!!!
			| event index |
			var res = event.deepCopy;
			res[\dur] = if(index == (ary.size-1), {
				res[\sustain];
			}, {
				ary[index+1][\beat]-res[\beat];
			});
			resAry = resAry ++ [res];
		});
		if(length.notNil and: { this.actualLength < length }, { // if the specified length is longer than the actual length, pad the list with a rest at the end to sync.
			resAry = resAry ++ [(type:\rest, dur:(length-this.actualLength))];
		});
		// FIX: should trim list if the specified length is less than the actual length.
		^resAry;
		// ^if(resAry==[], [(type:\rest)], resAry);
	}
	asPattern {
		^Pchain(protoEvent, Pseq(this.asList));
	}
	play {
		| clock protoEvent quant |
		this.asPattern.play(clock, protoEvent, quant);
	}
	fork {
		| clock quant protoEvent |
		^this.play(clock, protoEvent, quant);
	}
	record {
		| mode=\midi |
	}
	recordGui {
		^this.recorder.makeWindow;
	}
	recorder {
		^UserView().background_(Color.red);
	}
}