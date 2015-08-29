// Markers.sc
// FIX: maybe just delete Markers and rename MarkerSpec to Markers for simplicity(?)

MarkerSpec { // actual list of points.
	var >points, <>text, <>color, <>type, <>total;	
	// 'total' should be the total number of frames of the source sound file, so that a percent can be made.
	*new {
		arg points, text="", color=Color.blue;
		^super.newCopyArgs(points, text, color, \manual).init;
	}
	*extrapolate {
		arg points, text="", color=Color.blue;
		^super.newCopyArgs(points, text, color, \extrapolate).init;
	}
	*series {
		arg points, text="", color=Color.blue;
		^super.newCopyArgs(points, text, color, \series).init;
	}
	*manual {
		arg points, text="", color=Color.blue;
		^super.newCopyArgs(points, text, color, \manual).init;
	}
	init {
		if(points.isNil, {
			points = [];
		});
	}
	storeOn {
		| stream |
		stream << "MarkerSpec." << this.type << "(" << points.cs << ", " << text.cs << ", " << color.cs << ")" << if(total.isNil, "", ".total_(" ++ total.asString ++ ")");
	}
	at { // should return a single "section" - an array of 2 numbers
		| index se=nil |
		var list = this.points.sort;
		if(se.isNil, {
			^[list[index], list[index+1]];
		}, {
			if(se==0, {
				^list[index];
			}, {
				^list[index+se];
			});
		});
	}
	start {
		| index |
		^this.at(index, 0);
	}
	end {
		| index |
		^this.at(index, 1);
	}
	points {
		switch(this.type,
			\manual, {
				^points.sort;
			},
			\extrapolate, {
				var diff = [], avg;
				points.stutter.drop(1).drop(-1).doAdjacentPairs({
					| a b |
					diff = diff ++ [b-a];
				});
				avg = diff.sum/diff.size;
				^(points[0], points[0]+avg .. points.last);
			},
			\series, {
				^(points[0], points[1] .. points[2]);
			},
		);
	}
	marks {
		^this.points;
	}
	forPattern { // output for use in patterns, for sending the array of points to a synth.
		^[this.points.asArray];
	}
	sndp { // same as above, but uses percentages (0..1) instead of frame numbers, for my SndP pseudo-UGen.
		^[this.points.asArray/this.total];
	}
	add {
		| frame |
		if(points.includes(frame).not, { // don't add dupes cuz that's dumb!
			points = points.add(frame);
		});
	}
	mark {
		| frame |
		this.add(frame);
	}
	remove {
		| frame |
		points.remove(frame);
	}
	unmark {
		| frame |
		this.remove(frame);
	}
}

Markers { // several different MarkerSpecs combined. use this class for convenience.
	var <>markersList, markers;
	*new {
		| ... specs |
		^super.new.init(*specs);
	}
	init {
		| ... specs |
		if(specs.size == 0, {
			markersList = [MarkerSpec.new];
		}, {
			markersList = [];
			specs.do({
				| sp |
				this.add(sp);
			});
		});
	}
	storeArgs {
		^markersList;
	}
	add {
		| spec |
		markersList = markersList ++ [spec];
		markers = nil;
	}
	delete {
		| index |
		markersList = markersList.removeAt(index);
		markers = nil;
	}
	at {
		| n |
		^markersList[n];
	}
	prMakeMarkers {
		var res = [];
		markersList.do {
			| marker n |
			var marks = switch(marker.class,
				MarkerSpec, {
					marker.points;
				},
				Event, {
					case(
						{ marker[\start].notNil }, {
							(marker[\start], marker[\second] .. marker[\end]);
						},
						{ }, {
						});
				});
			if(marks.notNil, {
				marks = marks.collect({
					| mrk |
					(frame: mrk, color: marker.color, type: n, text: marker.text);
				});
			});
			res = res ++ marks;
		};
		markers = res.sortBy(\frame);
	}
	markers {
		this.prMakeMarkers;
		^markers.asArray;
	}
	points {
		^this.markers.collect(_[\frame]);
	}
	sf {
		^[this.points++[this.total]];
	}
	getMarkerToLeft {
		| frame |
		var marks = this.markers;
		if((marks.size == 0) or: {frame == 0}, {
			^(frame: 0);
		}, {
			var iogt = marks.collect(_[\frame]).indexOfGreaterThan(frame);
			if(iogt.isNil, {
				^marks[marks.size-1];
			}, {
				if(iogt == 0, {
					^(frame:0);
				}, {
					^marks.clipAt(iogt-1);
				});
			});
		});
	}
	getMarkerToRight {
		| frame |
		var marks = this.markers;
		if((marks.size == 0) or: {frame == -1}, {
			^(frame: -1);
		}, {
			var iogt = marks.collect(_[\frame]).indexOfGreaterThan(frame);
			if(iogt.isNil, {
				^(frame: -1);
			}, {
				^marks.clipAt(iogt);
			});
		});
	}
	getMarkersSurrounding {
		| frame num=0 | // num is how many markers to skip on each side. if 0, select the 2 nearest markers. if 1, select the 2 second-nearest markers, etc.
		var marks = this.markers;
		^[this.getMarkerToLeft(frame), this.getMarkerToRight(frame)]; // FIX: need to take num into account.
		// if(marks.size == 0, {
		// 	^[(frame:0), (frame:-1)];
		// }, {
		// 	var iogt = marks.collect(_[\frame]).indexOfGreaterThan(frame);
		// 	if(iogt.isNil, { // there is no marker to our right.
		// 		^[marks.clipAt(iogt-(1+num)), (frame:-1)];
		// 	}, { // there is a marker to our right
		// 		if(iogt == 0, { // there is no marker to our left.
		// 			^[(frame:0), marks.clipAt(iogt+num)];
		// 		}, {
		// 			^[marks.clipAt(iogt-(1+num)), marks.clipAt(iogt+num)];
		// 		});
		// 	});
		// });
	}
	getMarkersInRange {
		| start end |
		^this.markers.select({
			| mrk |
			(mrk[\frame] >= start) && {mrk[\frame] <= end};
		});
	}
	mark {
		| frame type=0 |
		markersList[type].mark(frame);
	}
	unmark {
		| frame type |
		if(type.isNil, {
			markersList.do({
				| markersspec |
				markersspec.remove(frame);
			});
		}, {
			markersList[type].remove(frame);
		});
	}
	unmarkInRange {
		| start end |
		this.getMarkersInRange(start, end).do({
			| mrk |
			this.unmark(mrk[\frame], mrk[\type]);
		});
	}
	total_ {
		| num |
		this.markersList.do({
			| m |
			m.total_(num);
		});
	}
	total {
		^this.markersList[0].total;
	}
	sndp {
		^this.markersList[0].sndp;
	}
}