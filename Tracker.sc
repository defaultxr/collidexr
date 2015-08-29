// Tracker.sc
// class files for the SuperCollider tracker.

TrackerPattern { // one pattern for the tracker.
	var <>steps = 0; // the number of steps in the pattern
	var <>headings = #[]; // the heading for each track
	var <>tracks = #[]; // the actual track data. this will be a list of lists. the "steps" var doesn't have to match the length of each list in this array; if "steps" is less, the missing steps are assumed blank. if it's more, it only plays "steps" steps.
	var <>beatsteps = 4; // number of steps in a beat.
	*new {
		| steps=64 headings=#[] |
		^super.newCopyArgs(steps, headings).init;
	}
	// *load { // Don't even need this - use Object.readArchive instead
	// 	"Dude... Loading patterns doesn't work yet...".postln;
	// }
	init {
		tracks = Array.newClear(headings.size);
	}
	deleteTrack {
		| track |
		tracks.removeAt(track);
		headings.removeAt(track);
	}
	addTrack {
		| heading |
		headings = headings.add(heading);
		tracks = tracks.add(Array.newClear(this.steps));
	}
	getStep {
		| track step |
		if((tracks[track].size <= step) || (step > steps),
			{ ^nil; },
			{
				if(step == (-1),
					{ ^headings[track]; },
					{ ^tracks[track][step]; });
			});
	}
	setStep {
		| track step val |
		if((tracks[track].size <= step) || (step > steps),
			{ ^nil; },
			{
				if(step == (-1),
					{ headings[track] = val; },
					{ tracks[track][step] = val; });
			});
	}
	playTrackStep {
		| track step |
		var stepval;
		stepval = if((tracks[track].size <= step) || (step > steps),
			{ nil },
			{ tracks[track][step]; });
		if((stepval != nil) && (stepval != ""), {
			(headings[track].replace("%%", stepval)).interpret;
		});
	}
	playStep {
		| step |
		headings.do({
			| heading track |
			this.playTrackStep(track, step);
		});
	}
	play {
		{
			steps.do({
				| step |
				this.playStep(step);
				(1/beatsteps).wait;
			});
		}.fork;
	}
	beats {
		^(steps*(1/beatsteps));
	}
	// save { // don't need this - use tracker_pattern.writeArchive instead (read the documentation for Object)
	// 	"Saving is... uh...".postln;
	// }
	asPattern { // the trackerpattern as a normal supercollider pattern. might be hard to do!
		"NOT DONE YET!".postln;
	}
}

TrackerSong { // a 'song'
	var <>title;
	var <patterns; // a list containing all the TrackerPatterns
	var <>song = #[]; // a list containing the indexes of the patterns from the patterns array that comprise the song
	*new {
		| title |
		^super.newCopyArgs(title).init;
	}
	init {
		patterns = [];
	}
	add { // add a TrackerPattern to the TrackerSong internal list.
		| pattern |
		patterns = patterns.add(pattern);
		^(patterns.size-1);
	}
	remove { // remove a TrackerPattern from the internal list, also erasing all references to it from the song list.
		| pattern | // should be the pattern number returned by 'add'.
		patterns.removeAt(pattern);
		song = song.reject(_==pattern);
	}
	getPattern {
		| index |
		^patterns[song[index]];
	}
	playPattern {
		| pattern |
		this.getPattern(pattern).play;
	}
	play {
		{
			song.do({
				| pattern |
				this.playPattern(pattern);
				this.getPattern(pattern).beats.wait;
			});
		}.fork;
	}
}

Tracker {
	// config:
	var <>font;
	var <>txtwidth = 196;
	var <>txtheight = 28;
	var <>sidebar = 26;
	var <>top = 100;
	var <>disabledColor, <>interfaceColor, <>backgroundColor, <>rowColors;
	var <>repeat = false;
	// state:
	var <>song;
	var <position; // position is an array: [pattern_at_top_of_screen, row_at_top_of_screen, col_at_left_of_screen]
	var <played; // [pattern, step] played.
	var lb, lp, <open = false;
	// gui elements:
	var win, title, posboxes, rows, headers, txts;
	// functions
	var keymap;
	var kDA;
	*new {
		| song |
		^super.new.init(song??{TrackerSong.new});
	}
	init {
		| song |
		this.song = song;
		// config
		font = Font("ProFont", 15);
		disabledColor = Color.gray(0.5);
		interfaceColor = Color.black;
		backgroundColor = Color.gray;
		rowColors = [Color.black, Color.blue];
		// state
		position = [0, 0, 0];
		// functions
		keymap = Keymap(( // commented this out cuz i'll fix all of it later. needs a lot of restructuring.
			// 'C-a': {
			// 	AppClock.sched(0, { view.select(0, 0); });
			// },
			// 'C-e': {
			// 	view.select(view.string.size, 0);
			// },
			// 'C-b': {
			// 	view.select(view.selectionStart-1, 0);
			// },
			// 'C-f': {
			// 	view.select(view.selectionStart+1, 0);
			// },
			// 'M-b': {
			// 	this.focus(x-1, y);
			// 	true;
			// },
			// 'M-f': {
			// 	this.focus(x+1, y);
			// 	true;
			// },
			// 'M-n': {
			// 	this.focus(x, y+1);
			// 	true;
			// },
			// 'M-p': {
			// 	this.focus(x, y-1);
			// 	true;
			// },
			// 'C-n': { // should be "next line or next step"
			// },
			// 'C-p': { // should be "previous line or previous step"
			// },
			// 'C-j': {
			// 	this.confirm(x, y);
			// 	true;
			// },
			// 'M-j': {
			// 	this.confirm(x, y);
			// 	true;
			// },
			// 'C-t': { // "trigger"
			// 	if(cxy != false && y != (-1), {
			// 		this.playStep(position[0], cxy[1]);
			// 	});
			// 	true;
			// },
			// 'M-t': {
			// 	if(cxy != false, {
			// 		this.play(position[0], cxy[1].max(0));
			// 	});
			// 	true;
			// },
			// 'M-s': {
			// 	this.stop;
			// 	true;
			// },
			// 'M-up': {
			// 	this.focus(x, y-1);
			// 	true;
			// },
			// 'M-left': {
			// 	this.focus(x-1, y);
			// 	true;
			// },
			// 'M-down': {
			// 	this.focus(x, y+1);
			// 	true;
			// },
			// 'M-right': {
			// 	this.focus(x+1, y);
			// 	true;
			// },
		));
		kDA = { // keyDownAction
			| x y view char modifiers unicode keycode |
			var cxy = this.represents(x, y);
			var cpat = song.getPattern(position[0]);
			var ret;
			AppClock.sched(0, {
				this.colorize(x, y);
			});
			ret;
		};
	}
	show {
		if(open, { win.front; ^this }, { lb = nil; });
		open = true;
		win = Window("Tracker");
		win.onClose = { open = false; };
		title = TextField(win, Rect(0, 0, txtwidth, txtheight)).background_(interfaceColor).stringColor_(interfaceColor.complementary).string_(song.title).action_({
			| field |
			song.title = field.value;
		});
		posboxes = [
			TextField(win, Rect(txtwidth, 0, sidebar, txtheight)).string_(position[0].asString).action_({
				| field |
				position[0] = field.value.asInteger;
				win.refresh;
			}),
			TextField(win, Rect(txtwidth+sidebar, 0, sidebar, txtheight)).string_(position[1].asString).action_({
				| field |
				position[1] = field.value.asInteger;
				win.refresh;
			}),
			TextField(win, Rect(txtwidth+(sidebar*2), 0, sidebar, txtheight)).string_(position[2].asString).action_({
				| field |
				position[2] = field.value.asInteger;
				win.refresh;
			}),
		];
		rows = Array.newClear(50);
		headers = Array.newClear(10);
		txts = Array.newClear(10);
		win.drawFunc = {
			| view |
			var numrows = ((view.bounds.height-(txtheight+top))/txtheight).floor;
			var extratop = ((view.bounds.height-(txtheight+top))%txtheight);
			var numcols = ((view.bounds.width-sidebar)/txtwidth).floor;
			Pen.color = interfaceColor;
			Pen.addRect(Rect(0, 0, view.bounds.width, top+extratop));
			Pen.addRect(Rect(0, top+extratop, sidebar, view.bounds.height-(top+extratop+txtheight)));
			Pen.addRect(Rect(0, view.bounds.height-txtheight, view.bounds.width, txtheight));
			Pen.perform(\fill);
			if(view.bounds != lb,
				{ // redraw the interface only if the window was resized.
					rows.do {
						| item n |
						rows[n].remove;
						rows[n] = nil;
					};
					(numrows-1).do {
						| n |
						rows[n] = StaticText(view, Rect(0, top+extratop+txtheight+(n*txtheight), sidebar, txtheight));
						rows[n].font_(font);
					};
					headers.do {
						| item n |
						headers[n].remove;
						headers[n] = nil;
					};
					numcols.do {
						| n |
						headers[n] = TextView(view, Rect(sidebar+(n*txtwidth), top+extratop, txtwidth, txtheight));
						headers[n].font_(font).hasHorizontalScroller_(false).hasVerticalScroller_(false);
						headers[n].keyDownAction_(kDA.(n, -1, _, _, _, _, _));
						headers[n].usesTabToFocusNextView_(true);
					};
					txts.do {
						| txt col |
						txt.do {
							| item n |
							txt[n].remove;
							txt[n] = nil;
						};
						txts[col] = Array.newClear(numrows);
					};
					(numrows-1).do {
						| row |
						numcols.do {
							| col |
							txts[col][row] = TextView(view, Rect(sidebar+(col*txtwidth), top+extratop+txtheight+(row*txtheight), txtwidth, txtheight));
							txts[col][row].font_(font).hasHorizontalScroller_(false).hasVerticalScroller_(false);
							txts[col][row].keyDownAction_(kDA.(col, row, _, _, _, _, _));
							txts[col][row].usesTabToFocusNextView_(true);
						};
					};
				});
			if(((lp != position) && (song.notNil)) || view.bounds != lb,
				{ // update the TextViews
					var cpat = song.getPattern(position[0]);
					lb = view.bounds;
					lp = position.deepCopy;
					headers.reject(_.isNil).do {
						| header n |
						var the_text = cpat.headings[n+position[2]];
						var oor = (n+position[2]) >= cpat.headings.size;
						header.string = if(oor, "", the_text);
						header.background = if(oor, disabledColor, Color.clear);
					};
					txts.reject(_.isNil).do {
						| txt x |
						txt.reject(_.isNil).do {
							| box y |
							var rep = this.represents(x, y);
							if(rep == false, { // out of range
								box.string = "";
							}, {
								var string = this.song.getPattern(position[0]).getStep(rep[0], rep[1]);
								if(string.notNil, {
									box.string = string;
								}, {
									box.string = "";
								});
							});
							this.colorize(x, y);
						};
					};
					rows.reject(_.isNil).do {
						| row n |
						var row_number = (position[1]+n);
						var color_number = (row_number/cpat.beatsteps).floor;
						row.string = if(row_number >= cpat.steps, "", row_number.asString);
						row.stringColor_(Color.white);
						row.background_(if(row_number >= cpat.steps, disabledColor, rowColors.wrapAt(color_number)));
					};
				});
			// Pen.fillColor = Color.red;
			// Pen.moveTo((view.bounds.width/2)@300);
			// Pen.lineTo((view.bounds.width)@700);
			// Pen.lineTo(0@700);
			// Pen.lineTo((view.bounds.width/2)@300);
			// Pen.fill;
		};
		win.front;
	}
	represents { // find out which track/step the GUI textview represents
		| x y | // returns false if the textview is "out of bounds"
		var res = [x+position[2], y+position[1]];
		var cpat = this.song.getPattern(position[0]);
		if((res[0] >= cpat.tracks.size) || (res[1] >= cpat.steps),
			{ ^false; },
			{ ^res; });
	}
	textbox {
		| x y |
		if(y == -1,
			{ ^headers[x]; },
			{ ^txts[x][y]; });
	}
	colorize { // properly colorize & format a textview
		| x y |
		if(y.isNil, {
			headers.size.do {
				| actualX |
				this.colorize(actualX, x);
			};
		}, {
			var cxy = this.represents(x, y);
			var view = this.textbox(x, y);
			if(view.notNil, {
				if(cxy == false, {
					// view.string_("");
					view.background = disabledColor;
				}, {
					var cbox = this.song.getPattern(position[0]).getStep(cxy[0], cxy[1]);
					if((view.string == cbox.asString) || (view.string == "" && cbox.isNil), {
						if(played.notNil && { position[0] == played[0] && cxy[1] == played[1] }, {
							view.background = Color.green(0.5, 0.5);
						}, {
							view.background = Color.clear;
						});
					}, {
						view.background = Color.red;
					});
				});
			});
		});
	}
	confirm {
		| x y |
		var cxy = this.represents(x, y);
		var view = this.textbox(x, y);
		var cpat = this.song.getPattern(position[0]);
		if (cxy == false, {
			if(y == (-1), {
				cpat.addTrack(view.string);
				lb = nil;
				this.refresh;
				{ this.textbox(cpat.headings.size-1, -1).focus; }.defer(0.01);
			}, {
				view.string = "";
			});
		}, {
			if(cxy[1] == (-1) && view.string == "", {
				cpat.deleteTrack(cxy[0]);
				lb = nil;
				this.refresh;
				{ this.textbox(cxy[0], cxy[1]).focus; }.defer(0.01);
			}, {
				cpat.setStep(cxy[0], cxy[1], view.string);
			});
		});
	}
	focus { // move the focus in the specified direction
		| x y |
		var cc = this.textbox(x, y);
		if(cc != false, { cc.focus; });
	}
	playStep { // play one step.
		| pattern step |
		var row = step-position[1];
		song.getPattern(pattern).playStep(step);
		if(played.notNil && { played[0] == position[0] }, {
			var oldrow = played[1]-position[1];
			if((oldrow >= 0) && this.textbox(0, row).notNil, {
				played = nil;
				{ this.colorize(oldrow); }.defer;
			});
		});
		played = [pattern, step];
		if(pattern == position[0] && (row >= 0) && { (txts[0][row].notNil) }, { // not sure why that last one has to be a function
			{ this.colorize(row); }.defer;
		});
	}
	play { // play the song, starting from the specified pattern.
		| pattern=0 step=0 |
		Tdef(\tracker, {
			block {
				| break |
				loop {
					// (step.asString ++ " " ++ pattern.asString).postln;
					this.playStep(pattern, step);
					(1/this.song.getPattern(pattern).beatsteps).wait;
					step = step + 1;
					if(step >= this.song.getPattern(pattern).steps, {
						step = 0;
						pattern = pattern + 1;
					});
					if(pattern >= this.song.song.size, {
						if(this.repeat == true, {
							pattern = 0;
						}, {
							break.value;
						});
					});
				};
			};
		}).play;
	}
	stop {
		Tdef(\tracker).stop;
		if(played.notNil, {
			var op = played[1];
			played = nil;
			this.colorize(op);
		});
	}
	refresh {
		win.refresh;
	}
}

TrackerTextView : TextView {
	*new {
		| w bounds id |
		^super.new.init; //.init(w, bounds, id);
	}
	init {
		| w bounds |
		super.init(w, bounds);
		this.keyDownAction = {
			| char modifiers unicode keycode |
			"no".postln;
			// this.keyDownAction2.();
		}
	}
	// defaultKeyDownAction {
	// 	| char modifiers unicode keycode |
	// 	"WHT".postln;
	// 	switch(char,
	// 		1.asAscii, // C-a
	// 		{
	// 			AppClock.sched(0, { this.select(0, 0); });
	// 			^this;
	// 		},
	// 		5.asAscii, // C-e
	// 		{
	// 			this.select(this.string.size, 0);
	// 			^this;
	// 		},
	// 		2.asAscii, // C-b
	// 		{
	// 			this.select(this.selectionStart-1, 0);
	// 			^this;
	// 		},
	// 		6.asAscii, // C-f
	// 		{
	// 			this.select(this.selectionStart+1, 0);
	// 			^this;
	// 		});
	// 	// ^nil;
	// }
}
