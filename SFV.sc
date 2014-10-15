SFV {
	classvar oscReplyRate=30; // controls the frequency that the time-cursor-position reply is sent from server to client (higher values use more CPU!)
	var font, precision;
	var <markers, <>markType, <buffer, <soundfile, <mode, <sfv, <audiofile, <>filename; // filename = project file name.
	var <playMode;
	var synth, oscFunc;
	var <keymap, win, userview, mouseDown=false, <>messagequeue;
	*initClass {
		var sfvsp = {
			| numChannels=2 doneAction=2 bufnum=0 rate=1 start=0 end=1 retrig=1 loop=0 |
			var bufframes, line_dur, startpos, endpos, line;
			bufframes = BufFrames.kr(bufnum);
			line_dur = (bufframes/BufSampleRate.kr(bufnum)*(abs(start-end)))/abs(rate);
			startpos = ((rate>0)*start)+((rate<0)*end);
			startpos = startpos * bufframes;
			endpos = ((rate>0)*end)+((rate<0)*start);
			endpos = endpos * bufframes;
			line = Env([startpos, startpos, endpos, endpos], [0, line_dur, 0], \lin, loop*2, 0).ar(doneAction, retrig);
			SendReply.kr(Impulse.kr(oscReplyRate), "/sfv/position", [line]); // THIS IS THE LINE THAT WAS ADDED.
			BufRd.ar(numChannels, bufnum, line);
		};

		SynthDef(\_sfv_sp, {
			| amp=0.5 pan=0 out=0 |
			var output;
			output = SynthDef.wrap(sfvsp, prependArgs:[2]);
			output = Balance2.ar(output[0], output[1], pan);
			Out.ar(out, output * amp);
		}).add;

		SynthDef(\_sfv_spm, {
			| amp=0.5 pan=0 out=0 |
			var output;
			output = SynthDef.wrap(sfvsp, prependArgs:[1]);
			output = Pan2.ar(output, pan);
			Out.ar(out, output * amp);
		}).add;

		SynthDef(\_sfv_onsets, {
			| bufnum |
			var phs, output, trigs, ampl;
			phs = Line.ar(0, BufFrames.kr(bufnum), BufDur.kr(bufnum), doneAction:2);
			output = BufRd.ar(2, bufnum, phs).sum/2;
			trigs = Coyote.kr(output);
			ampl = Amplitude.ar(output);
			SendReply.kr(trigs, '/sfv/onsets', [phs, ampl]);
		}).add;

	}
	*new {
		| file |
		^super.new.init(file);
	}
	init {
		| file |
		font = Font("ProFont", 14);
		precision = 0.001;
		playMode = \mono;
		messagequeue = [];
		oscFunc = OSCFunc({
			| msg |
			AppClock.sched(0, { sfv.timeCursorPosition_(msg[3].round); });
		}, '/sfv/position');
		win = Window("Waveform");
		win.onClose = {
			("Closed SFV window with Buffer"+buffer.bufnum.asString+"("++buffer.path++") hanging.").warn;
			oscFunc.free;
		};
		sfv = SoundFileView(win, win.view.bounds).resize_(5);
		sfv.drawsWaveForm = true;
		sfv.timeCursorOn = true;
		sfv.gridOn = false;
		sfv.action = { userview.refresh; };
		sfv.waveColors = Color.green!2;
		sfv.setSelectionColor(0, Color.gray(0.1));
		sfv.mouseDownAction = {
			| view x y modifiers buttonNumber clickCount |
			var clickedFrame = this.firstFrame + (sfv.viewFrames*(x/sfv.bounds.width));
			mouseDown = true;
			if(clickCount == 2, {
				// var begin = sfv.selection(sfv.currentSelection)[0];
				// if(markers.notNil, {
				// 	var frames = markers.getMarkersSurrounding(begin).collect(_[\frame]);
				// 	sfv.setSelection(0, [frames[0], frames[1]-frames[0]]);
				// }, {
				// 	sfv.setSelection(0, [0, sfv.numFrames]);
				// });
				// userview.refresh;
				if(modifiers.isShift, {
					this.addNearby(clickedFrame); // FIX
				}, {
					this.selectNearby(clickedFrame); // FIX
				});
				true;
			});
		};
		sfv.mouseUpAction = {
			mouseDown = false;
			this.select(this.sel[\s], this.sel[\e]);
		};
		sfv.mouseMoveAction = {
			if(mouseDown, {
				this.select(this.sel[\s], this.sel[\e]);
				userview.refresh;
			});
		};
		keymap = Keymap.new.binds(( // AMM = audacity muscle memory
			// load/save stuff
			'C-x C-s': {
				this.save;
			},
			'C-o': { // "open"
				this.load;
			},
			// view stuff
			'C-e': { // C-e - zoom to selection (AMM)
				this.zoomToSelection;
			},
			'C-1': { // zoom in (AMM)
				this.zoom(1/2);
			},
			'C-3': { // zoom out (AMM)
				this.zoom(2);
			},
			// selection stuff
			'space': {
				this.playSelection;
			},
			'C-left': {
				this.mode = \left;
				this.focus(\left);
			},
			'C-right': {
				this.mode = \right;
				this.focus(\right);
			},
			'C-up': {
				this.mode = \expand;
				this.focus;
			},
			'C-down': {
				this.mode = \shrink;
				this.focus;
			},
			'left': {
				var prefix = sfv.viewFrames*0.01;
				switch(this.mode,
					\left, {
						this.resize(-1*prefix);
						this.focus(\left);
					},
					\right, {
						this.resize(0, -1*prefix);
						this.focus(\right);
					},
					\expand, {
						this.resize(-1*prefix);
						this.focus(\left);
					},
					\shrink, {
						this.resize(0, -1*prefix);
						this.focus(\right);
					},
				);
			},
			'right': {
				var prefix = sfv.viewFrames*0.01;
				switch(this.mode,
					\left, {
						this.resize(1*prefix);
						this.focus(\left);
					},
					\right, {
						this.resize(0, 1*prefix);
						this.focus(\right);
					},
					\expand, {
						this.resize(0, 1*prefix);
						this.focus(\right);
					},
					\shrink, {
						this.resize(1*prefix);
						this.focus(\left);
					},
				);
			},
			'home': {
				this.setSelection(0);
			},
			'end': {
				this.setSelection(sfv.numFrames);
			},
			'M-left': {
			},
			'M-right': {
			},
			// marks stuff
			'C-i': { // "insert" mark (AMM)
				this.mark;
			},
			'C-j': { // "join" sections (remove marks in the range - AMM)
				this.unmark;
			},
			// other stuff
			'C-g': {
				this.refresh;
			},
			'C-c C-s': { // stop all audio
				CmdPeriod.run;
			},
			'C-u C-s': { // prefix / switch modes
				var modes = [\left, \right, \expand, \shrink];
				this.mode = modes.wrapAt((modes.indexOf(this.mode)?0)+1);
				this.focus;
			},
			'C-u C-p': {
				var modes = [\mono, \poly, \toggle, \loop];
				this.playMode_(modes.wrapAt((modes.indexOf(this.playMode)?0)+1));
				this.focus;
			},
			['C-h', '?']: { // help
				this.help;
			},
		));
		userview = UserView(win, win.view.bounds).resize_(5);
		userview.acceptsMouse = false;
		userview.drawFunc = {
			var csel = sfv.selection(sfv.currentSelection);
			var start = csel[0].asString, end = (csel[0]+csel[1]).asString;
			var ps = (csel[0]/sfv.numFrames).round(precision).asString, pe = (csel[0]+csel[1]/sfv.numFrames).round(precision).asString;
			var firstframe = this.firstFrame;
			if(markers.notNil, {
				markers.markers.do {
					| mark num |
					var frame = mark[\frame];
					Pen.strokeColor = mark[\color]??Color.white;
					if((frame > firstframe) && (frame < (firstframe + sfv.viewFrames)), {
						var xpos = ((frame-firstframe)/sfv.viewFrames)*sfv.bounds.width;
						Pen.line(xpos@0, xpos@sfv.bounds.height);
					});
					Pen.stroke;
				};
			});
			// "OSD"
			Pen.font = font;
			Pen.fillColor = Color.white;
			Pen.strokeColor = Color.white;
			if(messagequeue.size > 0, {
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
					Pen.stringAtPoint(text, 10@(sfv.bounds.height-(15*(num+2))-10), font, item[\color]??Color.white);
				});
			});
			Pen.stringAtPoint(if(csel[1] != 0,
				{ "Start: " ++ start ++ " (" ++ ps ++ ") End: " ++ end ++ " (" ++ pe ++ ") Length: " ++ csel[1].asString; },
				{ "Frame: " ++ start ++ " (" ++ ps ++ ")" }), sfv.bounds.leftBottom + (2@(-15)));
			Pen.stroke;
		};
		win.view.keyDownAction = {
			| view char modifiers unicode keycode |
			var res = keymap.keyDown(Keymap.stringifyKey(modifiers, keycode));
			if(res.isFunction, {
				res.value(sfv);
			});
		};
		win.view.keyUpAction = {
			| view char modifiers unicode keycode |
			userview.refresh;
		};
		userview.refresh;
		win.layout = StackLayout(userview, sfv).mode = \stackAll;
		if(file.notNil, {
			this.load(file);
		});
		this.dataReset;
		mode = \expand;
		win.front;
	}
	// window stuff
	// show { // FIX - make this work again.
	// 	if(open, { win.front; ^this; });
	// 	open = true;
	// }
	// load/save stuff
	dataReset {
		markType = 0;
		markers = Markers.new;
	}
	refresh {
		userview.refresh;
	}
	load {
		| file |
		if(file.notNil, { // FIX: in the future, should be able to load projects as well as audio files with this method.
			case(
				{file.isKindOf(String)}, {
					var ext = file.splitext[1];
					if((ext.notNil) && {[\wav, \aif, \aiff, \ogg, \mp3].includes(ext.toLower.asSymbol)}, {
						this.loadAudioFile(file);
					}, {
						var archive = Object.readArchive(file);
						this.loadAudioFile(archive.audiofile);
						this.markers = archive.markers;
					});
				},
				{file.isKindOf(Buffer)}, { // FIX
					this.loadAudioFile(file.path);
					// "Can't load from Buffers yet.".error;
				},
			);
		}, {
			Dialog.openPanel({
				| file |
				this.load(file);
			});
		});
	}
	save {
		| file |
		if(file.isNil, {
			if(filename.notNil, { // save to filename
				(
					markers: this.markers,
					audiofile: audiofile,
				).writeArchive(filename);
			}, {
				Dialog.savePanel({
					| file |
					filename = file;
					this.save;
				});
			});
		}, {
			filename = file;
			this.save;
		});
	}
	// buffer stuff
	loadAudioFile {
		| file |
		if(file.notNil, {
			var converted = file.convertSong;
			audiofile = file;
			if(buffer.notNil, {
				"Freeing old buffer.".postln;
				this.free;
			});
			soundfile = SoundFile.new;
			soundfile.openRead(converted);
			sfv.soundfile = soundfile;
			sfv.read(0, soundfile.numFrames);
			buffer = Buffer.read(Server.default, converted);
			this.dataReset;
			this.markers.total_(soundfile.numFrames);
		}, {
			"Error: No file was provided.".postln;
		});
	}
	free {
		buffer.free;
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
	// view stuff
	firstFrame {
		^((sfv.scrollPos)*sfv.numFrames)-(sfv.scrollPos*sfv.viewFrames);
	}
	zoom {
		| factor |
		var csel = sfv.selection(sfv.currentSelection);
		// var mid = ((csel[0]+(csel[0]+csel[1]))/2)/sfv.numFrames;
		sfv.zoom(factor);
		this.focus;
		// sfv.scrollTo(mid);
		// sfv.scroll(mid-0.5);
	}
	zoomToSelection {
		if(sfv.selectionSize(0)==0, {
			sfv.zoomToFrac(1);
		}, {
			var cs = sfv.selection(sfv.currentSelection);
			sfv.zoomSelection(sfv.currentSelection);
			sfv.sel_(cs[0], cs[0]+cs[1]);
		});
	}
	focusOn { // shift (but do not resize) the view so that the specified frame is centered.
		| frame |
		var mid = (frame/sfv.numFrames);
		sfv.scrollTo(mid);
		sfv.scroll(mid-0.5);
		userview.refresh;
	}
	focus { // focus on the current subject.
		| subject |
		var center = (this.sel.s+this.sel.e)/2;
		switch(subject?this.mode,
			\left, {
				this.focusOn(this.sel.s);
			},
			\right, {
				this.focusOn(this.sel.e);
			},
			\shrink, {
				this.focusOn(center);
			},
			\expand, {
				this.focusOn(center);
			},
		);
	}
	// selection stuff
	mode_ {
		| theMode |
		if([\left, \right, \expand, \shrink].includes(theMode), {
			mode = theMode;
			this.message("Mode:"+theMode.asString);
		});
	}
	playMode_ {
		| theMode |
		if([\mono, \poly, \toggle, \loop].includes(theMode), {
			playMode = theMode;
			this.message("Play mode:"+theMode.asString);
		});
	}
	selection {
		var csel = sfv.selection(sfv.currentSelection);
		var start = csel[0];
		var end = start + csel[1];
		^(\s:start, \e:end);
	}
	sel {
		| sel |
		^this.selection(sel);
	}
	select {
		| start end |
		var se = [start, end].replace(-1, sfv.numFrames).clip(0, sfv.numFrames);
		var newStart, newEnd;
		if(end.isNil, {
			case(
				{ start.class == Array }, {
					this.select(start[0], start[1]);
				},
				{ start.isKindOf(Number) }, {
					this.select(start, start);
				},
			);
		}, {
			se = se.sort;
			newStart = se[0];
			newEnd = se[1];
		});
		sfv.setSelection(sfv.currentSelection, [newStart, newEnd-newStart]);
		if(playMode == \loop and: { synth.notNil } and: { synth.isRunning }, {
			synth.set(\start, newStart/sfv.numFrames, \end, newEnd/sfv.numFrames);
		});
		this.refresh;
	}
	setSelection {
		| start end |
		^this.select(start, end);
	}
	sel_ {
		| sel |
		^this.setSelection(sel);
	}
	selectNearby {
		| sample |
		var frame = this.markers.getMarkersSurrounding(sample).collect(_[\frame]);
		this.sel_(frame[0], frame[1]);
	}
	addNearby {
		| sample |
		var frame = this.markers.getMarkersSurrounding(sample).collect(_[\frame]) ++ [this.sel.s, this.sel.e];
		this.sel_(frame.reduce(\min), frame.reduce(\max));
	}
	resize {
		| start=0 end=0 |
		var csel = this.sel;
		if(csel.s == csel.e, {
			var mv = start + end;
			this.sel_(csel.s + mv, csel.e + mv);
		}, {
			this.sel_(this.sel.s + start, this.sel.e + end);
		});
	}
	playSelection {
		| start end |
		/*
			playMode can be one of: mono, poly, toggle, loop
			mono mode plays the whole selection, restarts it when retriggered.
			poly mode plays the whole selection, plays another instance when retriggered.
			toggle mode plays the whole selection, stops when retriggered.
			loop mode loops the whole selection, stops when retriggered.
		*/
		var synthRunning, makeSynth;
		start = start ?? { sfv.selectionStart(0) };
		end = end ?? { sfv.selectionSize(0)+start };
		if(start == end, {
			end = soundfile.numFrames;
		});
		// var csel = sfv.selection(sfv.currentSelection), start = csel[0]/sfv.numFrames, end = csel[0]+csel[1]/sfv.numFrames;
		synthRunning = (synth.notNil and: {synth.isRunning});
		makeSynth = {
			| loop=false |
			synth = Synth(if(soundfile.numChannels == 1, \_sfv_spm, \_sfv_sp), [\rate, 1, \start, start/soundfile.numFrames, \end, end/soundfile.numFrames, \bufnum, buffer, \loop, loop.asInteger]).track;
		};
		switch(playMode,
			\mono, {
				if(synthRunning, {
					synth.free;
				});
				makeSynth.();
			},
			\poly, {
				makeSynth.();
			},
			\toggle, {
				if(synthRunning, {
					synth.free;
				}, {
					makeSynth.();
				});
			},
			\loop, {
				if(synthRunning, {
					synth.free;
				}, {
					makeSynth.(true);
				});
			},
		);
	}
	// marks stuff
	markers_ {
		| mrks |
		markers = mrks;
	}
	mark {
		| frame |
		if(frame.isNil, {
			var csel = sfv.selection(sfv.currentSelection);
			markers.mark(csel[0], markType);
			if(csel[1] != 0, {
				markers.mark(csel[0]+csel[1], markType);
			});
		}, {
			markers.mark(frame, markType);
		});
		this.refresh;
	}
	unmark {
		| start end |
		if(start.isNil, {
			var csel = sfv.selection(sfv.currentSelection);
			markers.unmarkInRange(csel[0], csel[0]+csel[1]);
		}, {
			markers.unmarkInRange(start, end);
		});
	}
	automark { // FIX - move this to a more appropriate class.. perhaps as an extension of Buffer?
		OSCdef(\_sfv_onsets_osc, {
			| msg |
			{this.mark(msg[3]);}.fork(AppClock);
		}, '/sfv/onsets');
		Synth(\_sfv_onsets, [\bufnum, buffer]);
	}
	// other stuff
	help {
		[
			"C-x C-s -- Save",
			"C-o -- Open",
			"C-e -- Zoom to selection",
			"C-1 -- Zoom in",
			"C-3 -- Zoom out",
			"Space -- Play",
			"C-i -- Mark",
			"C-j -- Join (Delete marks)",
			"C-c C-s -- CmdPeriod",
			"C-u C-s -- Switch selection mode",
			"C-u C-p -- Switch play mode",
		].do({
			| msg |
			this.message(msg);
		});
	}
}

