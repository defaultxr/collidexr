// my class extensions

XR { // FIX: research the Library class
	classvar <>fileDict, <>monoFileDict;
	*initClass { // also using this initClass instead of String's because String already has one.
		fileDict = Dictionary.new;
		monoFileDict = Dictionary.new;

		Server.default.doWhenBooted({
			SynthDef(\speakSynth, {
			| bufnum=0 amp=0.5 out=0 |
			var reader = DiskIn.ar(1, bufnum, 0);
			FreeSelfWhenDone.kr(reader);
			Out.ar(out, reader!2 * amp);
			}).add;
		});
	}
}

+ Object {
	view { // returns a view with the name of this object's class.
		/*
			ideas for this method:
			* 'edit' argument to make the view editable (maybe make a separate method for this?)
			* 'change' argument to link the view to 'parent' object, so when this one is changed, the parent is notified...
		*/
		| parent bounds |
		^StaticText(parent, bounds).string_(this.class);
	}
	makeWindow { // makes a window containing the result of the object's view method.
		var win, view;
		win = Window(this.class);
		view = this.view(win, win.view.bounds).resize_(5);
		win.front;
		^win;
	}
}

+ Symbol {
	view {
		| parent bounds |
		^StaticText(parent, bounds)
		.string_(this.asString);
	}
	desc {
		^SynthDescLib.global[this];
	}
	def {
		^this.desc.def;
	}
	resendSynthDef {
		^((this.def.cs++".add;").interpret);
	}
	requireSynthDef {
	}
}

+ String {
	split2 { // split a string by a string.
		| sep |
		var found = this.find(sep);
		^if(found.notNil, {
			[ this[..(found-1)] ] ++ this[(found+sep.size)..].split2(sep);
		}, [ this ]);
	}
	p { // shortcut for standardizePath because i hate typing long words with CamelCase.
		^this.standardizePath;
	}
	b { // buffer
		/*
			Loads a buffer with the sound specified by this string.
			You can call this method as much as you want on the same file because it keeps track of which files are loaded, only reading the file into a new Buffer if one doesn't already exist for the file.
			If the file is already loaded in a buffer via this method, that Buffer is returned.
			This doesn't keep track of freed files, though, so if you free a buffer that you loaded with this method, it will attempt to return that Buffer again, which will probably cause problems.
			If you're going to need to free Buffers, I recommend you write your own Buffer management instead of using this method to load them.
			You can still use the String.convertSong method to convert songs to a SuperCollider-compatible format, though.
		*/
		var the_file = this.p;
		if(XR.fileDict.isNil, {
			XR.fileDict = Dictionary.new;
		});
		if(File.exists(the_file), {
			the_file = the_file.convertSong;
			if(XR.fileDict.includesKey(the_file), {
				^XR.fileDict[the_file];
			}, {
				var sf = SoundFile.openRead(the_file);
				case( // make sure we return a buffer with 2 channels, since that's what I usually expect...
					{ sf.numChannels == 2 }, {
						XR.fileDict[the_file] = Buffer.read(Server.default, the_file);
					},
					{ sf.numChannels == 1 }, {
						XR.fileDict[the_file] = Buffer.readChannel(Server.default, the_file, channels:[0,0]);
					},
					{ sf.numChannels > 2 }, {
						XR.fileDict[the_file] = Buffer.readChannel(Server.default, the_file, channels:[0,1]);
					},
				);
				sf.close;
				^XR.fileDict[the_file];
			});
		}, {
			(the_file ++ " was not found.").error;
		});
	}
	bm { // mono buffer - same as the 'b' method above, but loads a mono buffer instead.
		/*
			FIX: there's lots of code duplication here. That means there's probably a better way to write these 2 methods.
		*/
		var the_file = this.p;
		if(XR.monoFileDict.isNil, {
			XR.monoFileDict = Dictionary.new;
		});
		if(File.exists(the_file), {
			the_file = the_file.convertSong;
			if(XR.monoFileDict.includesKey(the_file), {
				^XR.monoFileDict[the_file];
			}, {
				var sf = SoundFile.openRead(the_file);
				if(sf.numChannels == 1, {
					XR.monoFileDict[the_file] = Buffer.read(Server.default, the_file);
				}, {
					XR.monoFileDict[the_file] = Buffer.readChannel(Server.default, the_file, channels:[0]); // FIX: this only loads the left channel.
				});
				sf.close;
				^XR.monoFileDict[the_file];
			});
		}, {
			(the_file ++ " was not found.").error;
		});
	}
	convertSong {
		/*
			If the file specified by this string is not supported for loading by SuperCollider, it is converted to wav using the appropriate program, and then the path to the (temporary) wav file is returned.
		*/
		var dir = (Platform.defaultTempDir ++ "supercollider").mkdir; // should be a temporary directory so the converted files don't stick around and waste your harddrive space!
		var file = this.p;
		var output = dir +/+ file.basename.splitext[0] ++ ".wav";
		if([\wav, \aif, \aiff].includes(file.basename.splitext[1].toLower.asSymbol), { // things that don't need to be converted
			^file;
		}, {
			if(File.exists(output), { // the output file already exists(?), so we do nothing and just return the path to it.
				// (output ++ " already exists...").postln;
				^output;
			}, { // if the output file doesn't exist, we have to convert it.
				("Converting " ++ file ++ "...").postln;
				switch(file.splitext[1],
					"mp3", {
						("lame --decode \"" ++ file ++ "\" \"" ++ output ++ "\" 2>/dev/null").silentCommand;
					},
					"flac", {
						("flac -d \"" ++ file ++ "\" -o \"" ++ output ++ "\" 2>/dev/null").silentCommand;
					},
					"ogg", {
						("oggdec -o \"" ++ output ++ "\" \"" ++ file ++ "\" 2>/dev/null").silentCommand;
					});
				("Done converting " ++ file ++ ".").postln;
				^output;
			});
		});
	}
	silentCommand { // perform a shell command without showing the output. There might be a better way to do this! (FIX)
		var pipe = Pipe.new(this, "r");
		var line = pipe.getLine;
		while({ line.notNil },
			{ line = pipe.getLine; });
		pipe.close;
	}
	asArray {
		var res = [];
		this.do({
			| l |
			res = res ++ [l];
		});
		^res;
	}
	/*speak { // so Linux users can enjoy the .speak method! Requires the external program 'espeak'.
		/*
			This is kind of a hackish method. Espeak doesn't support JACK, so it has to be done this way.
			I figured I'd include this anyway, since speak is sort of a silly function to begin with.
		*/
		var rand = 10000.rand;
		var dir = (Platform.defaultTempDir ++ "supercollider/").mkdir;
		var filename = (dir ++ "/" ++ rand.asString ++ ".wav");
		var buf;
		("espeak -w \"" ++ filename ++ "\" \"" ++ this.quote ++ "\" &>/dev/null").unixCmd;
		fork{
			var buf = Buffer.cueSoundFile(Server.default, filename, numChannels: 1);
			var synth;
			Server.default.sync;
			synth = Synth(\speakSynth, [\bufnum, buf]);
			NodeWatcher.register(synth);
			block {
				| break |
				loop {
					1.wait;
					if(synth.isRunning.not, {
						buf.free;
						break.value;
					});
				}
			}
		}
	}*/
	drumConv {
		| symbolmap=(()) |
		var result = [];
		symbolmap = ('*':\kik, '!':\snare1, 'r':\rim, 'k':\kick, 's':\snare1, 'h':\hat, 't':\scratch) ++ symbolmap;
		this.do({
			| char |
			var schar = symbolmap[char.asSymbol];
			if(schar.notNil, {
				if(schar.isKindOf(Event), {
					result = result.add((dur:1/4)++schar);
				}, {
					result = result.add((instrument: schar, dur: 1/4));
				});
			}, {
				if([$\n, $ ].includes(char).not, {
					result = result.add((type: \rest, dur: 1/4));
				});
			});
		});
		^Pseq(result);
	}
	// drumDurConv { // should be like drumConv, but only output durs for \dur, or perhaps note types for \type.
	// 	^this.drumConv(('*'
	// }
	view {
		| parent bounds |
		^StaticText(parent, bounds)
		.string_(this);
	}
}

+ SimpleNumber { // some math stuff. Some of these might already be built into SC under a different name. Also these might not be correct. If i'm wrong, please tell me!
	cpsrate { // frequency to playback rate conversion (to allow you to play UGens like PlayBuf using a normal frequency value)
		| base |
		base = base ?? { 60.midicps }; // base note defaults to midi note 60
		^this/base;
	}
	ratecps { // FIX: needs fixing(?)
		| base |
		base = base ?? { 60.midicps };
		^base*this;
	}
	midirate {
		// midi note to playback rate conversion
		| base |
		base = base ? 60;
		^this.midicps.cpsrate(base.midicps); // i'm lazy!
	}
	ratemidi {
		| base |
		base = base ? 60;
		^this.ratecps(base.midicps).cpsmidi; // i'm lazy!
	}
	beatstime { // convert a number of beats to an amount of time (in seconds, of course) based on a TempoClock.
		| tempo |
		tempo = tempo ?? { TempoClock.default; };
		^this*(tempo.beatDur);
	}
	timebeats {
		| tempo |
		tempo = tempo ?? { TempoClock.default; };
		^this/(tempo.beatDur);
	}
	transpose { // transpose a frequency value by semitones and/or octaves.
		| semitones=0 octaves=0 | // for example: 440.transpose(1) will return a number one semitone higher than 440Hz.
		var semi = semitones + (octaves * 12);
		^this*(2**(semi/12));
	}
	remap { // map a number into a spec and out of a different one.
		| inspec outspec |
		^outspec.asSpec.map(inspec.asSpec.unmap(this));
	}
	rescale { // alias for remap
		"Use remap, not rescale!".warn;
	}
	truncString {
		| decimals=2 |
		var str = this.asString;
		var pos = str.find(".");
		if(pos.isNil, {
			^str;
		}, {
			^str[..(pos+decimals)];
		});
	}
}

+ Array {
	requireSynthDefs {
		this.do({
			| item |
			item.requireSynthDef;
		});
	}
	asEvent {
		var res = ();
		this.pairsDo({
			| e f |
			res[e] = f;
		});
		^res;
	}
}

+ SequenceableCollection { // 
	cpsrate { ^this.performUnaryOp('cpsrate') }
	ratecps { ^this.performUnaryOp('ratecps') }
	midirate { ^this.performUnaryOp('midirate') }
	ratemidi { ^this.performUnaryOp('ratemidi') }
	beatstime { ^this.performUnaryOp('beatstime') }
	timebeats { ^this.performUnaryOp('timebeats') }
	transpose { ^this.performUnaryOp('transpose') }
	remap {
		| inspec outspec |
		^this.collect({
			| n |
			n.remap(inspec, outspec);
		});
	}
	play { ^this.performUnaryOp('play') }
	normalAt {
		| index |
		^this[(index*this.size).floor];
	}
	durConv {
		/*
			converts nested arrays into a pattern.
			inspired by Alex McLean's Tidal.
			a flat array is array.size beats long.
			[\kick, \kick, \kick, \kick].durConv == Pbind(\instrument, \kick, \dur, Pseq([1, 1, 1, 1]))
			nested arrays divide that beat by the size of the nested array.
			[\kick, [\kick, \kick], \kick, \kick].durConv == Pbind(\instrument, \kick, \dur, Pseq([1, 1/2, 1/2, 1, 1]))
			you can also change the parameter that the values are:
			(50..53).durConv(param:\midinote) == Pbind(\midinote, Pseq([50, 51, 52, 53]), \dur, Pseq([1, 1, 1, 1]))
			or the default duration:
			(\kick!4).durConv(1) == Pbind(\instrument, \kick, \dur, Pseq([1/4, 1/4, 1/4, 1/4]))
		*/
		| dur param nest=0 |
		var subdiv = if(this.size != 0, this.size, 1);
		var res;
		var inst, durs;
		param = param?\instrument;
		dur = dur ?? { if(this.size != 0, this.size, 1); };
		res = this.collect({
			| item |
			if(item.class == Array, {
				item.durConv(dur/subdiv, param, nest+1);
			}, {
				(\instrument:item, \dur:dur/subdiv);
			});
		});
		if(nest == 0, {
			res = res.flat;
			inst = res.flat.collect(_[\instrument]);
			durs = res.flat.collect(_[\dur]);
			^Pbind(param, Pseq(inst), \dur, Pseq(durs));
		}, {
			^res;
		});
	}
	noteConv {
		| bpm |
		var notes = [], durs = [], res = [];
		this.flat.do {
			| current |
			var cstr = current.asString;
			var durv = [1, 1/2, 1/4][[\w, \h, \q].indexOf(cstr.keep(-1).asSymbol)];
			var note = cstr.drop(-1);
			notes = notes ++ [if([\, \r, \rest].includes(note.asSymbol), note.asSymbol, {note.notemidi})];
			durs = durs ++ [durv];
		};
		res = [\midinote, Pseq(notes, 1), \dur, Pseq(durs, 1)];
		if(bpm.notNil, {
			res = res ++ [\tempo, bpm/60];
		});
		^Pbind(*res);
	}
}

+ Event {
	fuse {
		| operation='mean' ... events |
		var ckeys = [];
		var res = ();
		this.keys.do({
			| key |
			ckeys = ckeys ++ [key];
			res[key] = ([this[key]] ++ events.collect(_[key]));
		});
		events.do({
			| event num |
			event.keys.do({
				| key |
				if(ckeys.includes(key).not, {
					ckeys = ckeys ++ [key];
					res[key] = ([event[key]] ++ (events[(num+1)..].collect(_[key])));
				});
			});
		});
		res.keys.do({
			| key |
			res[key] = Message(res[key], operation).value;
		});
		^res;
	}
	trueCompare { // because ('test':5.0, 'sustain':1) == ('test':1.0, 'sustain':1) doesn't return what you'd think it does...
		| event |
		if(this.keys != event.keys, {
			^false;
		});
		this.keys.do({
			| key |
			if(this[key] != event[key], {
				^false;
			});
		});
		^true;
	}
	trueAt {
		| item |
		^this.use({
			^item.envirGet.value;
		});
	}
	asArray { // FIX: this might conflict with the default version of this method????
		^this.keys.asArray.collect({
			| key |
			[key.asSymbol, this[key]];
		}).flat;
	}
	expandArrays {
		/*
			call this method on an Event that has an Array for a key, and all of the elements in that Array will be expanded into individual keys.
			in other words:
			([\foo, \bar, \baz]: \qux).expandArrays == (\foo: \qux, \bar: \qux, \baz: \qux)
		*/
		var res = ();
		this.keys.do({
			| key |
			var val = this[key];
			if(key.isKindOf(Array), {
				key.do({
					| item |
					res[item] = val;
				});
			}, {
				res[key] = val;
			});
		});
		^res;
	}
}

+ Buffer {
	*readInWavetable {
		| file |
		var sf, newsize, osig, nsig;
		case(
			{ file.isKindOf(String) }, {
				file = file.p;
				sf = SoundFile.openRead(file);
			},
			{ file.isKindOf(SoundFile) }, {
				sf = file;
			},
		);
		newsize = (2**(1..16)).select(_>sf.numFrames)[0];
		osig = Signal.newClear(sf.numFrames);
		sf.readData(osig);
		sf.close;
		nsig = osig.resamp1(newsize).as(Signal);
		^Buffer.loadCollection(Server.default, nsig.asWavetable);
	}
	length { // time in seconds
		^(this.numFrames/this.sampleRate);
	}
}

+ Synth { // convenience methods!
	desc {
		^SynthDescLib.global.at(this.defName.asSymbol);
	}
	info {
		^this.desc;
	}
	controls {
		^this.desc.controls;
	}
}

+ SynthDef {
	*list {
		| excludeSystem=true |
		var list = SynthDescLib.global.synthDescs.keys.asArray;
		if(excludeSystem, {
			var excludeList = (1..16).collect({
				| n |
				["system_link_audio_", "system_link_control_", "system_diskout_", "freqScope"].collect({
					| x |
					(x++n).asSymbol;
				});
			}).flat++[\freqScope0_magresponse_shm, \freqScope1_magresponse_shm, \freqScope0_magresponse, \freqScope1_magresponse, \freqScope0_shm, \freqScope1_shm];
			list = list.reject(excludeList.includes(_));
			list = list.reject({
				| e |
				e.asString[0] == $_
			});
		});
		^list;
	}
	memStore {
		this.add;
	}
}

+ Pattern {
	fork { // for compatibility with Pdef(\blah).fork
		| clock quant protoEvent |
		^this.play(clock, protoEvent, quant);
	}
	remap { // map a number into a spec and out of a different one.
		| inspec outspec |
		^outspec.asSpec.map(inspec.asSpec.unmap(this));
	}
	rec { |path, headerFormat = "wav", sampleFormat = "float", numChannels = 2, dur = nil, fadeTime = 0.2, clock(TempoClock.default), protoEvent(Event.default), server(Server.default), out = 0|
		var	buf, bus, start, recsynth, cmdp, pattern, defname, cond, startTime;
		if(dur.notNil) { pattern = Pfindur(dur, this) } { pattern = this };
		path ?? {
			if(thisProcess.platform.name == \windows) {
				path = thisProcess.platform.recordingsDir +/+ "SC_" ++ Main.elapsedTime.round(0.01) ++ "." ++ headerFormat;
			} {
				path = thisProcess.platform.recordingsDir +/+ "SC_" ++ Date.localtime.stamp ++ "." ++ headerFormat;
			};
		};
		if(path[0] != $/, {
			path = thisProcess.platform.recordingsDir +/+ path;
		});
		if(path.basename.splitext[1].isNil, {
			path = path ++ "." ++ headerFormat;
		});
		fork {
			cond = Condition.new;
			buf = Buffer.alloc(server, 65536, numChannels);
			SynthDef(defname = ("patrec"++numChannels).asSymbol, { |bufnum, bus, out|
				var	sig = In.ar(bus, numChannels);
				DiskOut.ar(bufnum, sig);
				Out.ar(out, sig);
			}).add;
			bus = Bus.audio(server, numChannels);
			server.sync(cond);
			buf.write(path, headerFormat, sampleFormat, numFrames: 0, startFrame: 0, leaveOpen: true);
			server.sync(cond);
			start = thisThread.beats;
			"Recording pattern into % at %\n".postf(path, thisThread.beats);
			recsynth = server.nextNodeID;
			OSCdef(\__rec, {
				| msg time addr recvPort |
				if(msg[1] == recsynth, {
					cond.unhang;
				});
			}, '/n_end');
			cmdp = {
				cond.unhang;
			};
			CmdPeriod.add(cmdp);
			Pprotect(
				// Pfset has a cleanupFunc, which executes even if pattern is stopped by cmd-.
				Pfset(nil,
					Pseq([
						Pfuncn({ startTime = thisThread.beats; 0 }),
						(type: \on, instrument: defname, bufnum: buf, bus: bus, out: out, id: recsynth,
							delta: 0),
						pattern <> (out: bus),
						Plazy({
							Pn((type: \rest, delta: (fadeTime ? 0)
								.roundUp(buf.numFrames / server.sampleRate)),
								1)
						}),
						Prout({ (type: \kill, id: recsynth).yield;nil.yield; }),
					], 1),
					{ "hi".postln;(type: \kill, id: recsynth).play }
				),
				// on error, killing the recsynth triggers rest of cleanup below
				{ "Het".postln;(type: \kill, id: recsynth).play }
			).play(clock, protoEvent, quant: 0);
			// clean up after recording synth stops
			cond.hang;
			OSCdef(\rec).free;
			CmdPeriod.remove(cmdp);
			buf.close.free;
			bus.free;
			server.sendMsg(\d_free, defname);
			"Finished recording % at % (% s)\n".postf(path, thisThread.beats, (thisThread.beats-start).beatstime);
		}
	}
}

+ Pdef {
	*list {
		^Pdef.all.keys.asArray;
	}
}

+ Pbind {
	view { // FIX: make this better in the future (allow editing, connect subpatterns' views to this one, etc.).
		| parent bounds |
		^View(parent, bounds)
		.layout_(VLayout(
			*this.patternpairs.clump(2).collect({
				| clump |
				HLayout(*clump.collect(_.view))
			})
		));
	}
}

+ Ndef {
	*list {
		^Ndef.all.values.collect({|i|i.activeProxies.asArray}).flat;
	}
	controls {
		^this.controlNames;
	}
}

+ NodeProxy {
	setNow { | ... args | // pairs of keys or indices and value
		nodeMap.set(*args);
		if(this.isPlaying) {
			server.sendMsg(*([15, group.nodeID] ++ args));
		}
	}
}

+ Env {
	*adr {
		| attackTime=0.01 decayTime=0.1 sustainLevel=0.5 releaseTime=1.0 peakLevel=1.0 curve=(-4.0) bias=0.0 |
		^this.new(
			[0, peakLevel, peakLevel * sustainLevel, 0] + bias,
			[attackTime, decayTime, releaseTime],
			curve
		)
	}
	*line {
		| start=0 end=1 time=1 curve=0 |
		^this.new([start, start, end, end], [0, time, 0], [0, curve, 0]);
	}
	*retrig {
		| start=0 end=1 time=1 curve=0 |
		^this.line(start, end, time, curve);
	}
	++ {
		| env | // FIX: need releaseNode, loopNode, offset!
		^Env(this.levels++env.levels, this.times++[0]++env.times, this.curves++[0]++env.curves.reverse.neg)
	}
	nearest { // provide the absolute time, and this method will return the index nearest that time.
		| time |
		^this.times.integrate.indexInBetween(time).round+1;
	}
	reverse {
		^Env(this.levels.reverse, this.times.reverse, this.curves); // FIX: need releaseNode, loopNode, offset!
	}
	insert { // insert a node into the Env without changing its duration.
		| level time curve | // 'time' is absolute
		var newlevels, newtimes, newcurves = this.curves.wrapExtend(this.times.size);
		if(time <= this.duration, {
			var dura = this.times.integrate;
			var pos = (dura.indexOfGreaterThan(time));
			if(pos == 0, {
				newtimes = [time] ++ [this.times[0]-time] ++ this.times[1..];
				newlevels = [this.levels[0]] ++ [level] ++ this.levels[1..];
				newcurves = [this.curves[0]] ++ newcurves;
			}, {
				var nt = (time-(dura[pos-1]));
				newtimes = this.times.insert(pos, nt);
				newtimes = newtimes.put(pos+1, newtimes[pos+1]-nt);
				newlevels = this.levels.deepCopy.insert(pos+1, level);
				newcurves = newcurves.deepCopy.insert(pos+1, curve??{this.curves.wrapAt(pos)});
			});
		}, {
			newlevels = this.levels ++ [level];
			newtimes = this.times ++ [(time-this.duration)];
		});
		^Env( // FIX: need releaseNode, loopNode, offset!
			newlevels,
			newtimes,
			newcurves
		);
	}
	delete { // delete a node without affecting other nodes or the Env's duration.
		| index |
		var newlevels = this.levels.deepCopy;
		var newtimes = this.times.deepCopy;
		var newcurves = if(index == 0, {
			this.curves[1..];
		}, {
			var f = this.curves.deepCopy;
			f.removeAt(index);
			f;
		});
		newlevels.removeAt(index);
		if(index == 0, {
			newtimes.put(0, newtimes.removeAt(0)+newtimes.at(0));
		}, {
			newtimes.put(index-1, newtimes.removeAt(index-1)+newtimes.at(index-1));
		});
		^Env(newlevels, newtimes, newcurves); // FIX: need releaseNode, loopNode, offset!
	}
}

+ UGen {
	delay {
		| delaytime=0.1 maxdelaytime |
		^if(maxdelaytime.isNil, {
			DelayL.multiNew(this.rate, this, delaytime, delaytime);
		}, {
			DelayL.multiNew(this.rate, this, maxdelaytime, delaytime);
		});
	}
	mdelay { // FIX
		| delaytimes=0.1 maxdelaytime |
		var lb, phase;
		maxdelaytime = maxdelaytime??{delaytimes.reduce(\max)};
		lb = LocalBuf(Server.default.sampleRate*maxdelaytime, 1);
		phase = DelTapWr.ar(lb, this);
		^DelTapRd.ar(lb, phase, delaytimes);
	}
	mdel {
		| delaytimes maxdelaytime |
		^this.mdelay(delaytimes, maxdelaytime);
	}
	transpose {
		| semitones=0 octaves=0 |
		var semi = semitones + (octaves * 12);
		^(this*(2**(semi/12)));//BinaryOpUGen.new('*', this, (2**(semi/12)));
	}
	beatstime {
		| tempo |
		tempo = tempo ?? { TempoClock.default };
		^(this*tempo.beatDur);//BinaryOpUGen.new('*', this, tempo.beatDur);
	}
}

