// my class extensions

BufferDirectories { // have to do this cuz i can't extend an already-defined class (String) with new classvars, apparently.
	classvar <>fileDict, <>monoFileDict;
	*initClass { // also using this initClass instead of String's because String already has one.
		SynthDef(\speakSynth, {
			| bufnum=0 amp=0.5 out=0 |
			var reader = DiskIn.ar(1, bufnum, 0);
			FreeSelfWhenDone.kr(reader);
			Out.ar(out, reader!2 * amp);
		}).add;
	}
}

+ Object {
	view { // returns a view with the name of this object's class.
		| parent bounds |
		^StaticText(parent, bounds).string_(this.class);
	}
	gui { // makes a window containing the result of the object's view method.
		var win, view;
		win = Window(this.class);
		view = this.view(win).resize_(5).bounds_(win.view.bounds);
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
}

+ String {
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
		if(BufferDirectories.fileDict.isNil, {
			BufferDirectories.fileDict = Dictionary.new;
		});
		if(File.exists(the_file), {
			the_file = the_file.convertSong;
			if(BufferDirectories.fileDict.includesKey(the_file), {
				^BufferDirectories.fileDict[the_file];
			}, {
				var sf = SoundFile.openRead(the_file);
				case( // make sure we return a buffer with 2 channels, since that's what I usually expect...
					{ sf.numChannels == 2 }, {
						BufferDirectories.fileDict[the_file] = Buffer.read(Server.default, the_file);
					},
					{ sf.numChannels == 1 }, {
						BufferDirectories.fileDict[the_file] = Buffer.readChannel(Server.default, the_file, channels:[0,0]);
					},
					{ sf.numChannels > 2 }, {
						BufferDirectories.fileDict[the_file] = Buffer.readChannel(Server.default, the_file, channels:[0,1]);
					}
				);
				sf.close;
				^BufferDirectories.fileDict[the_file];
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
		if(BufferDirectories.monoFileDict.isNil, {
			BufferDirectories.monoFileDict = Dictionary.new;
		});
		if(File.exists(the_file), {
			the_file = the_file.convertSong;
			if(BufferDirectories.monoFileDict.includesKey(the_file), {
				^BufferDirectories.monoFileDict[the_file];
			}, {
				var sf = SoundFile.openRead(the_file);
				if(sf.numChannels == 1, {
					BufferDirectories.monoFileDict[the_file] = Buffer.read(Server.default, the_file);
				}, {
					BufferDirectories.monoFileDict[the_file] = Buffer.readChannel(Server.default, the_file, channels:[0]); // FIX: this only loads the left channel.
				});
				sf.close;
				^BufferDirectories.monoFileDict[the_file];
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
		if(file.basename.splitext[1] == "wav", {
			file;
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
	speak { // so Linux users can enjoy the .speak method! Requires the external program 'espeak'.
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
	}
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
}

+ SequenceableCollection { // 
	cpsrate { ^this.performUnaryOp('cpsrate') }
	ratecps { ^this.performUnaryOp('ratecps') }
	midirate { ^this.performUnaryOp('midirate') }
	ratemidi { ^this.performUnaryOp('ratemidi') }
	beatstime { ^this.performUnaryOp('beatstime') }
	timebeats { ^this.performUnaryOp('timebeats') }
	transpose { ^this.performUnaryOp('transpose') }
	play { ^this.performUnaryOp('play') }
	normalAt {
		| index |
		^this[(index*this.size).floor];
	}
}

+ Pattern {
	fork { // for compatibility with Pdef(\blah).fork
		| clock quant protoEvent |
		^this.play(clock, protoEvent, quant);
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

+ Env {
	*adr { arg attackTime=0.01, decayTime=0.1, sustainLevel=0.5, releaseTime=1.0, peakLevel=1.0, curve = -4.0, bias = 0.0;
		^this.new(
			[0, peakLevel, peakLevel * sustainLevel, 0] + bias,
			[attackTime, decayTime, releaseTime],
			curve
		)
	}
}

+ UGen {
	delay { arg delaytime=0.1, maxdelaytime;
		^if(maxdelaytime.isNil, {
			DelayL.multiNew(this.rate, this, delaytime, delaytime);
		}, {
			DelayL.multiNew(this.rate, this, maxdelaytime, delaytime);
		});
	}
	transpose { arg semitones=0, octaves=0;
		var semi = semitones + (octaves * 12);
		^BinaryOpUGen.new('*', this, (2**(semi/12)));
	}
	beatstime { arg tempo;
		tempo = tempo ?? { TempoClock.default };
		^BinaryOpUGen.new('*', this, tempo.beatDur);
	}
}

// + QEnvelopeView {
// 	curves_ {
// 		| curve |
// 		curves = curve;
// 		^super.curves_(curves);
// 	}
// 	curves {
// 		^curves;
// 	}
// }