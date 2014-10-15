PianoKeys {
	/*
		PianoKeys - Use your computer keyboard as if it were a piano keyboard.
		I wrote this on Linux and haven't tested it elsewhere, so it probably won't work if you're using anything else.

		FIX: make it possible to change what the keys affect (i.e. instead of always midinote)
		FIX: make it possible for different keys to play different synths (i.e. for a "drumkit")
	*/
	var <>rec=false, <>keys, <>synth;
	var <>octave=4;
	var <>onPress, <>onRelease; // User-specified functions.
	var <>down; // Event of keys that are currently down and information about them. Probably don't attempt to access this directly unless you must.
	var <>running=true;
	var recorded;
	*new {
		^super.new.init;
	}
	*midinote { // return the midinote that the key would produce.
		| char octave=4 |
		var ioc = "zsxdcfvbhnjm,l.;/'q2w3e4rt6y7ui9o0p-[]".indexOf(char);
		if(ioc.isNil, {
			^nil;
		}, {
			if(ioc > 17, {
				ioc = ioc - 6;
			});
			^(octave*12)+ioc;
		});
	}
	init {
		this.keys_("zsxdcfvbhnjm,l.;/'");
		this.synth_(\default);
		down = ();
		recorded = [];
		this.prCheckForReleasedKeys;
	}
	prCheckForReleasedKeys { // periodically check for released keys. To stop this task, set the instance's 'running' variable to false.
		SystemClock.sched(0, {
			down.keys.do({
				| key |
				var dif = (Date.localtime.rawSeconds - down[key][\last]);
				if((dif > 0.2) or: {down[key][\presses] > 1} and: { dif > 0.05 }, {
					var char = key.asString[0];
					this.prReleased(char);
					this.onRelease.(char);
				});
			});
			if(running, 0.01, nil);
		});
	}
	press { // call this when a key on the keyboard is pressed.
		| char | // should be an actual Char object.
		var ioc = this.keys.indexOf(char);
		var csym = char.asSymbol;
		var new = down[csym].isNil; // no 'char' in the down event, so this is a key that is not currently pressed.
		if(new, {
			down[csym] = (
				last: Date.localtime.rawSeconds,
				presses: 0,
			);
			this.onPress.(char);
			if(ioc.notNil, {
				var note = (octave*12)+ioc;
				down[csym] = down[csym] ++ (
					synth: Synth(this.synth, [\freq, note.midicps]),
					note: note,
					beat: TempoClock.beats,
				);
			});
		});
		down[csym][\last] = Date.localtime.rawSeconds;
		down[csym][\presses] = down[csym][\presses]+1;
		^new;
	}
	release { // call this whenever you get a "key release" event from the GUI.
		| char |
		^this.press(char);
	}
	prReleased {
		| char |
		var ioc = this.keys.indexOf(char);
		var csym = char.asSymbol;
		if(ioc.notNil, {
			down[csym][\synth].release;
			if(rec, {
				recorded = recorded ++ [(midinote: down[csym][\note], dur: TempoClock.beats - down[csym][\beat])];
			});
		});
		down[csym] = nil;
	}
	stopAll {
		down.keys.do({
			| key |
			if(down[key][\synth].notNil, {
				down[key][\synth].release;
			});
		});
	}
	stop {
		this.running = false;
		this.stopAll;
	}
	restart {
		this.running = true;
		this.prCheckForReleasedKeys;
	}
}