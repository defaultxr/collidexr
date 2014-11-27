Prun : Pclutch { // pattern is "run"; the values are latched for as long as the duration of the original pattern says.
	embedInStream { arg inval;
		var stream = pattern.asStream;
		var outval;
		var heldsince = thisThread.clock.beats;
		outval = stream.next(inval);
		loop {
			if((thisThread.clock.beats - heldsince) >= outval[\dur], {
				outval = stream.next(inval);
				heldsince = thisThread.clock.beats;
			});
			outval.yield;
		}
	}
}

Pr : FilterPattern { // pretty much copied from Pstutter
	var <>repeats;
	*new { arg pattern, repeats=inf;
		^super.new(pattern).repeats_(repeats)
	}
	storeArgs { ^[pattern, repeats] }
	embedInStream { arg event;
		var inevent, nn;

		var stream = pattern.asStream;
		var nstream = repeats.asStream;

		while {
			(inevent = stream.next(event)).notNil
		} {
			if((nn = nstream.next(event++(pr:inevent))).notNil, {
				nn.abs.do {
					event = inevent.copy.yield;
				};
			}, {
				^event
			});
		};
		^event;
	}
}

Ps : Pattern {
	var <key;
	classvar <>all;
	*initClass {
		all = IdentityDictionary.new;
	}
	*new {
		| key |
		var strm, res;
		strm = all[key];
		if(strm.isNil, {
			var pdef = Pdefn.all.at(key);
			if(pdef.isNil, {
				pdef = Pdef.all.at(key);
			});
			all[key] = pdef.asStream;
		});
		^super.newCopyArgs(key)
	}
	storeArgs { ^[key] }
	asStream {
		^all[key];
	}
	// embedInStream { arg inval; ^this.all[key].value(inval) }
}

Prs : Pattern {
	*new {
		| key repeats=inf |
		^Pr(Ps(key), repeats);
	}
}

Pfuse : Pattern { // 'fuse' two event streams into one using event.fuse
	var <>operation, <>pattern1, <>pattern2, <>mix;
	*new {
		| operation='mean' pattern1 pattern2 mix=0 | // 'mix' is like pan
		^super.newCopyArgs(operation, pattern1, pattern2, mix);
	}
	storeArgs { ^[operation, pattern1, pattern2, mix] }
	embedInStream {
		| event |
		var ev1, ev2, stream1, stream2;
		stream1 = pattern1.asStream;
		stream2 = pattern2.asStream;
		while({
			(ev1 = stream1.next(event)).notNil and:
			{ (ev2 = stream2.next(event)).notNil };
		}, {
			ev1.fuse(operation, ev2).yield;
		});
	}
}

Pmix : Pattern { // mix between 2 or more patterns (i.e. PanAz's method)
}

Pline : Pattern {
	*initClass {
		SynthDef(\_pline, {
			| start=0 end=1 dur=1 out=0 |
			Out.kr(out, Line.kr(start, end, dur, doneAction: 2));
		}).add;
	}
	*new {
		var server = nil;
		var serverToUse = server ?? Server.default;
		var bus = Bus.control(serverToUse, 1);
		var lineSynth;
		// ^super.newCopyArgs(
	}
}

// IDEA: "Parp": use one pattern as an "arpeggiation" for a main pattern. similar to Pchain, but the arpeggiator pattern is restarted for every note in the main pattern, and only lasts for the duration of the note in the main pattern (i.e. with Pfindur)

Parp : Pattern { // "Arpeggiator" - retrigger a note with a possible transformation
	var <>arpeggiator, <>pattern;
	*new { arg arpeggiator, pattern;
		^super.newCopyArgs(arpeggiator, pattern);
	}
	storeArgs { ^[arpeggiator, pattern] }
	embedInStream { arg event;
		var inevent, nn;
		var ptn = if(pattern.isKindOf(Symbol), Pdef(pattern), pattern);
		var stream = ptn.asStream;
		while {
			(inevent = stream.next(event)).notNil
		} {
			var dur = inevent[\dur];
			var child = Psync(arpeggiator, dur, dur).asStream;
			var childevent;
			while {
				(childevent = child.next(inevent)).notNil
			} {
				childevent.yield;
			}
		}
		^event;
	}
}

Parpf : Pattern { // same as Parp, except the arpeggiator pattern is forked (so it can be any length) - TODO
	var <>arpeggiator, <>pattern;
	*new { arg arpeggiator, pattern;
		^super.newCopyArgs(arpeggiator, pattern);
	}
	storeArgs { ^[arpeggiator, pattern] }
	embedInStream { arg event;
		var inevent, nn;
		var ptn = if(pattern.isKindOf(Symbol), { Pdef(pattern) }, pattern);
		var stream = ptn.asStream;
		while {
			(inevent = stream.next(event)).notNil
		} {
			arpeggiator.play(thisThread.clock, inevent, 0);
			(inevent ++ (\type:\rest)).yield;
		}
		^event;
	}
}

Parpm : Pattern { // like Parp, except the pattern's notes are subdivided without retriggering (a la Pmono)
	var <>arpeggiator, <>pattern;
	*new { arg arpeggiator, pattern;
		^super.newCopyArgs(arpeggiator, pattern);
	}
	storeArgs { ^[arpeggiator, pattern] }
	embedInStream { arg event;
		var inevent, nn;
		var stream = pattern.asStream;

		while({
			(inevent = stream.next(event)).notNil;
		}, {
			var dur=inevent[\dur], child, childevent, n=0;
			child = if(arpeggiator.isKindOf(Pbind), arpeggiator.pairs, arpeggiator);
			child = Pmono(*([inevent[\instrument]]++child));
			child = Psync(child, dur, dur).asStream;
			while({
				(childevent = child.next(inevent)).notNil;
			}, {
				childevent.yield;
				n = n + 1;
			});
		});
		^event;
	}
}

Pf : Pattern { // "filter"
	*new {
		| base filter pattern |
		var res;
		if(base.isKindOf(Symbol), {
			base = Pdef(base);
		});
		if(filter.isKindOf(Symbol), {
			filter = Pdef(filter);
		});
		if(pattern.isKindOf(Symbol), {
			pattern = Pdef(pattern);
		});
		res = [filter, pattern, base];
		res = res.reject({|x|x.isNil});
		^Pchain(*res);
	}
}

Pg : Pattern { // "global" Pseries
	classvar <>all;
	var <>key;
	*initClass {
		all = ();
	}
	*del { // delete one key
		| key |
		all[key] = nil;
	}
	*reset {
		| key |
		^Pg.del(key);
	}
	*clear {
		all = ();
	}
	*new {
		| key |
		if(all[key].isNil, {
			all[key] = 0;
		});
		^super.new.key_(key);
	}
	storeArgs {
		^[key];
	}
	embedInStream {
		| inval |
		loop {
			var cur = all[key];
			all[key] = all[key] + 1;
			cur.yield;
		}
		^inval;
	}
}

/*
	IDEA: conditional Pchain-style filter
	* use a key from the chained pattern to turn on or off the filter
	* can arpeggiate with this (somehow?)
	TODO:
	* make the filters/keys pairs (Synth.new-style)
	* make it possible for the filters to be patterns
*/

Pq : Pattern { // "Pattern query"
	var <>base, <>filter, <>keys;
	*new {
		| base filter keys | // supports multiple filters and keys when those args are provided as arrays
		if(filter.class != Array, {
			filter = [filter];
		});
		if(keys.class != Array, {
			keys = [keys];
		});
		^super.newCopyArgs(base, filter, keys);
	}
	embedInStream {
		| ev |
		var stream = base.asStream;
		while({ (ev = stream.next(ev)).notNil }, {
			var sub;
			var ss = Psync(
				Pchain(*(
					keys.select({
						| key |
						ev[key].asBoolean;
					}).collect({
						| key |
						filter[keys.indexOf(key)];
					}) ++
					[ev]
				)), ev[\dur], ev[\dur]).asStream;
			while({ (sub = ss.next(ev)).notNil }, {
				sub.yield;
			});
			// var new = ev.deepCopy;
			// keys.do({
			// 	| key i |
			// 	if(new[key].asBoolean, {
			// 		case(
			// 			{ filter[i].class == Event }, {
			// 				new = new ++ filter[i];
			// 			},
			// 			{ filter[i].isKindOf(Pattern) }, {
			// 			},
			// 		);
			// 	});
			// });
			// new.yield;
		});
	}
}

/*
	IDEA: Pbind replacement - "Pb"
    * automatically provides \tempo
    * automatically provides \beat (the TempoClock's beat number)
	* automatically provides \pbeat (to the pattern, so that it can be read via Pkey, etc)
    * Pkey can access last value, even between events (maybe?)
    * automatically have Pdef's "global namespace" (no)
	* provide a Synth or Ndef instance as the argument to \set and Pb will set that synth/node's values.
*/

Pb : Pattern {
	*new {
		| ... pbind |
		var pc, pbe = pbind.asEvent;
		if(pbe[\set].notNil, {
			var idx = pbind.indexOf(\set);
			pbind = pbind ++ [
				\id, Pfunc({
					| e |
					e[\set].asNodeID;
				}),
				\args, Pfunc({
					| e |
					e[\set].controls.collect(_.name).sect(pbe.keys);
				}),
				// \instrument, Pfunc({
				// 	| e |
				// 	e[\set].defName;
				// }),
				\type, \set,
			];
		});
		pbind = [
			\tempo, Pfunc({thisThread.clock.tempo}),
			\tbeat, Pfunc({thisThread.clock.beats}),
			\beat, Pkey(\tbeat),
			\pbeat, Prout({
				var start = thisThread.clock.beats;
				loop {
					(thisThread.clock.beats-start).yield;
				};
			}),
			\event, Pseries(0, 1, inf),
		] ++ pbind;
		pc = Pbind(*pbind);
		if(pbe[\psync].notNil, {
			var len = pbe[\psync];
			pc = Psync(pc, len, len);
		});
		if(pbe[\pr].notNil, {
			pc = Pn(pc, pbe[\pr]);
		});
		^pc;
	}
}

// Pdefng : Pattern {
// }

Pd : Pattern {
	classvar <>all;
	*initClass {
		all = ();
	}
	*new {
		| name ... pbind |
		if(all[name].isNil, {
			all[name] = 0;
		});
		^Pdef(name, Pb(*[
			\pdef, name,
			\stream, Pr(
				Pfunc({
					| e |
					var new = Pd.all[e[\pdef]] + 1;
					Pd.all[e[\pdef]] = new;
					new;
				})
			)
		]
			++ (pbind)));
	}
}

Psyncer : Pattern { // provide patterns in order from longest to shortest, and they will be "synced" to a loop defined by the longest. - FIX
	var <>patterns;
	*new {
		| ... patterns |
		^Ppar([patterns[0]] ++ patterns[1..].collect(Pn(_)));
		// ^super.new.patterns_(patterns);
	}
}

Pfwd : Pattern { // sends copies of the events from 'pattern' to 'send'.value
	var <>pattern, <send, <>name;
	*new {
		| pattern send name |
		^super.new.pattern_(pattern).send_(send).name_(name??{if(pattern.isKindOf(Pdef), {pattern.key});});
	}
	send_ {
		| argsend |
		if(argsend.isKindOf(Pmod), {
			send = Message(argsend, \fwd);
		}, {
			send = argsend;
		});
	}
	embedInStream {
		| inval |
		var cevent, levent;
		var pstream = pattern.asStream;
		loop {
			if((cevent = pstream.next(inval)).isNil, {
				send.value(nil, name, thisThread.clock.beats);
				^nil;
			}, {
				send.value(cevent, name, thisThread.clock.beats);
				inval = cevent.yield;
			});
		}
	}
	storeArgs { ^[pattern, send, name]; }
}

Pmod : Pattern { // play a modified copy of a stream in parallel - TODO: do this better.
	var <>func, <>binds, <queue;
	*new {
		| ... args |
		if(args.size.odd, {
			^super.new.func_(args[0]).binds_(args[1..]).init;
		}, {
			^super.new.func_(nil).binds_(args).init;
		});
	}
	init {
		queue = [];
	}
	storeArgs {
		^([func]++binds).flat;
	}
	embedInStream { arg event;
		// var inevent, nn;
		// var ptn = if(pattern.isKindOf(Symbol), Pdef(pattern), pattern);
		// var stream = ptn.asStream;
		// while {
		// 	(inevent = stream.next(event)).notNil
		// } {
		// 	var dur = inevent[\dur];
		// 	var child = Psync(arpeggiator, dur, dur).asStream;
		// 	var childevent;
		// 	while {
		// 		(childevent = child.next(inevent)).notNil
		// 	} {
		// 		childevent.yield;
		// 	}
		// }
		^event;
	}
	fwd {
		| event name beat |
		queue = queue ++ [event];
	}
}

PMouseX : Pattern { // TODO
}

PMouseY : Pattern { // TODO
}

PSinOsc : Pattern { // STOLEN FROM THE UGENPATTERNS QUARK (FIX)
	var <>freq, <>phase, <>mul, <>add, <>length;
	*new {|freq= 440, phase= 0, mul= 1, add= 0, length= inf|
		^super.newCopyArgs(freq, phase, mul, add, length);
	}
	storeArgs {^[freq, phase, mul, add, length]}
	embedInStream {|inval|
		var freqStr= freq.asStream;
		var phaseStr= phase.asStream;
		var mulStr= mul.asStream;
		var addStr= add.asStream;
		var freqVal, phaseVal, mulVal, addVal;
		var theta= 0;
		length.value(inval).do{
			addVal= addStr.next(inval);
			mulVal= mulStr.next(inval);
			phaseVal= phaseStr.next(inval);
			freqVal= freqStr.next(inval);
			if(addVal.isNil or:{mulVal.isNil or:{phaseVal.isNil or:{freqVal.isNil}}}, {^inval});
			inval= (sin(theta/freqVal*2pi+phaseVal)*mulVal+addVal).yield;
			theta= theta+1;
		};
		^inval;
	}
}

// Pfork : Pattern { // launch parallel patterns based on events from the parent stream. use pattern.fork in a Pfunc instead.
// }

PfxbTW : Ppar { // from http://new-supercollider-mailing-lists-forums-use-these.2681727.n2.nabble.com/Node-failures-in-Pfxb-example-code-td7597669.html

     *new {
		 | pattern effect effectArgs server output=0 |

         var serverToUse = server ?? Server.default;
         var bus = Bus.audio(serverToUse, 2);
         var effectSynth;

         ^super.new([
             Pfunc({ effectSynth = Synth(
                 effect,
                 effectArgs ++ [\in, bus.index, \out, output],
                 serverToUse,
                 \addToTail
             ); nil }, 1),
             Pseq([
                 Pbindf(pattern, \server, serverToUse, \out, bus.index),
                 Pfunc({ effectSynth.release; nil }, 1)
             ], 1)
         ])
     }
}

+ Pattern {
	fwd {
		| send name |
		^Pfwd(this, send, name);
	}
}

+ Symbol { // for Pg
	pgu {
		var cur;
		if(Pg.all[this].isNil, {
			Pg.all[this] = 0;
		});
		cur = Pg.all[this];
		Pg.all[this] = cur + 1;
		^cur;
	}
	pg {
		if(Pg.all[this].isNil, {
			Pg.all[this] = 0;
		});
		^Pg.all[this];
	}
}

