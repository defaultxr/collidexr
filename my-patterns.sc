Prun : Pclutch { // pattern is "run"; the values are latched for as long as the duration of the original pattern says.
	/* FIX: the code below doesn't repeat the last event like it should, because of the \delta event that Psync gives the last event.
		Pchain(
		Pbind(\dur, 1/4),
		Prun(
		Pb(
		\instrument, Pseq([\kik, \SOSsnare], inf),
		\dur, 1,
		\psync, 4,
		)
		),
		).trace.play
	*/
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
		^inval;
	}
}

// new version (does not work yet):
// Prun : Pattern { // pattern is "run"; the values are latched for as long as the duration of the original pattern says.
// 	var <>pattern;
// 	*new {
// 		| pattern |
// 		^super.new(pattern);
// 	}
// 	storeArgs { ^[pattern]; }
// 	embedInStream { arg inval;
// 		var stream = pattern.asStream;
// 		var inevent, heldsince;
// 		while({
// 			(inevent = stream.next(inval)).notNil;
// 		}, {
// 			heldsince = thisThread.clock.beats;
// 			while({
// 				(thisThread.clock.beats - heldsince) <= inevent[\dur];
// 			}, {
// 				inevent.yield;
// 			});
// 		});
// 		^inval;
// 	}
// }

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
			if((nn = nstream.next(event++(pr:inevent)).value(event++(pr:inevent))).notNil, {
				nn.abs.do {
					event = inevent.copy.yield;
				};
			}, {
				^event
			});
		};
		^event;
	}
	estimateLength {
		case(
			{ this.repeats.isKindOf(Pattern) }, {
				var rl = this.repeats.estimateLength;
				var pl = this.pattern.estimateLength;
				case(
					{ rl == inf and: { pl == inf } }, {
						^inf;
					},
					{ true }, {
						^this.repeats.asStream.nextN([pl,rl].reduce(\min)).sum;
					},
				);
			},
			{ this.repeats.isKindOf(Number) }, {
				^(this.pattern.estimateLength * this.repeats);
			},
		);
		^(this.pattern.estimateLength)
	}
}

PnC : FilterPattern { // conditional looping (like Pn with a condition instead of number of repeats)
	var <>condition, <>key;
	*new { arg pattern, condition={true}, key;
		^super.newCopyArgs(pattern, condition, key)
	}
	storeArgs { ^[pattern, condition, key] }
	embedInStream {
		| event |
		if(key.isNil, {
			while({condition.value(event)}, {
				event = pattern.embedInStream(event);
			});
		}, {
			while({condition.value(event)}, {
				event = pattern.embedInStream(event);
				event[key] = true;
			});
			event[key] = false;
		});
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
	estimateDur {
		^this.pattern.estimateDur;
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
    * automatically provides \tbeat (the TempoClock's beat number)
	* automatically provides \pbeat (to the pattern, so that it can be read via Pkey, etc) (aliased to \beat because Sequence uses it for the same purpose)
    * Pkey can access last value, even between events (maybe?)
    * automatically have Pdef's "global namespace" (no)
	* provide a Synth or Ndef instance as the argument to \set and Pb will set that synth/node's values.
*/

Pb : Pattern {
	*new {
		| ... pbind |
		var pbe = pbind.asEvent;
		if(pbe[\instrument].isKindOf(Ndef), {
			pbind = pbind ++ [
				\id, Pfunc({
					| e |
					e[\instrument].asNodeID;
				}),
				\args, Pfunc({
					| e |
					e[\instrument].controls.collect(_.name).sect(pbe.keys);
				}),
				\type, \set,
			];
		});
		if(pbe[\bufnum].isKindOf(Bdef), {
			pbind[pbind.indexOf(\bufnum)+1] = pbe[\bufnum].buffer;
		});
		pbind = Pb.prConvAry(pbind);
		pbind = Pbind(*pbind);
		pbind = Pb.prContainers(pbind);
		^pbind;
	}
	*prConvAry {
		| array |
		^([
			\tempo, Pfunc({thisThread.clock.tempo}),
			\tbeat, Pfunc({thisThread.clock.beats}),
			\pbeat, Prout({
				var start = thisThread.clock.beats;
				loop {
					(thisThread.clock.beats-start).yield;
				};
			}),
			\beat, Pkey(\pbeat),
			\event, Pseries(0, 1, inf),
		] ++ array);
	}
	*prContainers {
		| pbind |
		var pbe = pbind.patternpairs.asEvent;
		if(pbe[\psync].notNil, {
			var len = pbe[\psync];
			pbind = Psync(pbind, len, len);
		});
		if(pbe[\pn].notNil, {
			pbind = Pn(pbind, pbe[\pn]);
		});
		if(pbe[\pfin].notNil, {
			pbind = Pfin(pbe[\pfin], pbind);
		});
		if(pbe[\pfindur].notNil, {
			pbind = Pfindur(pbe[\pfindur], pbind);
		});
		if(pbe[\pds].notNil, {
			pbind = PdurStutter(pbe[\pds], pbind);
		});
		// FIX: if pattern's first result is Nil, replace it with (type:\rest,dur:1)
		// if(pbe[\nilsafe].notNil, {
		// 	pbind = 
		// });
		^pbind;
	}
}

Pm : Pattern {
	*new {
		| name ... pbind |
		pbind = Pb.prConvAry(pbind);
		pbind = PmonoArtic(name, *pbind);
		pbind = Pb.prContainers(pbind);
		^pbind;
	}
}

// Pdefng : Pattern {
// }

Pd : Pattern {
	classvar <>all, <>send; // send is a func to evaluate for each event.
	*initClass {
		all = ();
	}
	*new {
		| name pattern |
		var pat = pattern;
		var sendfunc = {
			| event |
			if(Pd.send.notNil, {
				Pd.send.value(event, name, thisThread.clock.beats);
			});
			1;
		};
		block {
			| break |
			var pairs = [
				\pdef, name,
				\embed, Pr(
					Pfunc({
						| e |
						var old = Pd.all[name];
						var new = Pd.all[name] + 1;
						Pd.all[name] = new;
						old;
					})
				)
			];
			5.do({
				case(
					{ pat.respondsTo(\patternpairs) }, {
						pat.patternpairs_(pairs ++ pat.patternpairs ++ [\__fwd, Pfunc(sendfunc)]);
						break.value;
					},
					{ pat.respondsTo(\patterns) }, {
						pat.patterns_([Pbind(*(pairs++[\__fwd, Pfunc(sendfunc)]))] ++ pat.patterns);
						break.value;
					}
				);
				pat = if(pat.respondsTo(\pattern), {
					pat.pattern;
				});
			});
		};
		if(all[name].isNil, {
			all[name] = 0;
		});
		^Pdef(name, pattern);
	}
}

Psyncer : Pattern { // provide a list of patterns, and they will all be synced to loop to the first.
	// FIX: maybe make it so that if a pattern is still running, it just continues instead of being psync'd... (prolly not possible)
	// FIX: should act more like Psym, i.e. take an actual list and also accept patterns that generate lists...
	var <>patterns;
	*new {
		| patterns |
		var len;
		patterns = patterns.collect({
			| p |
			if(p.isKindOf(Symbol), {
				Pdef(p);
			}, p);
		});
		len = patterns[0].estimateDur;
		^Ppar(
			[patterns[0]] ++
			patterns[1..].collect({
				| p |
				Psync(Pn(p), len, len);
			})
		);
	}
}

Pcomp : Pattern {
	// Pattern composition
	// tracker-inspired pattern class for forking/playing/syncing other patterns/ndefs/synths/etc.
	// supposed to be for macro-level control
	// Pcomp(pattern, dict);
	// 'pattern' is a pattern that outputs any of the following kinds of items:
	// * [pat, pat, pat, ...] // in which case the length of the first 'pat' is used as dur, others are forked
	// * (dur: dur, type: pat, ...)
	// * [type, pat, type, pat, ...]
	// * \symbol
	// ...where 'dur' is the length of the event (beats until the next one is played)
	// ...where 'type' is one of:
	// * \fork - fork a pattern (or ndef/synth)
	// * \embed - embed a pattern - longest one determines dur of current Pcomp step if dur is not provided manually
	// * \play - play a pdef (or anything) - will not re-trigger if it's already playing (unlike fork)
	// * \loop - loop a pdef during the length of the current Pcomp event.
	// * \dur - specify duration (in beats) of current Pcomp step
	// * \stop - turn off an ndef (or stop a synth, playing pdef, etc.)
	// * \set - set parameters for an ndef or synth, or set a ctl
	// * \end - end the song... (FIX)
	// * you can also play a function or fork a routine (or tdef)
	// ...where 'pat' is a pdef, ndef, tdef, pattern, function, routine, synth, etc...
	// 'dict' is an optional dictionary specifying the definitions of any 'symbol' that the 'pattern' outputs, i.e. so you don't have to re-write them every time...
	// FIX: add cleanup function - Ndefs triggered don't stop when stop is called on the Pcomp.
	var <>pattern, <>dict, <>loop=true, <>trace=false;
	*new {
		| pattern dict loop=true trace=false |
		^super.new().pattern_(pattern).dict_(dict).loop_(loop).trace_(trace);
	}
	*defLookUp {
		| key |
		^case(
			{ Pdef.all.keys.includes(key) }, {
				Pdef(key);
			},
			{ Ndef.all.keys.includes(key) }, {
				Ndef(key);
			},
			{ Tdef.all.keys.includes(key) }, {
				Tdef(key);
			},
		);
	}
	lookUp {
		| key |
		^if(dict.notNil, {
			dict[key];
		}, {
			(embed:Pcomp.defLookUp(key));
		});
	}
	storeArgs { ^[pattern, dict] }
	embedInStream {
		| inval |
		var str, outval, ev, clooping;
		str = pattern.asStream;
		while({
			var continue;
			outval = str.next(inval);
			continue = if(outval.isNil, {
				if(loop, {
					str = pattern.asStream;
					outval = str.next(inval);
					true;
				}, {
					false;
				});
			}, true);
			if(trace, {
				outval.postln;
			});
			continue;
		}, {
			var embed = [], loops = [], dur, parser, repeat = 1;
			case(
				{ outval.isKindOf(Symbol) }, {
					outval = this.lookUp(outval);
				},
				{ outval.isSequenceableCollection }, {
					outval = outval.collect({
						| o |
						this.lookUp(o);
					}).reduce(\combineCommonIntoArrays);
					if(outval[\dur].notNil and: { outval[\dur].isSequenceableCollection }, {
						outval[\dur] = outval[\dur].reduce(\max);
					});
				},
			);
			parser = {
				| key val |
				if(val.isSequenceableCollection, {
					val.do({
						| v |
						parser.(key, v);
					});
				}, {
					if(val.isKindOf(Symbol), {
						val = Pcomp.defLookUp(val);
					});
					switch(key,
						\dur, {
							dur = val;
						},
						\embed, {
							if(val.isKindOf(Pattern), {
								embed = embed ++ [val];
							});
						},
						\play, {
							case(
								{ val.isKindOf(Function) }, {
									val.value(inval);
								},
								{ val.isKindOf(Pdef) }, {
									val.play(quant: 1, protoEvent: inval);
								},
								{ val.isKindOf(Tdef) }, {
									val.play(quant: 1);
								},
								true, { // catches Ndef and everything else
									val.play;
								},
							);
						},
						\loop, {
							loops = loops ++ [val];
						},
						\fork, {
							case(
								{ val.isKindOf(Function) }, {
									val.value(inval);
								},
								{ val.isKindOf(Ndef) }, {
									val.play;
								},
								{ val.isKindOf(Pdef) }, {
									val.fork(thisThread.clock, protoEvent:inval);
								},
								{ val.isKindOf(SynthDef) }, {
									// FIX - not implemented.
								},
							);
						},
						\stop, {
							case(
								{ val.isKindOf(Ndef) }, {
									val.stop;
								},
								{ val.isKindOf(Pdef) }, {
									val.stop;
								},
								{ val.isKindOf(SynthDef) }, {
									// FIX - not implemented.
								},
							);
						},
						\set, {
							// not sure what the syntax should be for this.
						},
						\repeat, {
							repeat = val.value;
						},
					);
				});
			};
			outval.keysValuesDo({
				| key val |
				parser.(key, val);
			});
			if(embed.size == 0, {
				embed = [Pdef.default]; // maybe use a 0-length pattern instead...
			}, {
				embed = embed.collect({
					| item |
					if(item.isKindOf(Symbol), {
						Pdef(item);
					}, {
						item;
					});
				});
			});
			embed = Ppar(embed, 1);
			if(dur.notNil, {
				embed = Psync(embed, dur, dur);
			});
			// PSYNC VERSION
			loops.do({
				| l |
				var embedbeats = embed.estimateDur; // FIX: do this without estimateDur.
				Psync(Pn(l), embedbeats, embedbeats).play;
			});
			inval = embed.embedInStream(inval);
			// NON-PSYNC VERSION (doesn't require estimateDur)
			// ~clooping = [];
			// inval = Pseq([
			// 	Pfunc({
			// 		~clooping = [];
			// 		loops.do({
			// 			| l |
			// 			~clooping = ~clooping ++ [Pn(l).play];
			// 		});
			// 		nil;
			// 	}),
			// 	embed,
			// 	Pfunc({
			// 		~clooping.do({ // also part of the NON-PSYNC VERSION
			// 			| l |
			// 			l.stop;
			// 		});
			// 		nil;
			// 	}),
			// ]).embedInStream(inval);
		});
		^inval;
	}
	getPattern {
		| key |
		^if(key.isKindOf(Pattern), {
			key;
		}, {
			if(key.isSequenceableCollection, {
				this.lookupClass.parallelise(
					key.collect({
						| each |
						this.lookUp(each.asSymbol);
					});
				);
			}, {
				this.lookUp(key.asSymbol);
			});
		});
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

// PMouseX : Pattern { // TODO
// }

// PMouseY : Pattern { // TODO
// }

PkEnv : Pattern {
	var <>levels, <>times, <>curve=\lin, <>releaseNode, <>loopNode, <>offset=0;
	*initClass {
		
	}
	*new {
		
	}
}

Pstut : Pstutter {
	// like PdurStutter but works for float AND event streams.
	embedInStream { arg event;
		var dur, stut, evt;
		var pats = pattern.asStream;
		var stutts = n.asStream;
		while({
			evt = pats.next(event);
			if(evt.isKindOf(Event), {
				dur = evt.use({
					\dur.envirGet.value;
				});
			}, {
				dur = evt;
			});
			evt.notNil and: { (stut = stutts.next(event)).notNil };
		},{
			if(stut > 0, { // 0 skips it
				if(stut > 1, {
					if(evt.isKindOf(Event), {
						evt = evt ++ (dur: dur / stut);
					}, {
						evt = evt / stut;
					});
					stut.do({
						event = evt.yield;
					});
				},{
					event = evt.yield
				});
			});
		})
		^event;
	}
}

/*
	Pfeed(start, func, bounds, once:false)
	start = starting value (can also be a function)
	func = function to evaluate for each step. first step is passed the starting value, and then subsequent steps are passed the function's result from the previous step.
	bounds = if func output is outside of this range ([bottom, top]) then act as if func returned start instead.
	once = if true, pattern yields nil instead of start if func's return value is out of bounds.
*/
Pfeed : Pattern {
	var <>start;
	var <>func;
	var <>bounds;
	var <>once;
	*new { arg start, func, bounds, once=false;
		^super.newCopyArgs(start, func, bounds, once);
	}
	storeArgs { ^[start, func, bounds, once] }
	embedInStream {
		| event |
		var rpt = {
			var val = start.value(event);
			val.yield;
			val = func.value(val, event);
			val.yield;
			while({ bounds.isNil or: { bounds.reduce(\min) <= val and: { bounds.reduce(\max) >= val } } }, {
				val = func.value(val, event);
				val.yield;
			});
		};
		rpt.();
		while({ once }, rpt);
		^event;
	}
}

+ Event {
	fuse {
		| operation='mean' mix=0.5 ... events |
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
	pd {
		^Pd(this);
	}
}

+ Pdef {
	embedReset {
		/* reset the 'embed' counter for this Pdef. */
		Pd.all[this.key] = 0;
	}
}