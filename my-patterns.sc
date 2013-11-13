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
			if((nn = nstream.next(event)).notNil) {
				nn.abs.do {
					event = inevent.copy.yield;
				};
			} { ^event };
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

// PfxbTW2 : Ppar { // make there some way to control the effect synth with patterns
//      *new { |pattern, effect, effectArgs, server, output=0| 

//          var serverToUse = server ?? Server.default; 
//          var bus = Bus.audio(serverToUse, 2); 
//          var effectSynth; 

//          ^super.new([ 
//              Prout({ effectSynth = Synth(
//                  effect, 
//                  effectArgs.collect({
// 					 | argu n |
// 					 if(n.isEven, argu, {
						 
// 					 });
// 				 }) ++ [\in, bus.index, \out, output], 
//                  serverToUse, 
//                  \addToTail 
//              ); nil }, 1), 
//              Pseq([ 
//                  Pbindf(pattern, \server, serverToUse, \out, bus.index), 
//                  Pfunc({ effectSynth.release; nil }, 1) 
//              ], 1) 
//          ]) 
//      } 

// }