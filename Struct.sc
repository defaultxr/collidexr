// Struct - to aid in song creation
/*
	* allows you to group resources (into a "Struct" which could be thought of as a song or a performance)
	* automatically takes care of sorting out these resources & providing a fast & easy way to generate a control surface for them
	* allows a timeline to be written out of "events" (not SC's Event type) and sequenced
	* patterns can be played together - like with Psym - or triggered to just fork
*/

/*
to track a pattern:
Pchain(
    Pbind(
        \__none__, Prout({
            | event |
            var start = thisThread.clock.beats;
            var num = 0;
            loop {
                ~fff.(num, (thisThread.clock.beats-start), event);
                num = num + 1;
                1.yield;
            }
        }),
    ),
    pattern, // THIS PATTERN
);

OR:

Pfwd(pattern, function_to_send_event_to); // see my-patterns.sc for Pfwd

~fff should be a function that gets args: event_number, patterns_beat_number, event
*/

// Struct {
// 	var <>key, <>resources;
// 	// var <>synths, <>ctls, <>patterns;
// 	var <>sstate, <>cstate, <>pstate;
// 	*initClass {
// 	}
// 	*new {
// 		| resources load |
// 		^super.new.init(resources, load);
// 	}
// 	init {
// 		| res load |
// 		key = 'test';
// 		resources = ();
// 		sstate = ();
// 		cstate = ();
// 		pstate = [];
// 		res.do({
// 			| thing |
// 			this.add(thing);
// 		});
// 		load.value(this);
// 	}
// 	add { // add a resource to the Struct.
// 		| thing options | // 'thing' is the thing to add. 'options' should be metadata about the thing.
// 		var evt, name, type;
// 		evt = (meta: options, obj: thing);
// 		case(
// 			// { thing.isKindOf(Event) }, {
// 			// 	name = thing[\name];
// 			// 	obj = thing;
// 			// },
// 			{ thing.isKindOf(Pdef) }, {
// 				name = thing.key;
// 				type = \pattern;
// 			},
// 			{ thing.isKindOf(Ctl) }, {
// 				name = thing.key;
// 				type = \ctl;
// 			},
// 			{ thing.isKindOf(SynthDesc) }, {
// 				name = thing.name;
// 				type = \synth;
// 			},
// 		);
// 		name = name.asSymbol;
// 		evt = evt ++ (name: name, type: type);
// 		if(resources.includesKey(name), {
// 			"Warning: Struct %: Updating resource '%'.".format(this.key, name).warn;
// 		});
// 		resources = resources ++ Event[name -> evt];
// 	}
// 	remove {
// 		| name |
// 		resources[name] = nil;
// 	}
// 	commandPattern { // see http://en.wikipedia.org/wiki/Command_pattern
// 		| resource | // resource name
		
// 	}
// 	run { // 'run' one of the resources.
// 		| name params |
// 		var evt = resources[name];
// 		var res_evt = ();
// 		case(
// 			{ evt[\type] == \synth }, {
// 				if(params.class != Event, {
// 					"Params should be an Event currently!".postln; // FIX
// 					// params = params.getPairs;
// 				});
// 				case(
// 					{
// 						evt[\meta].isNil or:
// 						{ evt[\meta].includesKey(\mode).isNil } or:
// 						{ evt[\meta][\mode] == \poly }
// 					}, {
// 						var synth = Synth(evt[\name], if(params.class == Event, { params.getPairs }, { params }));
// 						res_evt = (
// 							synth: synth,
// 							params: params,
// 							start: TempoClock.default.beats,
// 						);
// 						if(sstate[name].isNil, {
// 							sstate[name] = [res_evt];
// 						}, {
// 							sstate[name] = sstate[name] ++ [res_evt];
// 						});
// 					},
// 					{ evt[\meta][\mode] == \mono }, {
// 						"STRUCT'S MONO MODE IS NOT WORKING YET".postln; // FIX
// 					},
// 				);
// 			},
// 			{ evt[\type] == \pattern }, {
// 				if(pstate.includes(name).not, {
// 					pstate = pstate ++ [name];
// 				});
// 				this.play;
// 			},
// 			{ evt[\type] == \ctl }, {
// 			}
// 		);
// 	}
// 	set {
// 	}
// 	halt { // 'halt' one of the resources.
// 		| name params |
// 		var evt = resources[name];
// 		var res_evt = ();
// 		case(
// 			{ evt[\type] == \synth }, {
// 				if(params.isNil, {
// 					sstate[name].do({
// 						| thing |
// 						thing[\synth].release;
// 					});
// 					sstate[name] = [];
// 				}, {
// 					var nums = this.matchSState(name, params);
// 					nums.do({
// 						| n |
// 						sstate[name][n][\synth].release;
// 						sstate[name][n] = nil;
// 					});
// 					sstate[name] = sstate[name].reject(_.isNil);
// 				});
// 			},
// 			{ evt[\type] == \pattern }, {
// 				pstate.remove(name);
// 			},
// 			{ evt[\type] == \ctl }, {
// 			},
// 		);
// 	}
// 	synths {
// 		^resources.select({
// 			| res |
// 			res[\type] == \synth;
// 		});
// 	}
// 	ctls {
// 		^resources.select({
// 			| res |
// 			res[\type] == \ctl;
// 		});
// 	}
// 	patterns {
// 		^resources.select({
// 			| res |
// 			res[\type] == \pattern;
// 		});
// 	}
// 	matchSState {
// 		| name params |
// 		^sstate[name].collect({
// 			| thing n |
// 			var res = params.keys.collect({
// 				| par |
// 				thing[\params][par] == params[par];
// 			});
// 			if(res.includes(false).not, {
// 				n
// 			}, nil);
// 		}).reject(_.isNil);
// 	}
// 	callback {
// 		| name data |
		
// 	}
// 	play {
// 		Pdef(this.key, Psym(
// 			Pfunc({
// 				var st = this.pstate;
// 				if(st == [], {
// 					[\ ];
// 				}, {
// 					st;
// 				});
// 			})
// 		)).play;
// 	}
// 	stop {
// 		Pdef(this.key).stop;
// 	}
// 	at {
// 		| index |
// 	}
// 	drawResource { // draw a resource. to be called from the view's drawFunc
// 	}
// 	view {
// 		| parent |
// 		var view, st;
// 		view = UserView(parent, parent.view.bounds).resize_(5);
// 		st = StaticText(parent, parent.view.bounds).resize_(5).background_(Color.black).stringColor_(Color.white);
// 		view.keyDownAction_({
// 			| view char modifiers unicode keycode |
// 			st.string_("Synths: %\n\nCtls: %\n\nPatterns: %\n\npstate: %".format(this.synths, this.ctls, this.patterns, this.pstate));
// 		});
// 		^view;
// 	}
// }