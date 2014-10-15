// Keymap.sc
// class file for Emacs-style keybindings.

Keymap {
	var <bindings;
	var <>context;
	classvar keycodeMap, shiftedKeycodeMap;
	*initClass {
		keycodeMap = [
			nil, nil, nil, nil, nil, nil, nil, nil, nil, "esc",
			"1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
			"-", "=", "backspace", "tab", "q", "w", "e", "r", "t", "y",
			"u", "i", "o", "p", "[", "]", "enter", "lctrl", "a", "s",
			"d", "f", "g", "h", "j", "k", "l", ";", "'", "`",
			"lshift", "\\", "z", "x", "c", "v", "b", "n", "m", ",",
			".", "/", "rshift", "n*", "lalt", "space", "capslock", "f1", "f2", "f3",
			"f4", "f5", "f6", "f7", "f8", "f9", "f10", "numlock", nil, "n7",
			"n8", "n9", "n-", "n4", "n5", "n6", "n+", "n1", "n2", "n3",
			"n0", "n.", nil, nil, nil, "f11", "f12", nil, nil, nil,
			nil, nil, nil, nil, "nenter", "rctrl", "n/", nil, "ralt", nil,
			"home", "up", "pageup", "left", "right", "end", "down", "pagedown", nil, "delete",
			nil, nil, nil, nil, nil, nil, nil, "pause", nil, nil,
			nil, nil, nil, "win", nil, "menu", nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, "(", ")", nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil
		];
		shiftedKeycodeMap = [
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			"!", "@", "#", $$.asString, "%", "^", "&", "*", "(", ")",
			"_", "+", nil, nil, "Q", "W", "E", "R", "T", "Y",
			"U", "I", "O", "P", "{", "}", nil, nil, "A", "S",
			"D", "F", "G", "H", "J", "K", "L", ":", "\"", "~",
			nil, "|", "Z", "X", "C", "V", "B", "N", "M", "<",
			">", "?", nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
			nil, nil, nil, nil, nil
		];
	}
	*new {
		| binds=nil |
		^super.new.init(binds);
	}
	*keycode {
		| keycode |
		^keycodeMap[keycode];
	}
	*shiftedKeycode {
		| keycode |
		^shiftedKeycodeMap[keycode];
	}
	*stringifyKey {
		| modifiers keycode |
		var output, shifted;
		if(keycode.isNil, {
			output = "";
		}, {
			shifted = Keymap.shiftedKeycode(keycode);
			output = if(modifiers.isShift, {
				if(shifted.notNil, {
					shifted;
				}, {
					"S-"++Keymap.keycode(keycode);
				});
			}, {
				Keymap.keycode(keycode);
			});
		});
		if(output.isNil, {
			^false; // unknown key...
		});
		if(["rctrl", "lctrl", "lalt", "ralt", "lshift", "rshift"].select(_==output).size > 0, {
			^nil;
		}, {
			output = if(modifiers.isAlt, "M-", "") ++ output;
			output = if(modifiers.isCtrl, "C-", "") ++ output;
			^output;
		});
	}
	init {
		| binds |
		bindings = (keymap: true);
		context = "";
		if(binds.notNil, {
			this.binds(binds);
		});
	}
	bind {
		| key value |
		if(key.isKindOf(Array), {
			key.do({
				| k |
				this.bind(k, value);
			});
		}, {
			var insplit = key.asString.split($ );
			var current = value;
			insplit.reverse.do {
				| ckey |
				current = Event[ckey.asSymbol -> current, \keymap -> true];
			};
			bindings = bindings.recursivelyMerge(current);
		});
	}
	binds {
		| event |
		event.keys.do {
			| key |
			this.bind(key, event[key]);
		};
	}
	merge {
		| keymap |
		bindings = bindings.recursivelyMerge(keymap.bindings);
	}
	returnMerge { // like merge, but don't change the keymap's bindings, just return the merged map.
		| keymap |
		^bindings.recursivelyMerge(keymap.bindings);
	}
	at {
		| index |
		if(index.asString.includes($ ), {
			var split = index.asString.split($ );
			var current = bindings;
			split.do {
				| ckey |
				current = current[ckey.asSymbol];
			};
			^current;
		}, {
			^bindings[index.asSymbol];
		});
	}
	keyDown {
		| key | // should be a stringified Key via Keymap.stringifyKey
		var result = this[context ++ key.asString];
		if(result.isKeymap, {
			context = context ++ key.asString ++ " ";
		}, {
			context = "";
		});
		^result;
	}
	keyDownOld {
		| modifiers keycode |
		var keystring = Keymap.stringifyKey(modifiers, keycode);
		if(keystring.notNil, {
			var result = this[context ++ keystring];
			if(result.isKeymap, {
				context = context ++ keystring ++ " ";
			}, {
				context = "";
			});
			^result;
		});
	}
}

