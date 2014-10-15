+ Event {
	recursivelyMerge {
		| dictB |
		^this.merge(dictB, {
			| a b |
			if(a.class == Event && b.class == Event, {
				a.recursivelyMerge(b);
			}, {
				b
			});
		});
	}
	isKeymap {
		if(this[\keymap] == true, { ^true; }, { ^false });
	}
}

+ Object {
	isKeymap {
		^false;
	}
}

