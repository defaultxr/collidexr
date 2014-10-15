Effects {
	var <>group, <>bus, <>out, <>bp, <>list;
	*new {
		| ... fx |
		^super.new.init(*fx);
	}
	init {
		| ... fx |
		this.group_(Group.new);
		this.bus_(Bus.audio(Server.default, 2));
		this.list_([]);
		this.list_(this.list ++ [Synth.tail(this.group, \busPlayer, [\in, this.bus, \out, this.out])]);
		fx.do {
			| effect |
			this.add(effect);
		};
		this.bp_(this.list.wrapAt(-1));
		this.out_(0);
	}
	add {
		| fx pos=(-1) |
		var synth;
		if(fx.class == Symbol, {
			fx = [fx];
		});
		case(
			{ pos == 0 }, {
				synth = Synth.head(this.group, fx[0], fx[1..] ++ [\in, this.bus, \out, this.bus]);
			},
			{ pos == (-1) }, {
				synth = Synth.before(this.at(\busPlayer), fx[0], fx[1..] ++ [\in, this.bus, \out, this.bus]);
			},
			true, {
				synth = Synth.after(this.list.wrapAt((pos-1)), fx[0], fx[1..] ++ [\in, this.bus, \out, this.bus]);
			},
		);
		if(pos.isNegative, {
			pos = this.list.size+pos;
		});
		this.list_(this.list.insert(pos, synth));
	}
	remove {
		| pos=0 |
		var io = this.indexOf(pos);
		this.at(io).release;
		this.list[io].release;
		this.list.removeAt(io);
	}
	at {
		| index |
		if(index.isNil, {
			^nil;
		});
		if(index.isKindOf(Number), {
			^this.list[index];
		}, {
			^this.at(this.indexOf(index));
		});
	}
	put {
		| index effect |
		this.remove(index);
		this.add(effect, index);
	}
	indexOf {
		| item |
		if(item.isNil, {
			^nil;
		});
		if(item.isKindOf(Number), {
			^item;
		});
		^this.list.collect({
			| synth |
			synth.defName.asSymbol;
		}).indexOf(item);
	}
	busIndex {
		^this.bus.index;
	}
}