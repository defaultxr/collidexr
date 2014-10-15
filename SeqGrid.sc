SeqGrid : SCViewHolder {
	var <data, <bounds, <cstep, <offset, <>changeFunc; // data should be an array of Sequences.

	*new {
		| parent bounds data |
        ^super.new.init(parent, bounds, data);
    }

    init {
		| parent argbounds argdata |
		bounds = argbounds ?? {Rect(0, 0, 500, 500)};
        bounds = bounds.asRect;
		cstep = -1;
		offset = 0;
		view = UserView(parent, bounds);
		view.drawFunc = {
			this.draw;
		};
		data = argdata ?? [Sequence.new.length_(4)];
		view.mouseDownAction = {
			| view x y modifiers buttonNumber clickCount pos |
			var width = (view.bounds.width)/16;
			var height = (view.bounds.height)/(data.size);
			var xval = (x/width).asInteger;
			var yval = (y/height).asInteger;
			if((xval>=0) && (yval>=0), {
				this.setStep(xval+offset, yval,
					if(this.getStep(xval+offset, yval).size == 0, (), nil)
				);
				view.refresh;
			});
		};
    }

    draw {
		var width = (view.bounds.width)/16;
		var height = (view.bounds.height)/(data.size);
		var cstep_v = cstep - offset;
		// make the boxes
		data.do {
			| item y |
			var ypos = (y*height);
			16.do {
				| x |
				var xpos = (x*width);
				if((x + offset) < ((item.length)*4), {
					Pen.color = Color.gray(
						if(this.getStep((x + offset), y).size != 0,
							0,
							if((x + offset)%4==0, 0.4, 0.7)
						),
						0.95);
					Pen.fillRect(Rect(xpos+2, ypos+2, width-4, height-4));
				});
			};
		};
		// draw the "current step" box.
		if(cstep_v>=0, {
			Pen.color = Color.blue;
			Pen.fillRect(Rect(cstep_v*width, 0, width, view.bounds.height/20));
		});
		Pen.perform(\fill);
    }

	getStep {
		| x y |
		^data[y].eventsAt(x/4);
	}

	setStep {
		| x y event |
		data[y].removeAt(x/4);
		if(event.notNil, {
			data[y].add(event ++ (beat: x/4));
		});
		changeFunc.value(this);
	}

	refresh {
		view.refresh;
	}

	bounds_ {
		| bounds |
		bounds = bounds;
		view.bounds = bounds;
		this.refresh;
	}

	data_ {
		| argdata |
		data = argdata;
		changeFunc.value(this);
		this.refresh;
	}

	cstep_ {
		| num |
		cstep = num;
		this.refresh;
	}

	offset_ {
		| num |
		offset = num.clip(0, (this.data[0].length*4)-16);
		this.refresh;
	}

	add { // add a row.
		| protoEvent |
		data = data ++ [Sequence.new.protoEvent_(protoEvent).length_(4)];
		changeFunc.value(this);
		this.refresh;
	}

	del { // remove a row.
		| index |
		if(data.size > 1, {
			data.removeAt(index);
			changeFunc.value(this);
			this.refresh;
		});
	}

	expand { // double the length of the pattern.
		data.do({
			| cur |
			cur.length_((cur.length*2).round);
		});
	}

	shrink { // halve the length of the pattern.
		data.do({
			| cur |
			cur.length_((cur.length/2).round.max(1));
		});
	}

	play {
		data.do({
			| seq |
			seq.play;
		});
	}

	playStep {
		| step |
		data.size.do({
			| num |
			this.getStep(step, num).play;
		});
	}
}

SeqView : SCViewHolder {
	var <clock, <rows;

	*new {
		| parent bounds data |
		^super.new.init(parent, bounds, data);
	}

	init {
		| parent argbounds argdata |
		var keymap, bounds;
		bounds = argbounds ?? { Rect(0, 0, 500, 500) };
		bounds = bounds.asRect;
		view = UserView(parent, bounds).canFocus_(false);
		keymap = Keymap(());
		view.drawFunc_({
			| view |
			if(rows.size == 0, {
				Pen.color_(Color.red);
				Pen.addRect(view.bounds.asRect);
				Pen.fill;
			}, {
				var seqsRect = this.bounds;
				var oneHeight = (seqsRect.height/rows.size);
				var oneWidth = seqsRect.width;
				rows.do({
					| row num |
					var thisBounds = Rect(0, num*oneHeight, oneWidth, oneHeight);
					if(row.bounds != thisBounds, {
						row.bounds_(thisBounds);
					});
				});
			});
		});
		rows = [];
	}

	refresh {
		view.refresh;
	}

	bounds_ {
		| argbounds |
		view.bounds_(argbounds);
		this.refresh;
	}

	add { // add a new row
		| obj |
		if(obj.isKindOf(Class), {
			var thenew = obj.new(this, this.view.bounds);
			rows = rows ++ [thenew];
		}, {
			rows = rows ++ [obj];
		});
		this.refresh;
	}

	del { // delete a row. this is not 'remove' because that method is used for GUI-related stuff.
		| num |
		rows[num].remove;
		rows.removeAt(num);
		this.refresh;
	}
}

DrumSeq : SCViewHolder {
	var <bounds, <cseq=0, offset, <>changeFunc, textfields, <sg, <tfMouseDownAction; // data should be an array of Sequences.

	*new {
		| parent bounds data |
        ^super.new.init(parent, bounds, data);
    }

    init {
		| parent argbounds argdata |
		var keymap, sgview, osd, hl, tfs;
		bounds = argbounds ?? { Rect(0, 0, 500, 500) };
        bounds = bounds.asRect;
		view = View(parent, bounds).canFocus_(false);
		sgview = View(parent).canFocus_(false);
		keymap = Keymap((
			'C-i': {
				this.add;
			},
			'C-d': {
				this.remove;
			},
			'C-n': {
				this.cseq_(this.cseq+1);
			},
			'C-p': {
				this.cseq_(this.cseq-1);
			},
			'C-right': {
				this.offset_(this.offset + 16);
			},
			'C-left': {
				this.offset_(this.offset - 16);
			},
			'C-M-right': {
				this.expand;
				"HI".postln;
				// var len = this.data[this.cseq].length;
				// this.data[this.cseq].length_((len*2).round);
			},
			'C-M-left': {
				this.shrink;
				// var len = this.data[this.cseq].length;
				// this.data[this.cseq].length_((len/2).round.max(1));
			},
			'space': {
				if(Pdef(\__ds_play).isPlaying, {
					Pdef(\__ds_play).stop;
				}, {
					Pdef(\__ds_play, Prout({
						var cstep = 0;
						loop {
							{this.cstep_(cstep);}.defer;
							this.data.collect(_.eventsAt(cstep/4)).do(_.play);
							cstep = (cstep + 1)%(this.data[0].length*4);
							(1/4).wait;
						}
					})).play;
				// if(Tdef(\__ds_play).isPlaying, {
				// 	Tdef(\__ds_play).stop;
				// }, {
				// 	Tdef(\__ds_play, {
				// 		var cstep = 0;
				// 		loop {
				// 			this.data.do({
				// 				| seq |
				// 				var evts = seq.eventsAt(cstep/4);
				// 				evts.do({
				// 					| evt |
				// 					(evt).play;
				// 				});
				// 			});
				// 			{this.cstep_(cstep);}.defer;
				// 			cstep = (cstep + 1)%(this.data[0].length*4);
				// 			(1/4).wait;
				// 		};
				// 	}).play(TempoClock);
				});
			},
		));
		view.keyDownAction_({
			| view char modifiers unicode keycode |
			keymap.keyDown(Keymap.stringifyKey(modifiers, keycode)).value;
		});
		hl = HLayout().spacing_(0).margins_(0);
		view.layout = hl;
		tfs = UserView(parent).canFocus_(false);
		textfields = [];
		tfs.drawFunc_({
			| view |
			var width = (view.bounds.width);
			var height = (view.bounds.height)/(sg.data.size);
			textfields.do {
				| tf |
				tf.remove;
			};
			textfields = [];
			sg.data.size.do {
				| index |
				var ypos = (index*height);
				var textfield = TextField(view, Rect(0, ypos, width, height));
				textfields = textfields.add(textfield);
				textfield.string = sg.data[index].protoEvent[\instrument].asString;
				textfield.action = {
					| text |
					var pe = sg.data[index].protoEvent;
					pe[\instrument] = text.value.asSymbol;
					sg.data[index].protoEvent_(pe);
				};
				textfield.mouseDownAction_(tfMouseDownAction.(index, _, _, _, _, _, _));
			};
		});
		// osd = UserView(sgview, sgview.bounds).resize_(5).drawFunc_({
		// 	| view ds |
		// 	Pen.color_(Color.blue(1, 0.3));
		// 	Pen.addRect(Rect(ds.cstep*(view.bounds.width/16), 0, (view.bounds.width/16), (view.bounds.height)));
		// 	Pen.fill;
		// }.value(_, this)).acceptsMouse_(false);
		sg = SeqGrid(sgview, sgview.bounds).resize_(5).changeFunc_({
			| sg |
			this.refresh;
		});
		hl.add(tfs, 3);
		hl.add(sg.view, 16);
		// sgview.layout = StackLayout(osd, sg.view).mode = \stackAll;
    }

	tfMouseDownAction_ {
		| action |
		tfMouseDownAction = action;
		this.refresh;
	}

	refresh {
		view.refresh;
	}

	bounds_ {
		| argbounds |
		bounds = argbounds;
		view.bounds_(argbounds);
		this.refresh;
	}

	data_ {
		| argdata |
		var res = this.sg.data_(argdata);
		this.refresh;
		^res;
	}

	// cstep_ {
	// 	| num |
	// 	var res = this.sg.cstep_(num);
	// 	this.refresh;
	// 	^res;
	// }

	cseq_ {
		| num |
		cseq = num;
		this.refresh;
	}

	del {
		| index |
		this.sg.remove(index?(this.cseq));
	}

	doesNotUnderstand {
		| selector ... args |
		^Message(this.sg, selector, args).value;
	}

}