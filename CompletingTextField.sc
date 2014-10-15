// CompletingTextField.sc
// class file for TextFields that have completion

CompletingTextField : SCViewHolder {
	var <parent, <bounds, <tf, <lv, <kda, <>completionHeight=200, <completions, <showCompletions=false, <>autohide=true;
	*new {
		| parent bounds |
		^super.new.init(parent, bounds??{parent.bounds});
	}
	init {
		| parent argbounds |
		// parent = parent; // parent.getParents.wrapAt(-1);
		bounds = argbounds ?? {Rect(0, 0, 500, 500)};
		bounds = bounds.asRect;
		tf = TextField(parent, bounds);
		view = tf;
		tf.keyDownAction_({
			| view char modifiers unicode keycode |
			var res = kda.value(view, char, modifiers, unicode, keycode);
			if(res == true, true, {
				if(res.isNil and: lv.notNil, {
					switch(keycode,
						116, { // down
							var t = if(lv.value.isNil, 0, { (lv.value + 1).clip(0, lv.items.size) });
							lv.value = if(t==lv.items.size, nil, t);
						},
						111, { // up
							var t = if(lv.value.isNil, lv.items.size-1, { (lv.value - 1).clip(-1, lv.items.size-1) });
							lv.value = if(t==(-1), nil, t);
						},
						114, { // right arrow
							this.complete;
							true;
						},
						41, { // f
							if(modifiers.isCtrl, {
								this.complete;
								true;
							});
						},
					);
				});
			});
		});
		tf.keyUpAction_({
			| view char modifiers unicode keycode |
			if([111,116].includes(keycode).not and: { lv.notNil }, {
				this.filter;
			});
		});
		tf.focusGainedAction_({
			if(autohide, {
				this.showCompletions_(true);
			});
		});
		tf.focusLostAction_({
			if(autohide, {
				this.showCompletions_(false);
			});
		});
		// ~ctf = this;
	}
	// doesNotUnderstand { arg selector...args;
    //     // (this.class++" does not understand method "++selector);

    //     // If(UGen.findRespondingMethodFor(selector).notNil){
    //     //     "But UGen understands this method".postln
    //     // };
	// 	Message(tf, selector, args).value;
    // }
	// toolTip_ {
	// 	| text |
	// 	tf.toolTip_(text);
	// }
	// string {
	// 	^tf.string;
	// }
	string_ {
		| string |
		tf.string_(string.asString);
		if(autohide, {
			this.showCompletions_(false);
		});
	}
	showCompletions_ {
		| bool |
		if(bool == true, {
			showCompletions = true;
			lv = ListView(view.parents.at(0),
				Rect(
					tf.bounds.left+tf.parent.bounds.left,
					tf.bounds.top+tf.bounds.height+tf.parent.bounds.top,
					tf.bounds.width,
					completionHeight
				)
			);
			this.filter;			
		}, {
			showCompletions = false;
			if(lv.notNil, {
				lv.remove;
			});
			lv = nil;			
		});
	}
	hide {
		"CompletingTextField.hide is deprecated. Use the showCompletions_ method instead.".warn;
		this.showCompletions_(false);
	}
	show {
		"CompletingTextField.show is deprecated. Use the showCompletions_ method instead.".warn;
		this.showCompletions_(true);
	}
	completions_ { // can be a list or a function
		| list |
		completions = list.value.collect(_.asString);
	}
	filteredCompletions {
		| string |
		string = string??{tf.string};
		if(string == "", {
			^completions;
		}, {
			^completions.select(_.containsi(string));
		});
	}
	filter {
		// if(tf.string == "", {
		// 	lv.items = completions;
		// }, {
		// 	lv.items = completions.select(_.containsi(tf.string));
		// });
		lv.items = this.filteredCompletions;
		lv.value = nil;
	}
	complete {
		if(lv.items.size > 0, {
			tf.value_(lv.items[0]);
		});
	}
	action_ {
		| action |
		tf.action = {
			if(lv.value.notNil, {
				tf.value = lv.items[lv.value];
			});
			action.value(this);
			if(autohide, {
				this.showCompletions_(false);
			});			
		};
	}
	keyDownAction_ {
		| action |
		kda = action;
	}
	remove {
		tf.remove;
		this.showCompletions_(false);
		view.remove;
	}
}