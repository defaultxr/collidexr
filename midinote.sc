// Lance Putnam, with really minor changes by andreavalle

+SimpleNumber {
	
	midinote { arg style = \American ;
		var offset = 0 ;
		var midi, notes;
		case { style == \French } { offset = -1}
			{ style == \German } { offset = -3} ;
		midi = (this + 0.5).asInteger;
		notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
		^(notes[midi%12] ++ (midi.div(12)-1+offset))
	}
}

+String {
	notemidi { arg style = \American ;
		var offset = 0 ; // French usage: +1 ; German usage: +3
		var twelves, ones, octaveIndex, midis;

		case { style == \French } { offset = 1}
			{ style == \German } { offset = 3} ;
		
		midis = Dictionary[($c->0),($d->2),($e->4),($f->5),($g->7),($a->9),($b->11)];
		ones = midis.at(this[0].toLower);
		
		if( (this[1].isDecDigit), {
			octaveIndex = 1;
		},{
			octaveIndex = 2;
			if( (this[1] == $#) || (this[1].toLower == $s) || (this[1] == $+), {
				ones = ones + 1;
			},{
				if( (this[1] == $b) || (this[1].toLower == $f) || (this[1] == $-), {
					ones = ones - 1;
				});
			});
		});
		twelves = (this.copyRange(octaveIndex, this.size).asInteger) * 12;
		^(twelves + 12 + ones + (offset*12))
	}
}

+Symbol {
	notemidi {
		| style=\American |
		^this.asString.notemidi;
	}
}

+SequenceableCollection {
	midinote { ^this.performUnaryOp('midinote') }
	notemidi { ^this.performUnaryOp('notemidi') }
	play     { ^this.performUnaryOp('play') }
}