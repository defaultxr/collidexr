Gene {
	var <>dna;
	*new {
		| size=6 |
		^super.new.init(size);
	}
	*fromDNA {
		| dna |
		^super.new.init.dna_(dna);
	}
	init {
		| argsize |
		dna = {1.0.rand}.dup(argsize);
	}
	mutation {
		| rate=0.05 |
		^Gene.fromDNA(dna + ({ rate.rand2 }.dup(dna.size)));
	}
}