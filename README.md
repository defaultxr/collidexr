supercollidexr
==============

(Some of) my SuperCollider classes and related files.

introduction
============

Right now I'm working on a lot of Classes and convenience functions for SuperCollider. Of course, I want to share these. They are licensed under the MIT license, provided as the "LICENSE" file in this repository.

what's inside?
==============

extensions.sc
-------------

This file mostly contains a lot of additions to classes that are already built-in to SuperCollider.

It includes the "view" and "gui" methods for a few classes. 'view' returns a View representing the object, while gui opens a window using that View.

* BufferDirectories - you don't need to use this class. It's just needed to store data for the methods added to String.
* Object - view and gui methods for Object. These are just placeholders for methods that other classes should overload.
* Symbol - view method
* String - lots of stuff!
  * p - shortcut for standardizePath, because i hate typing long words with capital letters in them.
  * b - returns a stereo Buffer of the specified file. If you call it twice with the same filename, it will return the already-loaded Buffer instead of loading the file again and giving you a different Buffer. Because of this, you don't have to worry about accidentally loading the same file twice. It also takes care of converting the file if it's not a *.wav.
  * bm - same as the 'b' method, but returns a mono Buffer. Uses a separate dictionary from 'b' so you can have a stereo Buffer of a file, as well as a mono Buffer of the same file.
  * convertSong - uses external programs to decode compressed audio files (mp3, flac, ogg) to a wav in a temporary directory. Used by 'b' and 'bm'.
  * silentCommand - runs a command in the background, silently, writing no output to the postbuffer. 
  * speak - i implemented this method for Linux users, using Espeak.
  * view - view method.
* SimpleNumber - a few math utilities to reduce your cognitive load a tiny bit.
  * cpsrate - convert a frequency (Hz) to a rate that you can use with something like PlayBuf to play a sample at a pitch. You can also supply a different base frequency (midi note 60 (261.63Hz) is the default).
  * ratecps - does the opposite.
  * midirate - same as cpsrate, but using a midi note number instead of a frequency.
  * ratemidi - does the opposite.
  * beatstime - converts a number of beats to an amount of time (in seconds) based on a TempoClock.
  * timebeats - converts an amount of time (seconds) to an amount of beats based on a TempoClock.
  * transpose - transpose a frequency relatively, by semitones and/or octaves.
* SequenceableCollection - mostly contains methods that apply the above additions to SimpleNumber to a whole collection.
  * play - can't remember why i had this in here, but i probably used it for something, so i'm keeping it in for now.
  * normalAt - use a number in 0..1 to access an element of the Collection. 0 is the first, 0.999... is the last.
* Pattern - fork method (for compatibility with Pdef's fork method.)
* Pbind - view method. Really simple right now, but i'll make it better later.
* Env - adr method, which is basically like adsr without sustaining.
* UGen - a few things...
  * delay - wraps the UGen in a DelayL UGen, delaying it. Sort of like how the 'lag' method does the same with Lag.
  * transpose - same as the above SimpleNumber.transpose method, but for the server!
  * beatstime - same as the above SimpleNumber.beatstime method, but for the server!

my-patterns.sc
--------------

Some pattern classes i've written. May or may not be useful. I'm still wrapping my head around Patterns, so these classes might start your house on fire, or just be useless.

FIX: write more about these.

my-ugens.sc
-----------

Some pseudo-UGens i've written.

* HardGate - any number whose absolute value isn't greater than the 'level' is changed to 0.
* RateShift - based on the LADSPA plugin of the same name. (FIX: elaborate)
* SndP - slightly easier than PlayBuf. (FIX: elaborate)

Keymap.sc
---------

This file contains the Keymap class, which can be used to easily create keymaps (like Emacs).

FIX: write more about this.

future
======

I'm working on more stuff for SuperCollider, which i will upload to this repo once i've gotten them to a (mostly) usable and stable state.

* CompletingTextField - a TextField that, when focused, displays a ListView showing possible completions.
* Ctl - client-side fast and easy "control" abstraction. It can be used in Patterns. The general idea is similar to making a ~globalVariable and referencing it from a Pfunc, but it's quicker and simpler because you don't need to create the variable ahead of time (it defaults to 1), nor do you need to wrap it in a Pfunc. You can also get a GUI of the Ctl (with a Knob and a display of the current value), as well as set a ControlSpec.
* Effects - fast and easy way to make and manage an effects chain. Will also have a good 'view' method eventually, so you can easily edit the effects Synths being managed by it.
* KBD - might scrap this. It's a (computer-)keyboard interface for making music with. It shows what each key on your laptop's keyboard is mapped to. Uses Keymap, but is slightly more complex than just using Keymap bare-bones.
* Markers - mark split points in an audio file. Could be used, for example, to split up a drumloop by each hit. There will be a method to allow you to get the split points as an Array which could be used as a Synth input (i already have this Synth written and it works well!)
* PianoKeys - use your computer keyboard as a piano keyboard! I've made this specifically for Linux users, because it seems that X sends "key pressed" and "key released" messages while you hold down a key. I've filtered them out and this is the result. Now you can hold down a key to play a longer note. Will also have recording capability.
* SFV - (need a different name for this.) "SoundFileView" extension. Has the ability to make split points (with the above-described Markers) in an audio file. I'm hoping to turn this into an Audacity clone in the future. Wish me luck with that.
* SWModule - "module" system, to make it easy(?) to generate GUIs for creating and editing Synths. Might have to scrap the design and start over though.
* Sequence - represents a pattern of notes based on when they start, relative to when the pattern starts, rather than by the time between each Event. This will be useful for stuff like a Tracker or drum sequencer, where it would be more difficult to use the Pattern classes to represent their data. Has the ability to export its data as a regular Pattern.
* SeqGrid - (need a different name, probably.) GUI drum sequencer. Should use Sequence to hold the data (right now, it doesn't - FIX that).
* Tracker - GUI live-codable tracker. A column can supply parameters to Synths, or can be arbitrary SCLang code to execute. I gave up on the Emacs tracker idea i was writing a while back ( https://github.com/defaultxr/tracker.el ), and decided to just write a Tracker in SuperCollider instead.
* UVOM - (need a better name for this.) GUI piano roll sequencer. Forked from crucialfelix's UserViewObjectsManager (from his crucialviews quark).

All of the GUI-based projects will have Keymaps that emulate Emacs' style as closely as possible. But the Keymaps will be separate from the actual functionality, so you should be able to swap out your own Keymap fairly easily.
