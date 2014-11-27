;;; my-sclang.el --- my custom sclang stuff

;; Copyright (C) 2013  

;; Author:  <modula@iiiTreer>
;; Keywords: convenience, abbrev, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Notes:

;; TODO: add this in for post window colorization:

;; (defconst sclang-parse-error-regexp
;;   "^\\(WARNING\\|ERROR\\): .*\n[\t ]*in file '\\([^']\+\\)'\n[\t ]*line \\([0-9]\+\\) char \\([0-9]\+\\)"
;;   "Regular expression matching parse errors during library compilation.")

;;; Code:

(defun sclang-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun sclang-eval-balanced-expression ()
  "Automatically selects up to the last double-newline, executes the code, and returns to the original point."
  (interactive)
  (let ((region (get-current-paragraph)))
    (sclang-flash-region (car region) (cdr region))
    (sclang-eval-expression (buffer-substring-no-properties (car region) (cdr region)))))
(define-key sclang-mode-map (kbd "C-c C-c") 'sclang-eval-balanced-expression)

(defun sclang-make-var-list ()
  "Finds the variables and updates the var list."
  (interactive)
  (let ((region (get-current-paragraph))
        (args-list ())
        (vars-list ()))
    (save-excursion
      (save-restriction
        (narrow-to-region (car region) (cdr region))
        (goto-char (point-min))
        (let ((args (search-forward-regexp "^[\t ]*\\([|]\\|arg\\)\\(.*?\\)[;|]" nil t)))
          (when args
            (let* ((res (match-string-no-properties 2))
                   (res (replace-regexp-in-string "," " " res))
                   (res (replace-regexp-in-string "=[^ ]+" "" res))
                   (res (split-string res " "))
                   (res (remove-if (lambda (x) (equal "" x)) res)))
              (setf args-list res))))
        (goto-char (point-min))
        (let ((pos (search-forward-regexp "^[\t ]*var ")))
          (kill-line)
          (while (search-forward-regexp "^[\t ]*\\([A-Za-z0-9_]+\\)[\t ]*=[\t ]+" nil t)
            (setf vars-list (append vars-list (list (substring-no-properties (match-string 1))))))
          (goto-char pos)
          (let* ((new-vars-list (remove-duplicates vars-list :test 'equal))
                 (new-vars-list (remove-if (lambda (x) (position x args-list :test 'equal)) new-vars-list)))
            (insert (concat (mapconcat 'identity new-vars-list ", ") ";"))))))))
(define-key sclang-mode-map (kbd "C-c C-x") 'sclang-make-var-list)

(defun sclang-context-synth ()
  "Finds the name of the synth in the current context."
  (let ((region (get-current-paragraph))
        (regex "\\\\\\([a-zA-Z0-9]+\\)"))
    (save-excursion
      (backward-word)
      (backward-char)
      (if (looking-at-p regex)
          (progn
            (search-forward-regexp regex nil t)
            (substring-no-properties (match-string 1)))
        (save-restriction
          (narrow-to-region (car region) (cdr region))
          (goto-char (point-min))
          (or (search-forward "\\instrument" nil t)
              (search-forward "Pmono" nil t)
              (search-forward "Synth" nil t))
          (search-forward-regexp regex nil t)
          (substring-no-properties (match-string 1)))))))

(defun sclang-synth-args (synth)
  "Echoes the arguments of the synth."
  (message (sclang-eval-sync (concat "SynthDescLib.global.synthDescs[\\" synth "].controls.collect({|x|x.name ++ \"=\" ++ x.defaultValue ++ \" \"}).reduce('++')"))))

(defun sclang-context-synth-args ()
  "Echoes the arguments of the nearest synth."
  (interactive)
  (sclang-synth-args (sclang-context-synth)))
(define-key sclang-mode-map (kbd "C-c \\") 'sclang-context-synth-args)

(defun sclang-find-def-name-position ()
  "Returns the position in buffer of the name of the current paragraph's Def. In other words, a paragraph beginning with 'SynthDef(\\blah' would return the point of the backslash. Returns nil if no def name is found."
  (save-excursion
    (goto-char (car (get-current-paragraph)))
    (let ((res (search-forward-regexp "def(\\\\" (save-excursion (search-forward "\n" nil t)) t)))
      (when res
        (1- res)))))

(defun sclang-find-def-name ()
  "Returns the def name of the current paragraph (see `sclang-find-def-name-position') or nil if not found."
  (save-excursion
    (let ((dnp (sclang-find-def-name-position)))
      (when dnp
        (goto-char dnp)
        (search-forward ",")
        (backward-char)
        (buffer-substring-no-properties (1+ dnp) (point))))))

(defun sclang-find-def-type ()
  "Returns the type of the current paragraph (i.e. Ndef, Pdef, SynthDef, etc) or nil if not found."
  (save-excursion
    (let* ((cpb (car (get-current-paragraph)))
           (word (progn
                   (goto-char cpb)
                   (buffer-substring-no-properties cpb (progn (forward-word) (point))))))
      (when (let ((case-fold-search nil)) (string-match "^[A-Z]" word))
        word))))

(defun sclang-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (sclang-show-method-args))
(define-key sclang-mode-map (kbd "SPC") 'sclang-space)

;; (defun sclang-play-context (&optional freq) ;; OLDER VERSION
;;   "Plays (or stops if it is already playing) the paragraph's *def."
;;   (interactive "P")
;;   (save-excursion
;;     (let ((dnp (sclang-find-def-name-position))
;;           (string ""))
;;       (when dnp
;;         (goto-char dnp)
;;         (backward-word)
;;         (let ((context (buffer-substring-no-properties (point) (1- (search-forward-regexp "[,)]")))))
;;           (if (equal (substring context 0 4) "Synt")
;;               (setf string (concat "Synth("
;;                                           (substring context 9)
;;                                           (if freq
;;                                               (concat ", [\\freq, " (number-to-string freq) "]")
;;                                             "")
;;                                           ");"))
;;             (setf string (concat "if(" context ").isPlaying, { " context ")"
;;                                         (if (equal (substring context 0 4) "Ndef")
;;                                           (concat ".release(3);{3.wait;" context ").end;}.fork(SystemClock);")
;;                                         ".stop;")
;;                                         " }, { " context ").play; });")))
;;           (sclang-eval-expression string)
;;           (message string))))))

;; (defun sclang-play-context (&optional freq) ;; OLD VERSION
;;   "Plays (or stops if it is already playing) the paragraph's *def."
;;   (interactive "P")
;;   (save-excursion
;;     (sclang-eval-expression (concat "~startOrStop.(\"" (sclang-find-def-name) "\", \"" (sclang-find-def-type) "\");") t)))
;; (define-key sclang-mode-map (kbd "C-c p") 'sclang-play-context)

(defun sclang-play-context (&optional freq) ;; the new one
  "Plays (or stops if it is already playing) the paragraph's *def."
  (interactive "P")
  (let* ((paragraph (buffer-substring-no-properties (car (get-current-paragraph)) (cdr (get-current-paragraph))))
         (new-paragraph (mapconcat 'identity (split-string paragraph "\\\\") "\\\\"))
         (new-paragraph (mapconcat 'identity (split-string new-paragraph "\"") "\\\"")))
    (sclang-eval-expression (concat "~startOrStop.(\"" new-paragraph "\");") t)
    ;; (message (concat "~startOrStop.(\"" new-paragraph "\");") t)
    ))
(define-key sclang-mode-map (kbd "C-c p") 'sclang-play-context)

(defun tracker-change-number (arg) ;; maybe use this to increase the number for the following function in the future
  "Increase or decrease a number under the point."
  (interactive "p")
  (set-buffer tracker-buffer)
  (without-undo
   (save-excursion
     (search-backward-regexp "[^0-9-]")
     (forward-char)
     (let* ((start (point))
            (end (1- (save-excursion (search-forward-regexp "[^0-9-]"))))
            (string (buffer-substring start end)))
       (when (not (eq 0 (- end start)))
         (delete-char (- end start))
         (insert (number-to-string (+ (or arg 1) (string-to-number string)))))))))

(defun sclang-clone-context () ;; this function sucks. FIX: use the above function to change the number maybe?
  "Clones the current paragraph, giving the *def (if there is one) a new name."
  (interactive)
  (let ((point (point))
        (cp (get-current-paragraph)))
    (goto-char (car cp))
    (insert (buffer-substring (car cp) (cdr cp)) ?\n ?\n)
    (let ((dnp (sclang-find-def-name-position)))
      (goto-char dnp)
      (search-forward ",")
      (backward-char)
      (let ((num-end (point))
            (num-start (1+ (search-backward-regexp "[^0-9]" nil t))))
        (forward-char)
        (if (= num-end num-start) ;; there is no number at the end of the symbol
            (insert "1") ;; so we just inser
          (let* ((num (buffer-substring-no-properties num-start num-end))
                 (numl (length num))
                 (nump (1+ (string-to-int num))))
            (delete-char numl)
            (insert (number-to-string nump))))))
    (goto-char (+ 2 point (- (cdr cp) (car cp)))) ;; return to the original position within the "cloned" paragraph
    (let ((np (get-current-paragraph))) ;; if the new paragraph is bigger, move forward to compensate
      (forward-char (- (- (cdr np) (car np))
                       (- (cdr cp) (car cp)))))))
(define-key sclang-mode-map (kbd "C-c c") 'sclang-clone-context)

(defadvice sclang-free-node (before sclang-query-free-node activate)
  "Query before free node."
  (sclang-eval-string "s.queryAllNodes;" t))

(defun sclang-free-nodes (nodes)
  "Frees nodes specified by their ID. You can specify a range or a number."
  (interactive "sFree node(s): ")
  (sclang-eval-string (concat "~freeNodes.(\"" nodes "\");")))
(define-key sclang-mode-map (kbd "C-c f") 'sclang-free-nodes)

(defun sclang-query ()
  "Queries all nodes and patterns."
  (interactive)
  (sclang-eval-string "s.queryAllNodes;")
  (sclang-eval-string "(\"Pdefs: \" ++ ~patternsPlaying.().cs)" t))
(define-key sclang-mode-map (kbd "C-c q") 'sclang-query)

(defun sclang-query-all ()
  "Queries more stuff."
  (interactive)
  ;; (sclang-eval-string "~routinesPlaying.();" t))
  (sclang-eval-string "s.queryAllNodes;")
  (sclang-eval-string "(\"Pdefs: \" ++ ~patternsPlaying.().cs)" t)
  (sclang-eval-string "Buffer.cachedBuffersDo(s, {|b|b.postln;});" t))
(define-key sclang-mode-map (kbd "C-c Q") 'sclang-query-all)

(defun sclang-scope ()
  "Opens the scope for the server."
  (interactive)
  (sclang-eval-string "s.scope;"))
(define-key sclang-mode-map (kbd "C-c s") 'sclang-scope)

(defun sclang-freqscope ()
  "Opens the freqscope for the server."
  (interactive)
  (sclang-eval-string "s.freqscope;"))
(define-key sclang-mode-map (kbd "C-c S") 'sclang-freqscope)

(defun sclang-test-env ()
  "Plots the nearest Env."
  (interactive)
  (save-excursion
    (search-backward "Env")
    (search-forward-regexp "\\(\\(Env\\b\\)[\\.a-z]*?\(.*?\)\\)" nil t)
    (sclang-eval-string (concat (match-string 1) ".plot"))))
(define-key sclang-mode-map (kbd "C-c t") 'sclang-test-env)

(defun sclang-change-tempo (tempo)
  "Changes the tempo of the default tempoclock."
  (interactive "NTempo in BPM: ")
  (sclang-eval-string (concat "TempoClock.default.tempo = " (number-to-string tempo) "/60")))
(define-key sclang-mode-map (kbd "C-c T") 'sclang-change-tempo)

(defun sclang-pan2->balance2 ()
  "Converts a Pan2 into a Balance2."
  (interactive)
  (message "P2B")
  (forward-char 1) ;; in case we are on the Pan2 already..
  (search-backward "Pan2" nil t)
  (kill-word 1)
  (insert "Balance2")
  (forward-word 2)
  (backward-word)
  (kill-word 1)
  (yank)
  (insert "[0], ")
  (yank)
  (insert "[1]"))

(defun sclang-balance2->pan2 ()
  "Converts a Balance2 into a Pan2."
  (interactive)
  (message "B2P")
  (forward-char 1) ;; in case we are on the Balance2 already..
  (search-backward "Balance2" nil t)
  (kill-word 1)
  (insert "Pan2")
  (forward-word 2)
  (kill-word 3)
  (delete-char 1))

(defun sclang-transform () ;; FIX: make Ndef->SynthDef and SynthDef->Ndef
  "Transforms a nearby statement into an equivalent one. You must be on the
same line as the thing you want to change.

Transformations supported:

* Pan2->Balance2 and Balance2->Pan2"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((line (progn (search-forward-regexp "^\\(.*\\)$" nil t) (match-string 1))))
      (cond
       ((search "Pan2" line) (sclang-pan2->balance2))
       ((search "Balance2" line) (sclang-balance2->pan2))))))
(define-key sclang-mode-map (kbd "C-c C-t") 'sclang-transform)

(defun sclang-synth ()
  (completing-read "Synth: " (mapcar (lambda (x) (cons x x)) (sclang-eval-sync "~synthsList.();"))))

(defun sclang-previous-paragraph-name ()
  (save-excursion
    (search-backward "\n\n" nil t) ;; FIX ?
    (sclang-find-def-name)))

(defmacro sclang-skeleton (name desc query &rest body)
  "Write a sclang skeleton and add it to the sclang skeletons list."
  (let ((func-sym (intern (concat "sclang-skeleton-" (symbol-name name)))))
    `(progn
       (define-skeleton ,func-sym
         ,desc
         ,query
         ,@body)
       (add-to-list '*sclang-skeletons* ',name))))

;; (sclang-skeleton buffer ;; FIX
;;   "Write a buffer read via the .b shortcut."
;;   "\""
;;   (my-get-file-name)
;;   "\".b;" |)

(setf *sclang-skeletons* '())

(defun sclang-gen-random-number-string ()
  "Generates a random number as a string."
  (number-to-string (random 1000)))

(sclang-skeleton pbind
                 "Write a Pbind."
                 ""
                 "Pbind(\\instrument, \\" (sclang-synth) | "default" ",\n\t\\dur, 1,\n\t\\freq, 440," _ "\n)") ;; FIX - make a function to get a synth's args and put them into the pattern.

(sclang-skeleton pd
                 "Write a Pd."
                 ""
                 "Pd(\\i" (sclang-gen-random-number-string) ", \n\t\\instrument, \\" (sclang-synth) | "default" ",\n\t\\dur, 1,\n\t\\freq, Pseq([440, ], ),\n\t//\\midinote, Pseq([69, ], )," _ "\n).condition_(_.isNil);")

(sclang-skeleton pdpbp
                 "Write a Pdef and finite Pbind set to loop."
                 ""
                 "Pdef(\\i" (sclang-gen-random-number-string) ", Pb(\\instrument, \\" (sclang-synth) | "default" ",\n\t\\dur, 1,\n\t\\freq, Pseq([440, ], ),\n\t//\\midinote, Pseq([69, ], )," _ "\n)).condition_(_.isNil);")

(sclang-skeleton pdpbpj
                 "Write a Pdef containing a Psym to play a finite Pdef."
                 ""
                 "~xx = [\\" (sclang-previous-paragraph-name) "];\n" _ "\nPdef(\\j, Psym(Pfunc({~xx})));")

(sclang-skeleton pdpbpn
                 "Write a Pdef-Pbind and a Pdef-Psym."
                 ""
                 "Pdef(\\i, Pb(\\instrument, \\" (sclang-synth) | "default" ",\n\t\\dur, 1,\n\t\\freq, Pseq([440, ], ),\n\t//\\midinote, Pseq([69, ], )," _ "\n)).condition_(_.isNil);\n\nPdef(\\j, Psym([\\i]));")

(sclang-skeleton ndef
                 "Write an Ndef."
                 "Ndef name: "
                 "Ndef(\\" str | "ndef1" ", {\n\t| freq=440 amp=0.5 pan=0 | //out=0 |\n\tvar output;\n\toutput = " _ "\n\tB2.ar(output, pan, amp);\n}).play;")

(sclang-skeleton envs
                 "Write an Env with .kr."
                 ""
                 "Env([1, 0], [1]).kr();")

(sclang-skeleton synthdef
                 "Write a SynthDef."
                 "Synth name: "
                 "SynthDef(\\" str | "s1" ", {\n\t| gate=1 freq=440 amp=0.5 pan=0 out=0 |\n\tvar env, output;\n\tenv = Env.adsr(0.01, 0.1, 1, 0.5).kr(2, gate);\n\toutput = " _ "\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\freq, 440]);")

(sclang-skeleton synthdef-synth
                 "Write a synth SynthDef."
                 "Synth name: "
                 "SynthDef(\\" str | "synth1" ", {\n\t| gate=1 freq=440 amp=0.5 pan=0 out=0 |\n\tvar env, output;\n\tenv = Env.adsr(0.1, 0.5, 0.5, 1).kr(2, gate);\n\toutput = " _ "\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\freq, 440]);")

(sclang-skeleton synthdef-drum
                 "Write a drum SynthDef."
                 "Synth name: "
                 "SynthDef(\\" str | "drum1" ", {\n\t| freq=440 amp=0.5 pan=0 out=0 |\n\tvar env, output;\n\tenv = Env.perc(0.01, 0.3, 1, -8).kr(2);\n\toutput = " _ "\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\freq, 440]);")

(sclang-skeleton synthdef-pm
                 "Write a 3->2->1 phase modulation-based SynthDef."
                 "Synth name: "
                 "SynthDef(\\" str | "pm1" ", {\n\t| gate=1 freq=440 amp=0.5 pan=0 out=0 |\n\tvar env, penv, osc3, depth, osc2, osc1, output;\n\tenv = Env.adsr(0.1, 0.5, 0.5, 1).kr(2, gate);\n\tpenv = Env.adsr(0.1, 0.5, 0.2, 1, 1, 0).kr(0, gate);\n\tosc3 = SinOsc.ar(freq.transpose(0, 0));\n\tosc2 = SinOsc.ar(freq.transpose(0, -1));\n\tdepth = 0.002*penv;\n\tosc2 = DelayL.ar(osc2, 0.2, ((osc3)\n\t\t*depth)+depth);\n\tosc1 = SinOsc.ar(freq.transpose(0, -2));\n\tdepth = 0.001*penv;\n\tosc1 = DelayL.ar(osc1, 0.2, ((osc2)\n\t\t*depth)+depth);\n\toutput = osc1;" _ "\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\freq, 440]);")

(sclang-skeleton synthdef-pm23-1
                 "Write a 2+3->1 phase modulation-based SynthDef."
                 "Synth name: "
                 "SynthDef(\\" str | "pm1" ", {\n\t| gate=1 freq=440 amp=0.5 pan=0 out=0 |\n\tvar env, penv, osc3, osc2, depth, osc1, output;\n\tenv = Env.adsr(0.1, 0.5, 0.5, 1).kr(2, gate);\n\tpenv = Env.adsr(0.1, 0.5, 0.2, 1, 1, 0).kr(0, gate);\n\tosc3 = SinOsc.ar(freq.transpose(0, 0));\n\tosc2 = SinOsc.ar(freq.transpose(0, -1));\n\tosc1 = SinOsc.ar(freq.transpose(0, -2));\n\tdepth = 0.001*penv;\n\tosc1 = DelayL.ar(osc1, 0.2, ((osc2+osc3)/2\n\t\t*depth)+depth);\n\toutput = osc1;" _ "\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\freq, 440]);")

(sclang-skeleton synthdef-pm2-1
                 "Write a 2->1 phase modulation-based SynthDef."
                 "Synth name: "
                 "SynthDef(\\" str | "pm1" ", {\n\t| gate=1 freq=440 amp=0.5 pan=0 out=0 |\n\tvar env, penv, osc2, depth, osc1, output;\n\tenv = Env.adsr(0.1, 0.5, 0.5, 1).kr(2, gate);\n\tpenv = Env.adsr(0.1, 0.5, 0.2, 1).kr(0, gate);\n\tosc2 = SinOsc.ar(freq.transpose(0, -1));\n\tosc1 = SinOsc.ar(freq.transpose(0, -2));\n\tdepth = 0.001*penv;\n\tosc1 = DelayL.ar(osc1, 0.2, (osc2\n\t\t*depth)+depth);\n\toutput = osc1;" _ "\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\freq, 440]);")

;; (bounds-of-thing-at-point 'sexp) ;; FIX - use this function!

(sclang-skeleton synthdef-sndp
                 "Write a SndP-based SynthDef template."
                 "Synth name: "
                 "SynthDef(\\" str ", {\n\t| gate=1 rate=1 amp=0.5 pan=0 out=0 |\n\tvar bufnum, env, output;\n\tbufnum = \"\".b;\n\tenv = Env.asr(0.01, 1, 0.01).kr(2, gate);\n\toutput = SndP.ar(bufnum, rate);\n\tOut.ar(out, B2.ar(output, pan, env * amp));\n}).play(args:[\\bufnum, " _ "]);")

;; (sclang-skeleton synthdef-sample
;;   "Write a sample SynthDef template."
;;   "Synth name: "
;;   "SynthDef(\\" str ", {\n\t| bufnum rate=1 start=0 end=1 amp=0.5 pan=0 out=0 |\n\tvar bufframes, line_dur, startpos, endpos, line, output;\n\tbufframes = BufFrames.kr(bufnum);\n\tline_dur = (bufframes/BufSampleRate.kr(bufnum)*(abs(start-end)))/abs(rate);\n\tstartpos = ((rate>0)*start)+((rate<0)*end);\n\tendpos = ((rate>0)*end)+((rate<0)*start);\n\tline = Line.ar(startpos*bufframes, endpos*bufframes, line_dur, doneAction: 2);\n\toutput = BufRd.ar(2, bufnum, line);\n\toutput = B2.ar(output, pan);\n\tOut.ar(out, output * amp);\n}).play(args:[\\bufnum, " _ "]);")

(sclang-skeleton window
  "Write a template for a GUI."
  "Window title: "
  "(\nvar win, view;\nwin = Window(\"" str "\");\nview = UserView(win, win.view.bounds).resize_(5);\nview.keyDownAction_({\n\t| view char modifiers unicode keycode |\n});\n" _ "\nwin.front;\n)")

;; (sclang-skeleton class) ;; FIX

;; (sclang-skeleton nodeproxy
;;   "Write a template for a NodeProxy."
;;   ""
;;   "a = NodeProxy.audio(s, 2);\na.play;\n\na[0] = {\n\tvar output;\n\toutput = " _ "\n};")

(sclang-skeleton sound ;; FIX
  "Write a template for a sound."
  "Sound file: "
  "\"" (my-insert-file-name) "\".b")

(sclang-skeleton synth
                 "Write the name of a Synth."
                 ""
                 "\\" (sclang-synth))

(defun sclang-wiimote-key () ;; FIX
  (completing-read "Wiimote action: "
                   (mapcar (lambda (x) (cons x x))
                           (list "ao" "ax" "ay" "az" "bA" "bB" "bDown" "bHome" "bLeft" "bMinus" "bOne" "bPlus" "bRight" "bTwo" "bUp" "battery" "caleft" "caright" "cbA" "cbB" "cbDown" "cbHome" "cbL" "cbLeft" "cbMinus" "cbPlus" "cbR" "cbRight" "cbUp" "cbX" "cbY" "cbZL" "cbZR" "csx1" "csx2" "csy1" "csy2" "led1" "led2" "led3" "led4" "nao" "nax" "nay" "naz" "nbC" "nbZ" "nsx" "nsy"))))

(sclang-skeleton wiimote
                 "Write a template for responding to wiimote events."
                 ""
                 "w = WiiMote.all[0];

w.setAction(\\" (sclang-wiimote-key) ", {
	| val |
	" _ "
});")

(defun sclang-insert-skeleton (skeleton)
  "Insert a skeleton into the buffer."
  (interactive
   (list
    (completing-read "Skeleton: " (mapcar 'symbol-name *sclang-skeletons*))))
  (let ((func-name (intern (concat "sclang-skeleton-" skeleton))))
    (funcall func-name)))
(define-key sclang-mode-map (kbd "C-c j") 'sclang-insert-skeleton)

(defmacro sclang-skeleton (name desc query &rest body)
  "Write a sclang skeleton and add it to the sclang skeletons list."
  (let ((func-sym (intern (concat "sclang-skeleton-" (symbol-name name)))))
    `(progn
       (define-skeleton ,func-sym
         ,desc
         ,query
         ,@body)
       (add-to-list '*sclang-skeletons* ',name))))

(defmacro sclang-macro (key function &optional desc)
  "Defines a \"macro\" for SCLang mode. Pressing C-c m <key> in SCLang mode will
run the SuperCollider code provided as the 'function' argument."
  (let ((key-combo (concat "C-c m " key)))
  `(define-key sclang-mode-map (kbd ,key-combo) (lambda () ,desc (interactive) (sclang-eval-string ,function)))))

(sclang-macro "i" "~keyInfo.();"
              "Opens the key information window.")

(sclang-macro "p" "~pitchTrack.();"
              "Starts or stops the pitch tracker synth.")

(sclang-macro "q" "Quarks.gui;"
              "Opens the Quarks GUI.")

(sclang-macro "a" "SFV();"
              "Opens an instance of SFV (Audio editor)")

(sclang-macro "s" "Structer().makeWindow;"
              "Opens Structer.")

(sclang-macro "m" "if(Pdef(\\metronome).isPlaying, { Pdef(\\metronome).stop; }, { Pdef(\\metronome).play; });"
              "Starts or stops the metronome.")

(sclang-macro "w" "if(WiiMote.all.size < 1, { WiiMote.discover; }, { WiiMote.all.size.asString+\"Wiimotes currently registered.\".postln; });"
              "Detects Wiimotes.")

(sclang-macro "K" "Server.killAll;"
              "Kills all active servers.")

(sclang-macro "h" "History.document;"
              "Shows the current History document.")

(define-key sclang-mode-map (kbd "C-z") (lambda () (interactive) (call-process "stumpish" nil 0 nil "select" "Structer")))

;;;; sclang-piano-mode

(defun sclang-piano-set-timer ()
  (setf sclang-piano-timer (run-with-timer 0.2 nil 'sclang-piano-free-key)))

(defun sclang-piano-free-key ()
  (sclang-eval-string "~emacsPianoSynth.release;~emacsPianoSynth = nil;")
  (setf sclang-piano-timer nil))

(defun sclang-piano-play-key ()
  (interactive)
  (let ((pos (position last-input-event '(122 115 120 100 99 118 103 98 104 110 106 109 44 108 46 59 47))))
    (when pos
      (if (and sclang-piano-timer
               (eq last-input-event sclang-piano-key))
          (progn
            (cancel-timer sclang-piano-timer)
            (sclang-piano-set-timer))
        (progn
          (when sclang-piano-timer
            (cancel-timer sclang-piano-timer))
          (sclang-piano-free-key)
          (sclang-eval-string (concat "~emacsPianoSynth = Synth(\\" sclang-piano-synth ", [\\freq, " (number-to-string (+ pos (* sclang-piano-octave 12))) ".midicps, \\amp, 0.5]++~emacsPianoSynthArgs);"))
          (sclang-eval-string (concat "if(\\" sclang-piano-synth ".def.hasGateControl.not, { ~emacsPianoSynth = nil; });"))
          (setf sclang-piano-key last-input-event)
          (sclang-piano-set-timer))))))

(defvar sclang-piano-octave 5)

(defun clip (num bottom top)
  "Clips numbers within a range."
  (min top (max bottom num)))

(defun sclang-piano-set-octave (octave)
  (interactive "NOctave: ")
  (setf sclang-piano-octave (clip octave 0 8)))

(defvar sclang-piano-type 0) ;; 0 for Synth, 1 for Ndef

(defvar sclang-piano-synth "default")

(defvar sclang-piano-timer nil)

(defvar sclang-piano-key nil)

(defun sclang-piano-select-synth (synth)
  (interactive (list (sclang-synth)))
  (setf sclang-piano-synth synth))

(defun sclang-piano-guess-synth ()
  (interactive)
  (let ((dn (sclang-find-def-name))
        (dt (sclang-find-def-type)))
    (when dn
      (setf sclang-piano-synth dn))))

(defun nothing ()
  "Does nothing."
  (interactive)
  nil)

(define-minor-mode sclang-piano-mode
  "Toggle SCLang Piano Mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When SCLang Piano Mode is enabled, your computer keyboard acts as a piano
keyboard."
  nil
  " Piano"
  :keymap (let ((m (make-keymap)))
            (mapcar (lambda (x) (define-key m x 'sclang-piano-play-key)) '("z" "s" "x" "d" "c" "v" "g" "b" "h" "n" "j" "m" "," "l" "." ";" "/"))
            (mapcar (lambda (x) (define-key m x 'nothing)) '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "a" "f" "k" "'"))
            (define-key m (kbd "\\") 'sclang-piano-select-synth)
            m))
(define-key sclang-mode-map (kbd "C-c M-c") 'sclang-piano-mode)
(define-key sclang-mode-map (kbd "C-c k") 'sclang-piano-mode)
(define-key sclang-mode-map (kbd "C-\\") 'sclang-piano-mode)
(define-key sclang-piano-mode-map (kbd "`") 'sclang-piano-set-octave)
(define-key sclang-piano-mode-map (kbd "C-g") 'sclang-piano-mode)
(define-key sclang-piano-mode-map (kbd "RET") 'sclang-piano-guess-synth)

;; (defun sclang-genetic-generate (genome-length)
;;   "Generate a random SynthDef using the RedGAGenome and RedGAPhenome classes."
;;   (interactive "P")
;;   (sclang-eval-string (concat "~emacs_genome = " (number-to-string (or genome-length 15)) ".collect({1.0.rand});~emacs_phenome = RedGAPhenome(RedGAGenome(~emacs_genome));~emacs_phenome.asDefString2.postln;~emacs_phenome.asSynthDef2.play;")))
;; (define-key sclang-mode-map (kbd "C-c g g") 'sclang-genetic-generate)

;; (defun sclang-genetic-insert ()
;;   "Insert the last generated SynthDef into the document."
;;   (interactive)
;;   (insert (sclang-eval-sync "~emacs_phenome.asDefString2")))
;; (define-key sclang-mode-map (kbd "C-c g i") 'sclang-genetic-insert)

(provide 'my-sclang)

;;; my-sclang.el ends here
