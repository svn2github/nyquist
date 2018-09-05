(expand 5)

(load "xlinit.lsp" :verbose NIL)
(setf *gc-flag* nil)
(load "misc.lsp" :verbose NIL)
;; now compute-default-sound-file is defined; needed by system.lsp ...
(load "evalenv.lsp" :verbose NIL)
(load "printrec.lsp" :verbose NIL)

(load "sndfnint.lsp" :verbose NIL)
(load "seqfnint.lsp" :verbose NIL)

(load "velocity.lsp" :verbose NIL) ; linear-to-vel etc
(load "system.lsp" :verbose NIL)
;; now *file-separator* is defined, used by nyquist.lsp...
(load "nyquist.lsp" :verbose NIL)


(load "seqmidi.lsp" :verbose NIL)
(load "nyqmisc.lsp" :verbose NIL)
(load "stk.lsp" :verbose NIL)
(load "envelopes.lsp" :verbose NIL)
(load "equalizer.lsp" :verbose NIL)
(load "xm.lsp" :verbose NIL)
(load "sal.lsp" :verbose NIL)


(format t "~%Nyquist -- A Language for Sound Synthesis and Composition~%")
(format t "    Copyright (c) 1991,1992,1995,2007-2018 by Roger B. Dannenberg~%")
(format t "    Version 3.15~%~%")
(load "extensions.lsp" :verbose NIL)

;(setf *gc-flag* t)


