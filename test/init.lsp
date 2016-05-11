; init.lsp -- default Nyquist startup file
(print "nyquist/test/init.lsp is loading...")
(load "nyinit-dbg.lsp")

; add your customizations here:
;    e.g. (setf *default-sf-dir* "...")

(echoenabled nil)
;(play (pluck c4 0.1))
;(print "type something to continue")
;(read)

;(load "convolve")

;(setf echoes (simrep (i 3) (at (* i 0.2)  (cue impulse))))
;(setf echoes (sim (cue impulse) (at (/ 32768.0 44100) (cue impulse))))
;(display "len" (snd-length echoes ny:all))

;(play (snd-convolve (pluck c4 4.0) echoes))

(echoenabled nil)
;(sal-load "phasevocoder.sal")
;(load "long.lsp")

(setf *gc-flag* nil)
;(snd-set-max-audio-mem 5000000)
;(gc)
;(info)
;(play (setf xxx (osc c4 35)))

;(setf xx (snd-from-array 0 100.0 (vector 0 1 2 3 4 5 6 7 8 9)))
;(setf yy (force-srate 70 xx))
;(print (snd-samples yy 100))

(s-plot (convolve (pluck c4) (osc c7 0.01)))

