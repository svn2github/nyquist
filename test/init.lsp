; init.lsp -- default Nyquist startup file
(load "nyinit.lsp")

; add your customizations here:
;    e.g. (setf *default-sf-dir* "...")

(echoenabled nil)
(play (pluck c4 0.1))
;(print "type something to continue")
;(read)
(print "computing convolution")
'(play (snd-convolve (osc c4) (snd-from-array 0 44100.0 
  (vector 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))


(setf impulse (snd-from-array 0 44100.0 
      (vector 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;(setf echoes (simrep (i 3) (at (* i 0.2)  (cue impulse))))
(setf echoes (sim (cue impulse) (at (/ 32768.0 44100) (cue impulse))))
(display "len" (snd-length echoes ny:all))

(play (snd-convolve (pluck c4 4.0) echoes))

