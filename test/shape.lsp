;; there was a reported problem with shape reading 1 beyond the
;; end of the table -- this is a test to check it out

;; 20-Jun-97

; make sample rates low enough to look at by hand
(set-sound-srate 10)
(set-control-srate 10)

; make a table
(setf shape-signal (scale 2 (ramp 2)))
(print (snd-samples shape-signal 25))

; try it out
(setf input (scale 1.2 (stretch 3 (lfo .1))))
;(s-plot input)
(setf result (shape input shape-signal 1.0))
(print (snd-samples result 50))
;(s-plot result)

; conclusion :
; The shape must be defined all the way to the origin + 1.0,
; in this case, the origin is 1, so the shape must go to 2.0.
; Initially, I used (pwl 2 2 2 0) for the shape and had
; problems because this function is zero at 2.0!  I assume
; this is the source of the reported problem.  By using ramp,
; which actually extends to it's duration + 1 sample, we
; get the right result.

