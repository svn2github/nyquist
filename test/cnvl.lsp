;; cnvl.lsp -- convolve test

;; original bug: convolve truncated the result to the duration of the
;;   first parameter.
;; the fix: convolve.c was hand-modified to set the logical stop to 
;;   the end of the first parameter but the terminate time to the sum
;;   of durations.

(set-sound-srate 10.0)
(set-control-srate 10.0)

(defun impulse () (snd-from-array 0.0 *default-sound-srate* (vector 1.0)))

(defun train () (sim (impulse) (at 1.0 (impulse)) (at 2.0 (impulse))))


(s-plot (train))

