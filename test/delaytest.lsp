; test delay using seq and s-rest

(setf *vc-score* '((0 1 (pluck c4))))

(defun long-delay (in)
  (sum in (seq (s-rest 0.3) (cue in))))

;(play (timed-seq *vc-score*))

;(play (long-delay (timed-seq *vc-score*)))

;(play (sim (timed-seq *vc-score*) (at 2 (timed-seq *vc-score*))))
