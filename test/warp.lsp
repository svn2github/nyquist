(defun line () (seqrep (i 20) (pluck1 (+ 24 (random 12)))))

(defun pluck1 (p)
  (display "pluck1" (local-to-global 0) (local-to-global 1))
  (pluck p))

(play (warp (mult (pwl 21 2.5) (pwl 21 2.5)) (line)))

(s-plot (mult (pwl 11 2.5) (pwl 11 2.5)))



