;;; ADDITIVE SYNTHESIS
;;; Risset Tibetan
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(defun tibetan-wave ()
  (setf *tibetan-table*
    (sim (scale 0.3 (build-harmonic 1 2048))
         (scale 0.1 (build-harmonic 5 2048))
         (scale 0.1 (build-harmonic 6 2048))
         (scale 0.1 (build-harmonic 7 2048))
         (scale 0.1 (build-harmonic 6 2048))
         (scale 0.1 (build-harmonic 8 2048))
         (scale 0.1 (build-harmonic 9 2048))))
  (setf *tibetan-table* (list *tibetan-table* (hz-to-step 1) T)))

(if (not (boundp '*tibetan-table*)) (tibetan-wave))

(defun tibetan (frq offset dur rise dec)
  (mult (pwl rise 1.0 (- dur dec) 1.0 dur)
    (apply #'sim
      (mapcar #'(lambda (off)
                  (osc (hz-to-step (+ frq (* off offset))) dur *tibetan-table*))
               '(0 1 2 3 4 -1 -2 -3 -4)))))

(play
    (scale 0.1 (vector (sim (at 0.0 (tibetan 110 0.03 35 0.07 21))
                            (at 20.0 (tibetan 110 0.04 20 2 4))
                            (at 28.0 (tibetan 220 0.04 30 3 6))
                            (at 32.1 (tibetan 110 0.03 23 2.3 4.6)))
                       (sim (at 5.0 (tibetan 55  0.02 20 0.04 12))
                            (at 20.0 (tibetan 220 0.05 15  1.5 3))
                            (at 32.0 (tibetan 110 0.025 26 2.6 5.2))
                            (at 36.0 (tibetan 55 0.01 22 0.04 13))))))
