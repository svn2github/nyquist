;;; PARTIAL 

(load "pjmg.lsp")

(defun klin (fr coef)
  (mult (sine (hz-to-step (* 300.0 fr coef)) 2.0) (pwev 3.0  3.0 1e-2))) 

(defun klines (coef)
  (sim (at 0.0 (klin 6.0 coef))
       (at 0.3 (klin 7.0 coef))
       (at 0.5 (klin 5.5 coef))
       (at 0.7 (klin 6.5 coef))))

(defun bell ()
  (sim (mult (sine (hz-to-step (* 300.0 (/ 3.14 5))) 6.0) (scale 4.0 (pwe 6.0 1e-2)))
       (mult (sine (hz-to-step 300.0) 6.0) (pwl 2.0 0.75 3.0 1.0 4.0 0.75 5.0 0.2 6.0))
       (mult (sine (hz-to-step (* 300.0 1.57)) 6.0) (pwl 3.0 0.75 4.0 0.5 5.0))
       (mult (sine (hz-to-step (* 300.0 3.14)) 6.0) (pwl 2.5 0.5 4.0))
       (at 0.5 (scale 2.0 (mult (sine (hz-to-step (* 300.0 6.3)) 6.0) (pwe 3.0 5e-3))))
       (at 2.0 (scale 2.0 (mult (sine (hz-to-step (* 300.0 9.12)) 6.0) (pwe 3.0 1e-2))))
       (at 0.7 (scale 2.0 (mult (sine (hz-to-step (* 300.0 15.7)) 6.0) (pwe 4.0 2e-2))))
       (at 3.0 (klines 1.0))
       (at 4.0 (klines 1.5))
       (at 1.0 (mult (sine (hz-to-step (+ (* 300.0 6.3) 20.0)) 6.0)
                     (scale 5e-3 (pwe 2.0 1000.0 4.0)))) 
))

(ss (scale 0.1 (bell))) 
