(defun gate-test ()
 (setf yy (gate xx 1.0 0.1 2.0 0.1 0.5))
 (s-plot yy))
 
(set-control-srate 100)
(set-sound-srate 100)

(setf xx (pwl 0 1 0.1 0 3 0 3 1 4 0 5))

(setf zz (pwl 0 1 0.02 0 1.99 0 2.0 1 2.01 0 2.09 0 2.1 1 2.11 0 
              2.99 0 3 1 3.01 0 3.09 0 3.1 1 3.11 0 3.19 0 3.2 1 3.21 0 5))

(defun noise-gate (snd &optional (lookahead 0.5) (risetime 0.02) (falltime 0.5)
                                                 (floor 0.01) (threshold 0.01))
  (let ((rms (lp (mult snd snd) (/ *control-srate* 10.0))))
    (setf save-rms rms)
    (setf threshold (* threshold threshold))
    (mult snd (gate rms lookahead risetime falltime floor threshold))))
    
(defun ngtest ()
  (setf xx (mult (stretch 5 (lfo 40)) (pwl 0 1 0.5 0 2 0 2 1 2.5 1 2.5 0 5)))
  (setf xx (sum xx (scale 0.01 (stretch 5 (noise)))))
  (setf yy (noise-gate xx))
  (s-plot (vector xx yy)))
