;;; ADDITIVE SYNTHESIS
;;; Continuous pitch control by LFO
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(setf *pmorales-path* (current-path))
(load (strcat *pmorales-path* "pjmg.lsp"))

(defun lfo-pitch-control ()
  (pwlv 0.25 0.6 0.25 1.4 0.5 2.0 0.25 2.2 0.25 3.4 0.5 3.8 0.75 7.0 -0.2))

(defun starship (frq scl)
  (apply #'sim
         (mapcar #'(lambda (offset)
                     (fmosc (hz-to-step (+ frq offset))
                            (scale scl (lfo-pitch-control))))
                 '(0.0 4.5 9.4 23.0 39.0 84.0))))

(defun starship-demo () (ss (starship 200.0 1000.0)) )
