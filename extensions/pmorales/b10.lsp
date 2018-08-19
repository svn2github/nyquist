;;; ADDITIVE SYNTHESIS
;;; Sinus Chaos
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(setf *pmorales-path* (current-path))
(load (strcat *pmorales-path* "pjmg.lsp"))

(defun env31 ()
  (pwlv 0.99 25e-3 0.99 225e-3 0.318 275e-3 0.318 475e-3 0.99 500e-3 0.99))

(defun env32 ()
  (pwlv 0.377 250e-3 0.99 500e-3 0.377))

(defun env33 ()
  (pwlv 0.5 20e-3 0.5 225e-3 0.99 250e-3 0.99 480e-3 0.5 500e-3 0.5))

(defun env34 ()
  (pwlv 0.333 25e-3 0.333 225e-3 0.999 275e-3 0.999 475e-3 0.333 500e-3 0.333))

(defun make-env31 ()
  (setf *env31* (list (env31) (hz-to-step 2) T)))

(defun make-env32 ()
  (setf *env32* (list (env32) (hz-to-step 2) T)))

(defun make-env33 ()
  (setf *env33* (list (env33) (hz-to-step 2) T)))

(defun make-env34 ()
  (setf *env34* (list (env34) (hz-to-step 2) T)))

(if (not (boundp '*env31*)) (make-env31))
(if (not (boundp '*env32*)) (make-env32))
(if (not (boundp '*env33*)) (make-env33))
(if (not (boundp '*env34*)) (make-env34))

(defun make-table12 ()
  (setf *table12* (sim (build-harmonic 21.0 2048)
                       (build-harmonic 29.0 2048)
                       (build-harmonic 39.0 2048)))
  (setf *table12* (list *table12* (hz-to-step 1) T)))

(if (not (boundp '*table12*)) (make-table12))


(defun chaos-partial (amp rate frq dur env &optional (table *table*))
  (scale amp (fmosc (hz-to-step 1e-3)
                    (scale frq (osc (hz-to-step rate) dur env)) table)))

(defun partial2 (amp frandi rate frq dur env)
  (mult (randi1 frandi dur)
        (scale amp (fmosc (hz-to-step 1e-3)
                          (scale frq (osc (hz-to-step rate) dur env))))))

(ss
 (sim
   (chaos-partial 4.5 0.12 880.0  24 *env31*)
   (partial2      4.0 200.0 0.17  1660.0 24 *env32*)
   (chaos-partial 1.2 0.05 200.0  24 *env33*)
   (chaos-partial 0.7 0.33 2400.0 24 *env34*)

  ))
