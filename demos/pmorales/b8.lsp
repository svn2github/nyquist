;;; ADDITIVE SYNTHESIS
;;; Risset Drum
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(defun drum-env (amp dur)
  (pwev amp dur (* 12e-5 amp)))

(defun noise-component (amp dur central-frq noise-frq)
  (amosc (hz-to-step central-frq)
         (mult (drum-env (/ amp 2) dur)
               (randi1 noise-frq dur))))

(defun fund-component (amp dur frq)
  (amosc (hz-to-step frq)
         (drum-env (/ amp 2.5) dur)))

(defun drum-inh-wave ()
  (setf *drum-inh-table*
    (sim (build-harmonic 10 2048)
         (scale 1.5 (build-harmonic 16 2048))
         (scale 2.0 (build-harmonic 22 2048))
         (scale 1.5 (build-harmonic 23 2048))))
  (setf *drum-inh-table* (list *drum-inh-table* (hz-to-step 1) T)))

(if (not (boundp '*drum-inh-table*)) (drum-inh-wave))

(defun inh-component (amp dur frq)
  (amosc (hz-to-step (/ frq 10))
         (drum-env (/ amp 6.0) dur)
         *drum-inh-table*))
         
(defun risset-drum (amp dur frq)
  (sim (noise-component amp dur 500 400)
       (inh-component amp dur frq)
       (fund-component amp dur frq)))

(ss (sim
    (at 0.0 (risset-drum 1.0 3.0 100.0))
    (at 0.5 (risset-drum 1.0 1.0 50.0))
    (at 1.0 (risset-drum 1.0 1.0 75.0))
    (at 1.2 (risset-drum 1.0 1.0 200.0))
    (at 1.4 (risset-drum 1.0 3.0 300.0))
    (at 1.8 (risset-drum 1.0 6.0 500.0))))
