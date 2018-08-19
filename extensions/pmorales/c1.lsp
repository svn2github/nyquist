;;; ADDITIVE SYNTHESIS
;;; Random Signals
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(setf *pmorales-path* (current-path))
(load (strcat *pmorales-path* "pjmg.lsp"))


(defun simple-noise ()
  (mult (noise 4.0)
        (pwl 0.4 1.0 3.6 1.0 4.0)))

(defun simple-randi (frandi)
  (mult (randi1 frandi 4.0)
        (pwl 0.4 1.0 3.6 1.0 4.0)))

(defun tenney (frandi frq dur)
  (amosc (hz-to-step frq)
         (mult (randi1 frandi dur) (pwl 0.4 1.0 (- dur 0.4) 1.0 dur))))



;(ss (seq (simple-noise) (simple-randi 200) (simple-randi 400)))

(defun tenny-sequence ()
  (seq (tenney 200.0 400.0 4.0)
       (tenney 800.0 300.0 2.0)
       (tenney 400.0 1600.0 4.0)))

(defun tenny-demo () (ss (tenny-sequence)))

