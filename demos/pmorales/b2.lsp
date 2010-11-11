;;; ADDITIVE SYNTHESIS
;;; Risset's Spectral Analysis of a Chord
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(setf *pmorales-path* (current-path))
(load (strcat *pmorales-path* "pjmg.lsp"))

; Probar con las dos envolventes
;(defun sac-env (dur)
;  (let ((xx (pwl (/ dur 2) 1.0 dur)))
;    (mult xx xx)))

(defun sac-env (dur)
  (pwev 1.0 dur 5e-2))

(defun attk-list (offset num-harms &optional (out-list (list 0.0)))
  (if (= num-harms 0) (reverse out-list)
      (attk-list offset (1- num-harms) (cons (+ offset (car out-list)) out-list))))

  
(defun sac (frq dur offset-entry num-harm)
    (mapcar #'(lambda (xhrm xoff)
                (at xoff (amosc (hz-to-step (* (step-to-hz frq) xhrm)) (sac-env dur))))
           (attk-list -1 (1- num-harm) (list num-harm))
           (attk-list offset-entry (1- num-harm))))

(defun sac-left-right (l)
  (do* ((i 0 (1+ i))
        (left () (if (evenp i) (cons (nth i l) left) left))
        (right () (if (oddp i) (cons (nth i l) right) right)))
       ((= i  (1- (length l))) (vector (apply #'sim left) (apply #'sim right))))) 

(defun st-sac (frq dur offset-entry num-harm)
  (sac-left-right (sac frq dur offset-entry (1+ num-harm))))

(defun st-sac-sequence ()
  (scale 0.17 (sim (at 0.0 (st-sac as6 7.5 2.5 5))
                   (at 0.01 (st-sac b5 7.5 2.5 5))
                   (at 3.75 (st-sac e5 3.75 1.25 9))
                   (at 3.76 (st-sac g5 3.75 1.25 9))
                   (at 5.5 (st-sac d4 2 1.0 11))
                   (at 5.51 (st-sac gs3 2 1.0 11)))))

(defun st-sac-demo () (ss (st-sac-sequence)))
