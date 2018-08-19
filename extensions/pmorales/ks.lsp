;;; DSP in Nyquist
;;; Karplus-Strong Algorithm
;;; Coded by Pedro J. Morales.
;;; e-mail: pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(setf ks-class (send class :new
                  '(cnt total-cnt last-output current-output string len total-len)))

(send ks-class :answer :isnew '(pitch dur)
  '((setf len (round (/ *sound-srate* (step-to-hz pitch))))
    (setf total-len (* *sound-srate* dur))
    (setf string (snd-samples (noise (/ (step-to-hz pitch))) len))
    (setf cnt 0)
    (setf total-cnt 0)
    (setf last-output 0.0)
    (setf current-output 0.0)))

(send ks-class :answer :next '()
  '((setf current-output (aref string cnt))
    (setf (aref string cnt) (/ (+ current-output last-output) 2.0))
    (setf last-output current-output)
    (setf cnt (if (= (1- len) cnt) 0 (1+ cnt)))
    (setf total-cnt (1+ total-cnt))
    (if (= total-cnt total-len) NIL current-output)))

(defun ks (pitch dur)
  (let (obj)
    (setf obj (send ks-class :new pitch dur))
    (snd-fromobject (local-to-global 0.0) *sound-srate* obj)))

(ss (seq (ks e2 2)(ks a2 2)(ks d3 2)(ks g3 2)(ks b3 2)(ks e4 2)))
