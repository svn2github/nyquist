;;; Simple KARPLUS-STRONG
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es


; NYQUIST code for simple Karplus-Strong algorithm

(load "pjmg.lsp")

(setf ks-class
 (send class :new '(cnt total-cnt z-1output output delay-line len total-len)))

(send ks-class :answer :isnew '(pitch dur)
  '((setf len (round (/ *sound-srate* (step-to-hz pitch))))
    (setf total-len (* *sound-srate* dur))
    (setf delay-line (snd-samples (noise (/ (step-to-hz pitch))) len))
    (setf cnt 0)
    (setf total-cnt 0)
    (setf z-1output 0.0)
    (setf output 0.0)))

(send ks-class :answer :next '()
  '((setf output (aref delay-line cnt))
    (setf (aref delay-line cnt) (/ (+ output z-1output) 2.0))
    (setf z-1output output)
    (setf cnt (if (= (1- len) cnt) 0 (1+ cnt)))
    (setf total-cnt (1+ total-cnt))
    (if (= total-cnt total-len) NIL output)))

(defun ks (pitch dur)
  (let (obj (d (get-duration dur)))
    (setf obj (send ks-class :new pitch d))
    (snd-fromobject *rslt* *sound-srate* obj)))

(defun ks-env (pitch dur)
  (mult (pwe dur 0.064)
        (ks pitch dur)))

;(ss (seq (ks a4 1.0) (ks b4 1.0) (ks c5 3.0)))

(ss (seq (ks-env a3 1.0) (ks-env b3 1.0)))
