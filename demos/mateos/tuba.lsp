;; By Daniel Mateos - Feb 2005
;; This FM sound is similar to that of the tuba.
;; Therefore, intended mainly for low register.
;; It is built upon a FM 8 parallel oscillator circuit.
;; The attack will remain constant whatever the enviroment.
;; Also, it is properly prepared for transposition.
;; Modified by Roger Dannenberg, Nov 2005

;; Variable amplitude & frequency oscillator
(defun tuba-osc (amp freq)
  (mult amp (hzosc freq)))


;; Parallel 8 oscillators FM
(defun tuba-eight-osc (acar fcar amod1 fmod1 amod2 fmod2 amod3 fmod3
         amod4 fmod4 amod5 fmod5 amod6 fmod6 amod7 fmod7 amod8 fmod8)
  (tuba-osc acar (sim fcar
		      (tuba-osc amod1 fmod1) 
		      (tuba-osc amod2 fmod2) 
		      (tuba-osc amod3 fmod3)
		      (tuba-osc amod4 fmod4) 
		      (tuba-osc amod5 fmod5) 
		      (tuba-osc amod6 fmod6)
		      (tuba-osc amod7 fmod7) 
		      (tuba-osc amod8 fmod8))))


;; Define amplitude envelope of each modulator
(defun tuba-amod (numb)
 (seq
   (stretch-abs 1 (pwl (/ numb 10) 100))
   (pwl 0 100 (- 1 (/ numb 10)))))


;; Defines Amplitud envelope of carrier
(defun tuba-acar ()
 (seq
  (stretch-abs 1 (pwl 0.1 0.8))
  (pwl 0 0.8 0.7 0.8 0.9)))


;; Defines frequency of each modulator
(defun tuba-fmod (numb fcar)
 (case numb
  (1 (- fcar 2))
  (2 (- (* fcar 4) 3))
  (3 (- (* fcar 3) 2))
  (4 (- (* fcar 5) 2))
  (5 (- (* fcar 6) 2))
  (6 (- (* fcar 7) 2))
  (7 (- (* fcar 8) 2))
  (8 (- (* fcar 9) 2))))


;; DMHM-TUBA -- a tuba-like FM sound
;;
;; named dmhm-tuba to avoid name conflicts with other (possible) tubas
;;
(defun dmhm-tuba (fcar)
  (setf transp (float (get-transpose)))
  (cond ((> transp 0.0) 
	 (setf fcar (float fcar)) 
	 (setf fcar (* fcar (expt 2.0 (/ transp 12.0)))) ))
  (cond ((< transp 0.0)
	 (setf transp (* -1.0 transp))
	 (setf fcar (float fcar)) 
	 (setf fcar (/ fcar (expt 2.0 (/ transp 12.0)))) ))
  (scale 0.8 ; normalize
    (lp
      (hp
        (transpose-abs 0 
          (tuba-eight-osc (tuba-acar) fcar
			  (tuba-amod 1) (tuba-fmod 1 fcar)
			  (tuba-amod 2) (tuba-fmod 2 fcar)
			  (tuba-amod 3) (tuba-fmod 3 fcar)
			  (tuba-amod 4) (tuba-fmod 4 fcar)
			  (tuba-amod 5) (tuba-fmod 5 fcar)
			  (tuba-amod 6) (tuba-fmod 6 fcar)
			  (tuba-amod 7) (tuba-fmod 7 fcar)
			  (tuba-amod 8) (tuba-fmod 8 fcar)) )
	10)
      22000)))

;; DMHM-TUBA-TEST -- play a sample of dm-tuba instrument
;;
(defun dmhm-tuba-test ()
  (autonorm-off)
  (play
   (seq
    (dm-tuba 70 )
    (stretch 5 (dm-tuba 70))
    (loud -10 (dm-tuba 70))
    (transpose -10 (stretch 3 (dm-tuba 70))))))

