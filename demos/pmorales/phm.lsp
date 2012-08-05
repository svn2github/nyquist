;;; DSP in Nyquist
;;; Flute Physical Modelling
;;; Based on Nicky Hind CLM Tutorial
;;; (Based on Perry Cook Flute Physical Modelling)
;;; Coded by Pedro J. Morales
;;; e-mail: pmorales @iele-ab.uclm.es

(load "pjmg.lsp")

;; DELAY LINE

(setf dl-class (send class :new '(cnt line len output)))

(send dl-class :answer :isnew '(init-len)
  '((setf cnt 0)
    (setf len init-len)
    (setf line (make-array len))
    (dotimes (i len) (setf (aref line i) 0.0))))

(send dl-class :answer :next '(val)
  '((setf output (aref line cnt))
    (setf (aref line cnt) val)
    (setf cnt (if (= cnt (1- len)) 0 (1+ cnt)))
    output))

(defun make-delay-line (len)
  (send dl-class :new len))

(defun delay-line (dl-obj val)
  (send dl-obj :next val))

; UNA EXCITACION

(defun flute-exc (noise-lev vib-amount vib-rate atk dec dur)
  (let ((current-flow (sim (pwl atk 0.55 (- dur dec) 0.55 dur) ;puede variar 0.5 .. 0.8
                           (scale vib-amount (lfo vib-rate dur)))))
    (sim current-flow
         (scale noise-lev (mult current-flow (lp (noise dur) (/ *sound-srate* 2.0)))))))

;; FLUTE PHYSICAL MODELLING
(setf flute-class (send class :new '(sum1 sum1-output freq dur bore-delay emb-delay
                                     period-samples out-sig last-sig current-bore)))

(defun cubic-polynomial (x) (- x (expt x 3.0)))

(send flute-class :answer :isnew '(exc emb-size ifreq idur)
  '((setf sum1 exc)
    (setf freq (step-to-hz ifreq))
    (setf period-samples (round (/ *sound-srate* freq)))
    (setf bore-delay (make-delay-line period-samples))
    (setf emb-delay (make-delay-line (round (* emb-size period-samples))))
    (setf last-sig 0.0)))

(send flute-class :answer :next '()
  '((setf sum1-output (snd-fetch sum1))
    (when sum1-output
      (progn
        (setf current-bore (delay-line bore-delay last-sig))
        (setf out-sig
          (+ (* 0.7 (+ (* 0.55 current-bore)
                       (cubic-polynomial (delay-line emb-delay (+ sum1-output
                                                                (* 0.5 current-bore))))))
             (* 0.3 last-sig)))
        (setf last-sig out-sig)))))
       

(defun pm-flute (freq dur &key (noise-lev 0.0356) (atk 0.05) (dec 0.1) (emb-size 0.5)
                            (vib-amount 0.015) (vib-rate 5))
  (let (obj)
    (setf obj (send flute-class :new
                (flute-exc noise-lev vib-amount vib-rate atk dec dur) emb-size freq dur))
    (hp (snd-fromobject 0.0 *sound-srate* obj) 20.0)))

(ss (seq (pm-flute a4 0.5 :dec 0.01)
         (pm-flute b4 0.5 :dec 0.01)
         (pm-flute c5 0.5 :dec 0.01)
         (pm-flute gs4 1.0))) 
 

