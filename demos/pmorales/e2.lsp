;;; FM
;;; Chowning Dynamic Spectral Evolution
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

;;; WARNING: needs REVERB.LSP

(load "pjmg.lsp")

; Chowning BELL -----------------------------------------------------
(defun exp-env (amp dur)
  (scale amp (pwe dur 64e-4)))

(defun fm-bell (frq cm-ratio imax dur amp)
  (mult (exp-env amp dur)
        (fmosc (hz-to-step frq)
               (mult (exp-env (* imax (/ frq cm-ratio)) dur)
                     (osc (hz-to-step (/ frq cm-ratio)) dur)))))

; Chowning WOOD-DRUM ------------------------------------------------
(defun wood-env (amp dur)
  (scale amp (pwev 0.8 (* dur 0.2) 1.0 (* dur 0.25) 1.0 dur 64e-4)))

(defun wood-mod-env  (amp dur)
  (scale amp (pwlv 1.0 (* 0.2 dur) 0.0 dur)))
  
(defun fm-wood-drum (frq cm-ratio imax dur amp)
  (mult (wood-env amp dur)
        (fmosc (hz-to-step frq)
               (mult (wood-mod-env (* imax (/ frq cm-ratio)) dur)
                     (osc (hz-to-step (/ frq cm-ratio)) dur)))))

; Chowning BRASS ----------------------------------------------------
(defun brass-env (amp dur)
  (scale amp (pwl 0.1 1.0 0.2 0.8 (- dur 0.1) 0.7 dur)))

(defun fm-brass (pitch cm-ratio imax dur amp)
  (let ((frq (step-to-hz pitch)))
    (mult (brass-env amp dur)
          (fmosc pitch
                 (mult (brass-env (* imax (/ frq cm-ratio)) dur)
                       (osc (hz-to-step (/ frq cm-ratio)) dur))))))

; Chowning CLARINET -------------------------------------------------
(defun clar-env (amp dur)
  (scale amp (pwev 64e-4 0.1 1.0 (- dur 0.1) 1.0 dur 64e-4)))

(defun clar-mod-env (vmax vmin dur)
  (pwev vmax (* dur 0.3) vmin dur vmin))

(defun fm-clar (pitch cm-ratio imax imin dur amp)
  (let ((frq (step-to-hz pitch)))
    (mult (clar-env amp dur)
          (fmosc pitch
                 (mult (scale (/ frq cm-ratio) (clar-mod-env imax imin dur))
                       (osc  (hz-to-step (/ frq cm-ratio)) dur))))))
                       
; Chowning VARIABLE FM INSTRUMENT -----------------------------------
; este instrumento hay que mejorarlo

; load REVERB
(if (not (boundp '*REVERB*))
  (prog ()
    (print "Loading REVERB..")
    (load "reverb")
    (setfn tone lp)
    (setf *REVERB* T)))

    
(defun variable-fm-env (amp break mid dur)
  (scale amp (pwev (* mid 64e-4) break mid dur (* mid 64e-4))))
  
(defun variable-mod-osc (index frq kdyn mid break dur)
  (amosc (hz-to-step frq)
         (scale (* index frq) (pwlv kdyn break mid dur))))

(defun variable-fm-inst (amp break mid dur kdyn index frq cm-ratio)
  (mult (variable-fm-env amp break mid dur)
        (fmosc (hz-to-step frq)
               (variable-mod-osc index (/ frq cm-ratio) kdyn mid break dur))))


(defun variable-fm-rev-st (amp break mid dur kdyn index frq cm-ratio coefrev)
  (let ((snd (variable-fm-inst amp break mid dur kdyn index frq cm-ratio)))
    (sim (cue snd) (scale coefrev (reverb snd dur)))))

;(ss (seq (fm-bell 100.0 (/ 5.0 7.0) 10 10 1.0)
;         (fm-bell 150.0 (/ 5.0 7.0) 7  10 1.0)
;         (fm-bell 200.0 (/ 5.0 7.0) 15 7 1.0)))

(defun fm-w-d (pitch)
  (fm-wood-drum (step-to-hz pitch) (/ 16.0 11.0) 25 0.2 1.0))

;(ss (seq (fm-w-d a2) (fm-w-d b2) (fm-w-d c3) (fm-w-d d3) (fm-w-d e3)
;         (fm-w-d f3) (fm-w-d g3) (fm-w-d a3)
;         (fm-w-d a1) (fm-w-d b1) (fm-w-d c2) (fm-w-d d2) (fm-w-d e2)
;         (fm-w-d f2) (fm-w-d g2) (fm-w-d a2)))

(defun fm-br (pitch)
  (fm-brass pitch 1.0 5 0.6 1.0))
  
;(ss (seq (fm-br c4) (fm-br d4) (fm-br e4) (fm-br f4) (fm-br g4)
;         (fm-br a4) (fm-br b4) (fm-br c5)))

(defun fm-c (pitch)
  (fm-clar pitch (/ 3.0 2.0) 5 2 0.5 1.0))

;(ss (seq (fm-c c5) (fm-c d5) (fm-c e5) (fm-c f5) (fm-c g5)
;         (fm-c a5) (fm-c b5) (fm-c c6)))

(defun v-fm (pitch break mid dur rev)
  (variable-fm-rev-st 1.0 break mid dur 0.8 20.0 (step-to-hz pitch) (/ 7.0 5.0) rev))

;(ss (sim (at 0.0 (v-fm a4 0.7 0.2 3.0 0.5))
;         (at 1.5 (v-fm e6 0.2 0.3 3.0 0.4))
;         (at 3.0 (v-fm d5 2.0 0.6 4.0 0.4))
;         (at 6.0 (v-fm d6 0.01 0.7 3.0 0.5))))

; Double Carrier Brass ----------------------------------------------

(defun dc-env (dur)
  (pwl (* dur 0.1) 1.0 (* dur 0.2) 0.8 (* dur 0.9) 0.7 dur))

(defun dc-modulator (frq dur imax imin)
  (amosc (hz-to-step frq)
         (sim (scale (* frq (- imax imin)) (dc-env dur))
              (const (* frq imin) dur))))

(defun dc-fm1 (frq1 dur amp modulator)
  (scale amp
    (mult (dc-env dur)  
          (fmosc (hz-to-step frq1) modulator))))

(defun dc-fm2 (frq1 dur cm-ratio index-ratio amp amp-ratio modulator)
  (scale (* amp amp-ratio)
    (mult (dc-env dur)
         (fmosc (hz-to-step (/ frq1 cm-ratio))
                (scale index-ratio modulator)))))

(defun double-carrier (dur frq cm-ratio amp amp-ratio imax imin index-ratio)
  (let ((modulator (dc-modulator (/ frq cm-ratio) dur imax imin)))
    (sim (dc-fm1 frq dur amp modulator)
         (dc-fm2 frq dur cm-ratio index-ratio amp amp-ratio modulator))))

;(ss (double-carrier 0.6 440.0 1.0 1.0 0.5 3 1 (/ 3.0 1.5)))

; Double Carrier Trumpet --------------------------------------------

(defun port-env (dur)
  (pwlv -1.0 (* 0.25 dur) 0.1 (* 0.5 dur) 0.0 dur))


(ss (seq (fm-bell 100.0 (/ 5.0 7.0) 10 10 1.0)
         (fm-bell 150.0 (/ 5.0 7.0) 7  10 1.0)
         (fm-bell 200.0 (/ 5.0 7.0) 15 7 1.0)
         (fm-w-d a2) (fm-w-d b2) (fm-w-d c3) (fm-w-d d3) (fm-w-d e3)
         (fm-w-d f3) (fm-w-d g3) (fm-w-d a3)
         (fm-w-d a1) (fm-w-d b1) (fm-w-d c2) (fm-w-d d2) (fm-w-d e2)
         (fm-w-d f2) (fm-w-d g2) (fm-w-d a2)
         (fm-br c4) (fm-br d4) (fm-br e4) (fm-br f4) (fm-br g4)
         (fm-br a4) (fm-br b4) (fm-br c5)
         (double-carrier 0.6 440.0 1.0 1.0 0.5 3 1 (/ 3.0 1.5))))
