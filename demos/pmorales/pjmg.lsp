;;; PJMG.LSP
;;; Rutinas para Nyquist

; Some utilities and functions not defined in
; the released version of Nyquist


(defun set-current-file (cf)
  (setf *CURRENT-FILE* cf))
  
(defun l () (load *CURRENT-FILE*))

;; A comment by Dannenberg on the following function:
;; This function takes an expression for a sound and 
;; finds its peak value. This forces a computation of all
;; samples, which are saved in memory (4 bytes per sample).
;; The samples are then normalized and written to a file.
;; This should be fine for short examples, but is not 
;; recommended for general use because you may run out 
;; of memory. See the manual for more notes on normalization.
;;
(defun ss (m) 
  (let ((m-max (peak m NY:ALL)))
    (s-save (scale (/ 1.0 m-max) m) NY:ALL *default-sound-file* 
                 :play *soundenable*))) 

(defun randi1 (fr dur)
  (let ((d (get-duration dur)))
       (snd-white *rslt* fr d)))

(defun randi2 (fr dur)
  (at 0.0 (snd-white 0.0 fr dur)))

(defun randh1 (fr dur)
  (let ((d (get-duration dur)))
   (snd-compose (noise d) (quantize (ramp d) (round (* fr d))))))

(defun rndh2 (fr dur)
  (at 0.0 (snd-compose (noise dur)
                       (quantize (ramp dur) (round (* fr dur))))))
