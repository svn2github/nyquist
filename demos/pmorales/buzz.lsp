;;; BUZZ generator for Nyquist
;;; Pedro J. Morales. Albacete, Spain. Jule, 2001
;;; pmorales@iele-ab.uclm.es

; tested on Nyquist IDE 3.0 under Windows


; Summation formula taken from F. Richard Moore "Elements of Computer Music"
; section 3.4 page 273
(defun buzz-aux (harm len)
  (let ((frq (/ *sound-srate* len)))
  (scale (/ 1.0 harm)
    (mult (osc (hz-to-step (* (+ 1 harm) 0.5 frq)) (/ 1.0 frq))
          (osc (hz-to-step (* harm 0.5 frq))(/ 1.0 frq))
          (clip (recip (osc (hz-to-step (* 0.5 frq)) (/ 1.0 frq)))
                 10000)))))

; A table implies a constant spectrum.
; If you need another spectrum try to change the number of harmonics
(defun make-buzz-table (harm &optional (len 2047))
  (list (buzz-aux harm len)
        (hz-to-step (/ *sound-srate* len))
        T))

; This function calculates de maximun number of harmonics
; without aliasing
(defun num-harm (pitch)
  (truncate (/ *sound-srate* 2.0 (step-to-hz pitch))))

; Constant frequency buzz oscillator
; Number of harmonics is optional. If it is not
; specified then the waveform is calculated with maximum
; number of harmonics without aliasing
(defun buzz (pitch dur &optional harm)
  (unless harm (setf harm (num-harm pitch)))
  (osc pitch dur (make-buzz-table harm)))

; vibrato buzz
(defun vib-buzz (pitch dur &optional harm)
  (unless harm (setf harm (num-harm pitch)))
  (fmosc pitch (scale 10 (lfo 6 dur)) (make-buzz-table harm)))

; buzz in fm oscillator form
(defun fmbuzz (pitch modulator harm)
  (fmosc pitch modulator (make-buzz-table harm)))

; filter with three formants intended for vowel synthesis
; (this synthesis algorithm may be improved by means of finer
; control of parameters)

(defun formants (beh f1 f2 f3)
  (sim (reson beh f1 100 2)
       (reson beh f2 100 2)
       (reson beh f3 100 2)))

; vowels formants data taken from John R. Pierce "Los sonidos de la Musica"
; (Scientific American, spanish edition)
(defun ah (pitch dur)  ; Hawed foneme
  (mult (pwl 0.2 1 (- dur 0.4) 1 dur)
        (formants (vib-buzz pitch dur) 570 840 2410)))

(defun eh (pitch dur)  ; Head foneme
  (mult (pwl 0.2 1 (- dur 0.4) 1 dur)
        (formants (vib-buzz pitch dur) 530 1840 2480)))

(defun eeh (pitch dur)  ; Heed foneme
  (mult (pwl 0.2 1 (- dur 0.4) 1 dur)
        (formants (vib-buzz pitch dur) 270 2290 3010)))

(defun ooh (pitch dur)  ; Who'd foneme
  (mult (pwl 0.2 1 (- dur 0.4) 1 dur)
        (formants (vib-buzz pitch dur) 300 870 2240)))

; TEST
(defun buzz-test ()
  (play (seq (ah c3 1)(eeh c3 1)(ooh c3 1)
             (ah c2 1)(eeh c2 1)(ooh c2 1)
             (ah c4 1)(eeh c4 1)(ooh c4 1)
             (ah d4 1)(eeh d4 1)(ooh d4 1)
             (ah g4 1)(eeh g4 1)(ooh g4 1)
             (ah c5 1)(eeh c5 1)(ooh c5 1)
             (ah c4 1)(eh  b3 0.5)(ah c4 0.5)
             (eeh e4 1)(eeh d4 1)(ah c4 3))))

(buzz-test)

