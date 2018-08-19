; voice.lsp - voice synthesis instrument
; Eduardo Reck Miranda
;
; Implements a geometrical articulator for tongue position (h p) and 
; lips rouding (r) 
;
;---------------------------------------------------------------------------
; Geometrical articulator: the following FORMx functions estimates the formant
; values from the positions of the three articulators p h and r, where:
; p = horizontal position of the tongue: 0.0 = front and 1.0 = back 
; h = vertical position of the tongue: 0.0 = low and 1.0 = high
; r = rounding of the lips: 0.0 = spread -> 1.0 rounded
;---------------------------------------------------------------------------
; FORM1: converts p-h-r articulators to first formant frequency
;---------------------------------------------------------------------------
(defmacro form1 (p h r)
  `(+ (* (+ (* (+ (- 392) (* 392 ,r)) (expt ,h 2))
            (* (- 596 (* 668 ,r)) ,h) 
            (+ (- 146) (* 166 ,r))) 
         (expt ,p 2))
 
      (* (+ (* (- 348    (* 348 ,r)) (expt ,h 2)) 
            (* (+ (- 494) (* 606 ,r)) ,h) 
            (- 141 (* 175 ,r)))
         ,p)
 
      (+ (* (- 340 (* 72 ,r)) (expt ,h 2)) 
         (* (+ (- 796) (* 108 ,r)) ,h)
         (- 708 (* 38 ,r)))
      ))

;---------------------------------------------------------------------------
; FORM2: converts p-h-r articulators to second formant frequency
;---------------------------------------------------------------------------
(defmacro form2 (p h r)
  `(+ (* (+ (* (+ (- 1200) (* 1208 ,r)) (expt ,h 2))
            (* (- 1320 (* 1328 ,r)) ,h) 
            (- 118 (* 158 ,r))) 
         (expt ,p 2))
 
      (* (+ (* (- 1864 (* 1488 ,r)) (expt ,h 2)) 
            (* (+ (- 2644) (* 1510 ,r)) ,h) 
            (+ (- 561) (* 221 ,r)))
         ,p)
 
      (+ (* (+ (- 670) (* 490 ,r)) (expt ,h 2)) 
         (* (- 1355  (* 697 ,r)) ,h)
         (- 1517 (* 117 ,r)))
      ))

;---------------------------------------------------------------------------
; FORM3: converts p-h-r articulators to third formant frequency
;---------------------------------------------------------------------------
(defmacro form3 (p h r)
  `(+ (* (+ (* (- 604 (* 604 ,r)) (expt ,h 2))
            (* (- 1038 (* 1178 ,r)) ,h) 
            (+ 246 (* 566 ,r))) 
         (expt ,p 2))
 
      (* (+ (* (+  (- 1150) (* 1262 ,r)) (expt ,h 2)) 
            (* (+ (- 1443) (* 1313 ,r)) ,h) 
            (- (- 317) (* 483 ,r)))
         ,p)
 
      (+ (* (- 1130 (* 836 ,r)) (expt ,h 2)) 
         (* (+ (- 315)  (* 44 ,r)) ,h)
         (- 2427 (* 127 ,r)))
      ))


;---------------------------------------------------------------------------
; FORM4: converts p-h-r articulators to fourth formant frequency
;---------------------------------------------------------------------------
(defmacro form4 (p h r)
  `(+ (* (+ (* (+ (- 1120) (* 16 ,r)) (expt ,h 2))
            (* (- 1696 (* 180 ,r)) ,h) 
            (+ 500 (* 522 ,r))) 
         (expt ,p 2))
 
      (* (+ (* (+  (- 140) (* 240 ,r)) (expt ,h 2)) 
            (* (+ (- 578) (* 214 ,r)) ,h) 
            (- (- 692) (* 419 ,r)))
         ,p)
 
      (+ (* (- 1480 (* 602 ,r)) (expt ,h 2)) 
         (* (+ (- 1220)  (* 289 ,r)) ,h)
         (- 3678 (* 178 ,r)))
      ))

;---------------------------------------------------------------------------
; ADSR-SMOOTH: a standard ADSR envelope
;---------------------------------------------------------------------------
(defun adsr-smooth (signal dur)
	 (mult signal (env 0.1 0.2 0.5  1.0  0.8  0.4 dur)))
;---------------------------------------------------------------------------
; VIBRATO: generates vibrato
; vib-rate = vibrato rate in Hz
; dur = duration in seconds
;---------------------------------------------------------------------------
(defun vibrato (vib-rate dur)
	(osc (hz-to-step vib-rate) dur))

;---------------------------------------------------------------------------
; PULSE-TABLE: build table for generating a pulse signal
; harm = number of harmonics
;---------------------------------------------------------------------------
(defun pulse-table (harm)
  (abs-env ;prevent any timewarping in the following
    (let ((table (build-harmonic 1 2048)))
      (cond ((> harm 1) ;sum remaining harmonics
		 (setf harm (- harm 1))
             (dotimes (i harm)
               (setf table (sum table (build-harmonic (1+ i) 2048))))))
      table)))

;---------------------------------------------------------------------------
; PULSE-WITH-VIBRATO: generate pulse with vibrato
; step = pitch in steps
; duration = duration in seconds
; vib-rate = vibrato rate in Hz
;---------------------------------------------------------------------------
(defun pulse-with-vibrato (step duration vib-rate)
  (let (harm freq)
    (setf freq (step-to-hz step))
    (setf harm (truncate (/ 22050 (* 2 freq))))
    (setf table (scale (/ 1.0 harm) (pulse-table harm)))
    (fmosc step (vibrato vib-rate duration) (list table (hz-to-step 1) t))))

;---------------------------------------------------------------------------
; VOICING-SOURCE: generate voicing source: pulse with vibrato + LPFs
; step = pitch in steps
; duration = duration in seconds
; vib-rate = vibrato rate in Hz
;---------------------------------------------------------------------------
(defun voicing-source (step duration vib-rate)
	(lp
	  (lp 
	    (pulse-with-vibrato step duration vib-rate) 
		(*  1.414 (* 2 (step-to-hz step)))) 
		(*  1.414 (* 4 (step-to-hz step)))))
	
;---------------------------------------------------------------------------
; NOISE-SOURCE: generate noise source: noise + offset oscillator + LPF
; step = pitch in steps
; duration = duration in seconds
; vib-rate = vibrato rate in Hz
;---------------------------------------------------------------------------
(defun noise-source (step duration vib-rate)
	(lp 
	  (sum 
		(noise duration)
		(fmosc step (vibrato vib-rate duration))) 8000))

;---------------------------------------------------------------------------
; SOURCE: generate source signal: voicing + noise sources
; freq = fundamental frequency in Hz
; duration = duration in seconds
; vib-rate = vibrato rate in Hz
; voicing-scale = percentage of voicing in the resulting signal (0.0 -> 1.0)
; noise-scale = percentage of noise in the resulting signal (0.0 -> 1.0)
;---------------------------------------------------------------------------
(defun source (freq duration vib-rate voicing-scale noise-scale)
	(sum
		(scale voicing-scale (voicing-source (hz-to-step freq) duration vib-rate))
		(scale noise-scale (noise-source (hz-to-step freq) duration vib-rate))))


;---------------------------------------------------------------------------
; MAKE-SPECTRUM: formant filters
; freq = fundamental frequency in Hz
; dur = duration in seconds
; vib-rate = vibrato rate in Hz
; v-scale = amplitude scaling for the voicing source
; n-scale = amplitude scaling for the noise source 
; p = horizontal position of the tongue (0.0 = front -> 1.0 = back) 
; h = vertical position of the tongue (0.0 = low -> 1.0 = high)
; r = rouding of the lips (0.0 = spread -> 1.0 = rounded)
;---------------------------------------------------------------------------
(defun make-spectrum (freq dur vib-rate v-scale n-scale p h r)
	(let ((src (source freq dur vib-rate v-scale n-scale)))
 		(setf spectrum
  			(sim	
   				(reson src (form1 p h r) 50 1)
   				(reson (scale-db (- 10) src) (form2 p h r) 70 1)
   				(reson (scale-db (- 14) src) (form3 p h r) 110 1)
   				(reson (scale-db (- 20) src) (form4 p h r) 250 1)))))
			
;---------------------------------------------------------------------------
; SYNTHESISE: the synthesise function
; Simplified version of the instrument used by the agents discussed in Chapter 6.
; f0 = pitch frequency
; w1 = amplitude of voicing source (min = 0.0 max = 1.0)
; w2 = amplitude of noise source (min = 0.0 max = 1.0)
; a = horizontal position of the tongue (0.0 = front -> 1.0 = back) 
; b = vertical position of the tongue (0.0 = low -> 1.0 = high)
; c = rouding of the lips (0.0 = spread -> 1.0 = rounded)
; fm = vibrato rate (in Hz)
; h = duration in seconds
;---------------------------------------------------------------------------
(defun synthesise (f0 w1 w2 a b c fm h)
	(adsr-smooth (make-spectrum f0 h fm w1 w2 a b c) h))
		
;=== The code for the instrument ends here ===

;---------------------------------------------------------------------------
; Test the SYNTHESISE function with different positions of the articulators
;
; Running steps:
; 1 - run Nyquist
; 2 - load "voice/voice.lsp"
; 3 - type (play (vowel-1)) to synthesise the first test, and so on
;---------------------------------------------------------------------------
(defun vowel-1 ()
	(synthesise 220 1.0 0.005 0.0 0.0 0.0 5.6 1.0))

(defun vowel-2 ()
	(synthesise 220 1.0 0.005 0.0 0.0 1.0 5.6 1.0))

(defun vowel-3 ()
	(synthesise 220 1.0 0.005 0.5 0.0 0.0 5.6 1.0))

(defun vowel-4 ()
	(synthesise 220 1.0 0.005 0.5 0.0 1.0 5.6 1.0))

(defun vowel-5 ()
	(synthesise 220 1.0 0.005 1.0 0.0 0.0 5.6 1.0))

(defun vowel-6 ()
	(synthesise 220 1.0 0.005 1.0 0.0 1.0 5.6 1.0))

(defun vowel-7 ()
	(synthesise 220 1.0 0.005 0.0 0.5 0.0 5.6 1.0))

(defun vowel-8 ()
	(synthesise 220 1.0 0.005 0.0 0.5 1.0 5.6 1.0))

(defun vowel-9 ()
	(synthesise 220 1.0 0.005 0.5 0.5 0.0 5.6 1.0))

(defun vowel-10 ()
	(synthesise 220 1.0 0.005 0.5 0.5 1.0 5.6 1.0))

(defun vowel-11 ()
	(synthesise 220 1.0 0.005 1.0 0.5 0.0 5.6 1.0))

(defun vowel-12 ()
	(synthesise 220 1.0 0.005 1.0 0.5 1.0 5.6 1.0))

(defun vowel-13 ()
	(synthesise 220 1.0 0.005 0.0 1.0 0.0 5.6 1.0))

(defun vowel-14 ()
	(synthesise 220 1.0 0.005 0.0 1.0 1.0 5.6 1.0))

(defun vowel-15 ()
	(synthesise 220 1.0 0.005 0.5 1.0 0.0 5.6 1.0))

(defun vowel-16 ()
	(synthesise 220 1.0 0.005 0.5 1.0 1.0 5.6 1.0))

(defun vowel-17 ()
	(synthesise 220 1.0 0.005 1.0 1.0 0.0 5.6 1.0))

(defun vowel-18 ()
	(synthesise 220 1.0 0.005 1.0 1.0 1.0 5.6 1.0))
 
;; play everything
(defun vowel-n (n) (funcall (intern (format nil "VOWEL-~A" n))))

(defun play-all-vowels ()
  (autonorm-off)
  (dotimes (i 18) (play (scale 20 (vowel-n (1+ i)))))
  (autonorm-on))

; (play-all-vowels) will play everything in sequence
