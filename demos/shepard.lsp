; Shepard tones and paradoxes

; to use try
; (playscale (majorscale 60))
; (playscale (minorscale 60))
; (playscale (chromascale 60))
; (playparadoxscale (chromascale 60))

; for shepard sweeps, try
; (play (sheptone-sweep 60 60 2 72 60 12 4))

; the signature of sheptone-sweep should tell what the parameters do
; (defun sheptone-sweep (pitch-1 centerpitch-1 duration pitch-2 centerpitch-2
;                     overtonesemi overtones
;                     &optional (wavetable *sine-table*))

; Some notes about how this works:
; Shepard tones consist of harmonics that are an octave apart, thus
; the ratios are 1, 2, 4, 8, 16, etc. Note that the pitch is ambiguous
; in the sense that there could be a missing fundamental at 0.5, 0.25, etc.
; The other trick is that the spectral shape is constant. The amplitude
; of each harmonic is a function of its absolute frequency. Here, the
; shape is triangular so that as the frequency sweeps upward, harmonics
; (which are ramping up in frequency) fade in, reach a maximum, and fade out.
; 
; In this implementation, each harmonic is generated using an FM oscillator
; controlled by a frequency ramp. The harmonic is multiplied by an envelope
; to implement the spectral shape function. The envelope is computed by
; running the frequency control (with some scaling) into a SHAPE function
; that uses a triangular table to implement the spectral shape.
;
; Warning: Although I have not analyzed this code too carefully, I (RBD)
; believe that the oscillators keep sweeping up to higher and higher
; frequencies even after the amplitude drops to zero. This is not only
; wasteful, but when oscillators start to alias, they run slower. If you
; generate a very long Shepard tone with harmonics spanning many octaves,
; the run time could get to be very large. A better implementation would
; start the harmonics when they enter the non-zero part of the spectral
; envelope and end them when they leave it. 


(setf *onepi* 3.141592654)
(setf *twopi* (* 2 pi))
(setf *halfpi* (/ pi 2))


; envshaper is a raised cosine curve used to control
; the spectral shape. Its domain is 0 to 2
; it transforms (0 2) into 0 1
; it has to be used like
; (shape s (envshaper) 1)

(defun envshaper ()
  (mult (sum 1 (hzosc (const (/ 1.0 2.0) 2) *table* 270)) 0.5))


; some utility functions

;; ISEQ-HELPER -- generates an integer sequence
(defun iseq-helper (a b)
  (let ((mylist '()))
    (dotimes (i (1+ (- b a)) (reverse mylist))
	     (setf mylist (cons (+ a i) mylist)))))

;; ISEQ -- sequence of integers from a to b
(defun iseq (a b)
  (if (> a b) (reverse (iseq-helper b a))
              (iseq-helper a b)))


(defun floor (x)
       (if (< x 0)
           (1- (truncate x))
           (truncate x)))



; the main part

(defun sheptone-sweep-helper (pitch-1 centerpitch-1
                             duration
                             pitch-2 centerpitch-2
                             overtonesemi overtones
			     &optional (wavetable *sine-table*))
  (let ((mytone (const 0 duration))
	(maxovertones (+ (floor (/ (float (max (abs (- pitch-1 centerpitch-2))
					       (abs (- pitch-1 centerpitch-2))))
				   overtonesemi))
			 overtones 2))
	(ampshaper (envshaper)))
    ;; synthesize and sum maxovertones partials
    (dolist (i (iseq (-  maxovertones) maxovertones) mytone)
	    (progn
	      ;; partials start at pitch-1, spaced by overtonesemi (normally 12)
	      (setf startpitch (+ pitch-1 (* i overtonesemi)))
	      ;; partials end at pitch-2 + offset
	      (setf endpitch (+ pitch-2 (* i overtonesemi)))
	      ;; f is the frequency modulation (in hz)
	      (setf f (pwe 0 (step-to-hz startpitch)
			   duration (step-to-hz endpitch)))
	      ;; p is the pitch in steps
	      (setf p (pwl 0 startpitch duration endpitch))
	      ;; c is the centerpitch curve
	      ;;   (probably we could compute this outside the loop)
	      (setf c (pwl 0 centerpitch-1 duration centerpitch-2))
	      ;; normwidthfactor is used to map pitch curves into the spectral shape
	      ;;  function (range 0 to 2)
	      (setf normwidthfactor (/ 1.0 (* overtones overtonesemi)))
	      ;; a is the amplitude envelope: f(p - c)
	      (setf a (shape (mult (diff p c) normwidthfactor)
			     ampshaper 1))
	      ;; voice is one partial
	      (setf voice  (mult a (hzosc f wavetable)))
	      ;; sum the partials into mytone
	      (setf mytone (sum mytone voice))
	      )
	    )))


(defun sheptone-sweep (pitch-1 centerpitch-1 duration pitch-2 centerpitch-2
			       overtonesemi overtones
			       &optional (wavetable *sine-table*))
  (normalize ;; note: you might not want to normalize as is done here
   ;; use an envelope to get a smooth start and stop
   (mult (sheptone-sweep-helper pitch-1  centerpitch-1
				duration
				pitch-2 centerpitch-2
				overtonesemi overtones wavetable)
	 (env 0.05 0 0.05 1 1 1 duration))))


;; SHEPTONE is a special case of  sheptone-sweep. 
;;   The spectral centroid and pitch is constant.
(defun sheptone (pitch centerpitch duration
		       overtonesemi overtones
		       &optional (wavetable *sine-table*))
  (sheptone-sweep pitch centerpitch duration pitch centerpitch
		  overtonesemi overtones
		  wavetable))

(defun majorscale (basepitch)
  (mapcar (lambda (x) (+ basepitch x)) '(0 2 4 5 7 9 11 12)))

(defun minorscale (basepitch)
  (mapcar (lambda (x) (+ basepitch x)) '(0 2 3 5 7 8 10 12)))

(defun chromascale (basepitch)
  (mapcar (lambda (x) (+ basepitch x)) (iseq 0 12)))


;; MAKE-TABLE turns a function of 0-1 into a lookup table
(defun make-table (func-exp points)
  (let ((table (make-array points)))
    (dotimes (i points)
	     (setf (aref table i)
		   (funcall func-exp (/ (float i) (float points)))))
    (list (snd-from-array 0.0 points table) (hz-to-step 1) T)
    ))


(defun erich-wave (skew)
  (make-table
   (lambda (x) (if (< (abs skew) 0.000001) (sin (* *twopi* x))
		 (*
		  (/ (sin (* *twopi* x)) (- (/ 1.0 skew)
					    (cos (* *twopi* x))))
		  (/ (sqrt (- 1.0 (* skew skew))) skew))))
   2048))


;; NORMALIZE -- normalize a sound
;;
(defun normalize (s &optional (maxvol 0.8) (maxlen  44100))
  (let* ((mysound s)
	 (vol (peak mysound maxlen)))
    (scale (/ (float maxvol) vol) mysound)))

(defun playsafe (s)
  (play (normalize s)))

;; PLAYSCALE uses SHEPTONE to synthesize a scale that goes up on every
;;  step, but never actually ends up an octave higher
;;
(defun playscale (scaleseq  &optional (duration 1)  (wavetable *sine-table*))
  (mapcar (lambda (x) (play (sheptone x 60 duration 12 4 wavetable)))
	  scaleseq))


;; PLAYPARADOXSCALE uses sheptone to go up by half steps, yet end up
;;   an octave lower than it starts
;;
(defun playparadoxscale (scaleseq
			 &optional (duration 1) (wavetable *sine-table*))
  (mapcar (lambda (x y) (play (sheptone x y duration 12 4 wavetable)))
	  scaleseq (reverse scaleseq)))
