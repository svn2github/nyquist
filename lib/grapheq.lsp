; basic 4 band equalizer with cuts at 4k, 2k, 1k, and 630 hertz.
; designed as a test
;(defun 4band (s f630 f1k f2k f4k) 
;  (eq-band 
;   (eq-band 
;    (eq-band 
;     (eq-band s 
;	      630 f630 0.33) 
;     1000 f1k 0.33) 
;    2000 f2k 0.33)
;   4000 f4k 0.33))


;; inf-const -- stretch a number into an "infinite" signal
;; Nyquist does not have infinite signals, so just make it very long
;; ny:all is a large number of samples, so use it as the length in samples
;;
(defun inf-const (x)
  (stretch-abs (/ ny:all *default-sound-srate*)
               (const x)))

; n-band graphic eq with variable range.
; sig - input signal
; gains - vector of gain changes in dB
; lowf - lower eq band limit
; highf - upper eq band limit
(defun nband-range (sig gains lowf highf)
  (let ((bandsep ;; bandwidth of each channel in steps
	     (/ (- (hz-to-step highf) (hz-to-step lowf)) (length gains)))
	    lowstep ;; low frequency in steps
	    (newsnd sig)
        (chans (length gains)))
    (setf lowstep (+ (hz-to-step lowf) (* 0.5 bandsep)))
    (cond ((< bandsep 0)
	       (error "band width must be greater than 0"))
      	  (t
           (dotimes (i chans newsnd)
             ;; gains[i] can be either a number or a signal
             (cond ((numberp (aref gains i))
		            (cond ((not (zerop (aref gains i)))
				           (setf newsnd
				                 ;; note: gain in dB
                                 (eq-band newsnd
                                          (step-to-hz (+
                                                       (* i bandsep)
                                                       lowstep))
                                          (aref gains i)
                                          (/ bandsep 12))))))
                   (t
                    (setf newsnd
                          (eq-band newsnd
	                               (inf-const (step-to-hz (+
					                                       (* i bandsep)
		                                                   lowstep)))
                                   (aref gains i)
					               (inf-const (/ bandsep 12)))))))))))


; nband eq without variable range
; wraps around nband-vl with standard limits
(defun nband (sig gains)
  (nband-range sig gains 20 20000)
)	
       

; simple test demo
;(play (nband-vl (noise 1) '(-6 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0) 20 20000))

; variable gain demo
;(play (nband-vl (noise 1) '(0 0 0 0 0 0 (pwl 0 0 .9 10 1) 0 0 0 0 0 0 0 0 0 0 (pwl 0 -10 .9 10 1) (pwl 0 -10 .9 10 1) (pwl 0 -10 .9 10 1) (pwl 0 -10 .9 10 1) 0 0 0 0) 20 20000))

; test of adjacent band cuts
;(play (nband-vl (noise 1) '(0 0 0 0 0 0 0 0 0 0 6 6 6 0 0 0 -6 -6 0 0 0 0) 20 20000))
