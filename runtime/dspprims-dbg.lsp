;; dspprims.lsp -- interface to dsp primitives

;; ARESON - notch filter
;; 
(defun areson (s c b &optional (n 0))
  (multichan-expand "ARESON" #'nyq:areson s c b n))

(setf areson-implementations
      (vector #'snd-areson #'snd-aresonvc #'snd-aresoncv #'snd-aresonvv))

;; NYQ:ARESON - notch filter, single channel
;;
(defun nyq:areson (signal center bandwidth normalize)
  (select-implementation-1-2 "ARESON" areson-implementations 
   signal center bandwidth normalize))


;; hp - highpass filter
;; 
(defun hp (s c)
  (multichan-expand "HP" #'nyq:hp s c))

(setf hp-implementations
      (vector #'snd-atone #'snd-atonev))

;; NYQ:hp - highpass filter, single channel
;;
(defun nyq:hp (s c)
  (select-implementation-1-1 "HP" hp-implementations s c))


;; comb-delay-from-hz -- compute the delay argument
;;
(defun comb-delay-from-hz (hz)
  (recip hz))

;; comb-feedback -- compute the feedback argument
;;
(defun comb-feedback (decay delay)
  (s-exp (mult -6.9087 delay (recip decay))))

;; COMB - comb filter
;; 
;; this is just a feedback-delay with different arguments
;;
(defun comb (snd decay hz)
  (multichan-expand "COMB" #'nyq:comb snd decay hz))


(defun nyq:comb (snd decay hz)
  (ny:assert (or (numberp decay) (multichannelp decay))
    "In COMB, 2nd argument (decay) must be a number, sound, or array thereof"
    decay)
  (ny:assert (and (numberp hz) (> hz 0))
    "In COMB, 3rd argument (hz) must be a positive number or array of them" hz)
  (let (delay feedback len d)
    ; convert decay to feedback
    (setf delay (/ 1.0 (float hz)))
    (setf feedback (comb-feedback decay delay))
    (nyq:feedback-delay snd delay feedback "COMB")))

;; ALPASS - all-pass filter
;; 
(defun alpass (snd decay hz &optional min-hz)
  (multichan-expand "ALPASS" #'nyq:alpass snd decay hz min-hz))
  
(defun nyq:alpass (snd decay hz min-hz)
  (ny:assert (or (numberp decay) (soundp decay))
    "In ALPASS, 2nd argument (decay) must be a number, sound, or array thereof"
    decay)
  (ny:assert (or (and (numberp hz) (> hz 0)) (soundp hz))
    "In ALPASS, 3rd argument (hz) must be a positive number or array thereof" hz)
  (let (delay feedback len d)
    ; convert decay to feedback, iterate over array if necessary
    (setf delay (comb-delay-from-hz hz))
    (setf feedback (comb-feedback decay delay))
    (nyq:alpass1 snd delay feedback min-hz)))


;; CONST -- a constant at control-srate
;;
(defun const (value &optional (dur 1.0))
  (ny:assert (numberp value)
    "In CONST, 1st argument must be a number" value)
  (ny:assert (numberp dur)
    "In CONST, 2nd argument must be a number" dur)
  (let ((d (get-duration dur)))
    (snd-const value *rslt* *CONTROL-SRATE* d)))


;; CONVOLVE - fast convolution
;; 
(defun convolve (s r)
  (multichan-expand "CONVOLVE" #'nyq:convolve s r))

(defun nyq:convolve (s r)
  (ny:assert (soundp s)
    "In CONVOLVE, 1st argument must be a sound or multichannel sound" s)
  (ny:assert (soundp r)
    "In CONVOLVE, 2nd argument must be a sound or multichannel sound" r)
  (snd-convolve s (force-srate (snd-srate s) r)))


;; FEEDBACK-DELAY -- (delay is quantized to sample period)
;;
(defun feedback-delay (snd delay feedback)
  (multichan-expand "FEEDBACK-DELAY" #'nyq:feedback-delay snd delay feedback))
  

;; SND-DELAY-ERROR -- report type error
;;
(defun snd-delay-error (snd delay feedback)
  (error "FEEDBACK-DELAY with variable delay is not implemented"))


(setf feedback-delay-implementations
      (vector #'snd-delay #'snd-delay-error #'snd-delaycv #'snd-delay-error))


;; NYQ:FEEDBACK-DELAY -- single channel delay
;;
(defun nyq:feedback-delay (snd delay feedback &optional (src "FEEDBACK-DELAY"))
  (ny:assert (soundp snd)
    (strcat "In " src ", 1st argument (snd) must be a sound or multichannel sound")
    snd)
  (ny:assert (numberp delay)
    "In FEEDBACK-DELAY, 2nd argument (delay) must be a number or array of numbers"
    delay)
  (ny:assert (or (numberp feedback) (soundp feedback))
    "In FEEDBACK-DELAY, 2nd argument (feedback) must be a number, sound, or array thereof"
    delay)
  (select-implementation-1-2 src feedback-delay-implementations 
                             snd delay feedback))


;; SND-ALPASS-ERROR -- report type error
;;
(defun snd-alpass-error (snd delay feedback)
  (error "ALPASS with constant decay and variable hz is not implemented"))


(if (not (fboundp 'snd-alpasscv))
    (defun snd-alpasscv (snd delay feedback min-hz)
      (error "snd-alpasscv (ALPASS with variable decay) is not implemented")))
(if (not (fboundp 'snd-alpassvv))
    (defun snd-alpassvv (snd delay feedback min-hz)
      (error "snd-alpassvv (ALPASS with variable decay and feedback) is not implemented")))
      

(defun nyq:alpassvv (the-snd delay feedback min-hz)
    (let (max-delay)
      (cond ((or (not (numberp min-hz))
                 (<= min-hz 0))
             (ny:assert nil
              "In ALPASS, 4th parameter (min-hz) must be a number when delay is a sound"
              min-hz)))
      (setf max-delay (/ 1.0 min-hz))
      ; make sure delay is between 0 and max-delay
      ; use clip function, which is symetric, with an offset
      (setf delay (snd-offset (clip (snd-offset delay (* max-delay -0.5))
                                    (* max-delay 0.5))
                              (* max-delay 0.5)))
      ; now delay is between 0 and max-delay, so we won't crash nyquist when
      ; we call snd-alpassvv, which doesn't test for out-of-range data
      (snd-alpassvv the-snd delay feedback max-delay)))


;; NYQ:SND-ALPASS -- ignores min-hz argument and calls snd-alpass
;;
(defun nyq:snd-alpass (snd delay feedback min-hz)
  (snd-alpass snd delay feedback))

;; NYQ:SND-ALPASSCV -- ignores min-hz argument and calls snd-alpasscv
;;
(defun nyq:snd-alpasscv (snd delay feedback min-hz)
  (snd-alpasscv snd delay feedback))

(setf alpass-implementations
      (vector #'nyq:snd-alpass #'snd-alpass-error
              #'nyq:snd-alpasscv #'nyq:alpassvv))


;; NYQ:ALPASS1 -- single channel alpass
;;
(defun nyq:alpass1 (snd delay feedback min-hz)
  (select-implementation-1-2 "ALPASS" alpass-implementations
                              snd delay feedback min-hz))

;; CONGEN -- contour generator, patterned after gated analog env gen
;;
(defun congen (gate rise fall)
  (ny:assert (or (soundp gate) (multichannel-soundp gate))
    "In CONGEN, 1st argument (gate) must be a sound or multichannel sound" gate)
  (ny:assert (or (numberp rise) (numbersp rise))
    "In CONGEN, 2nd argument (rise) must be a number or array of numbers" rise)
  (ny:assert (or (numberp fall) (numbersp fall))
    "In CONGEN, 2nd argument (fall) must be a number or array of numbers" rise)
  (multichan-expand "CONGEN" #'snd-congen gate rise fall))


;; S-EXP -- exponentiate a sound
;;
(defun s-exp (s)
  (ny:assert (or (numberp s) (soundp s) (multichannelp s))
    "In S-EXP, argument must be a number, sound, or array thereof" s)
  (multichan-expand "S-EXP" #'nyq:exp s))


;; NYQ:EXP -- exponentiate number or sound
;;
(defun nyq:exp (s) (if (soundp s) (snd-exp s) (exp s)))

;; S-ABS -- absolute value of a sound
;;
(defun s-abs (s)
  (multichan-expand "S-ABS" #'nyq:abs s))

;; NYQ:ABS -- absolute value of number or sound
;;
(defun nyq:abs (s)
  (ny:assert (or (numberp s) (soundp s))
    "In S-ABS, argument must be a number, sound, or array thereof" s)
  (if (soundp s) (snd-abs s) (abs s)))

;; S-SQRT -- square root of a sound
;;
(defun s-sqrt (s)
  (multichan-expand "S-SQRT" #'nyq:sqrt s))

;; NYQ:SQRT -- square root of a number or sound
;;
(defun nyq:sqrt (s)
  (ny:assert (or (numberp s) (soundp s))
    "In S-SQRT, argument must be a number, sound, or array thereof" s)
  (if (soundp s) (snd-sqrt s) (sqrt s)))


;; INTEGRATE -- integration
;;
(defun integrate (s)
  (ny:assert (or (numberp s) (multichannelp s))
    "In INTEGRATE, argument must be a number, sound, or array thereof" s)
  (multichan-expand "INTEGRATE" #'snd-integrate s))


;; S-LOG -- natural log of a sound
;;
(defun s-log (s)
  (multichan-expand "S-LOG" #'nyq:log s))


;; NYQ:LOG -- log of a number or sound
;;
(defun nyq:log (s)
  (ny:assert (or (numberp s) (soundp s))
    "In S-LOG, argument must be a number, sound, or array thereof" s)
  (if (soundp s) (snd-log s) (log s)))


;; NOISE -- white noise
;;
(defun noise (&optional (dur 1.0))
  (ny:assert (numberp dur)
    "In NOISE, argument (dur) must be a number" dur)
  (let ((d (get-duration dur)))
    (snd-white *rslt* *SOUND-SRATE* d)))


(defun noise-gate (snd &optional (lookahead 0.5) (risetime 0.02) (falltime 0.5)
                                                 (floor 0.01) (threshold 0.01))
  (ny:assert (soundp snd)
    "In NOISE-GATE, 1st argument must be a sound" snd)
  (ny:assert (numberp lookahead)
    "In NOISE-GATE, all 2nd argument (lookahead) must be a number" lookahead)
  (ny:assert (numberp risetime)
    "In NOISE-GATE, all 3rd argument (risetime) must be a number" risetime)
  (ny:assert (numberp falltime)
    "In NOISE-GATE, all 4th argument (falltime) must be a number" falltime)
  (ny:assert (numberp floor)
    "In NOISE-GATE, all 5th argument (floor) must be a number" floor)
  (ny:assert (numberp threshold)
    "In NOISE-GATE, all 6th argument (threshold) must be a number" threshold)
  (let ((rms (lp (mult snd snd) (/ *control-srate* 10.0))))
    (setf threshold (* threshold threshold))
    (mult snd (gate rms floor risetime falltime lookahead threshold))))


;; QUANTIZE -- quantize a sound
;;
(defun quantize (s f)
  (ny:assert (or (soundp s) (multichannel-soundp s))
    "In QUANTIZE, 1st argument must be a sound or multichannel sound" s)
  (ny:assert (or (numberp f) (numbersp f))
    "In QUANTIZE, 2nd argument must be a number or array of numbers" f)
  (multichan-expand "QUANTIZE" #'snd-quantize s f))


;; RECIP -- reciprocal of a sound
;;
(defun recip (s)
  (multichan-expand "RECIP" #'nyq:recip s))


;; NYQ:RECIP -- reciprocal of a number or sound
;;
(defun nyq:recip (s)
  (cond ((soundp s) (snd-recip s))
        ((numberp s) (/ (float s)))
        (t (ny:assert nil
             "In RECIP, argument must be a sound, number, or array thereof"))))


;; RMS -- compute the RMS of a sound
;;
(defun rms (s &optional (rate 100.0) window-size)
  (let (rslt step-size)
    (ny:assert (soundp s)
      "In RMS, 1st argument must be a sound" s)
    (ny:assert (numberp rate)
      "In RMS, 2nd argument (rate) must be a number" rate)
    (setf step-size (round (/ (snd-srate s) rate)))
    (cond ((null window-size)
           (setf window-size step-size))
          (t (ny:assert (integerp window-size)
               "In RMS, 2nd argument (window-size) must be an integer"
               window-size)))
    (setf s (prod s s))
    (setf result (snd-avg s window-size step-size OP-AVERAGE))
    ;; compute square root of average
    (s-exp (scale 0.5 (s-log result)))))


;; RESON - bandpass filter
;; 
(defun reson (s c b &optional (n 0))
  (ny:assert (or (soundp s) (multichannel-soundp s))
    "In RESON, 1st argument must be a sound or multichannel sound" s)
  (ny:assert (or  (numberp c) (soundp c) (multichannelp c))
    "In RESON, 2nd argument (cutoff) must be a number, sound, or array thereof" c)
  (ny:assert (or  (numberp b) (soundp b) (multichannelp b))
    "In RESON, 3rd argument (bandwidth) must be a number, sound, or array thereof" b)
  (ny:assert (integerp n)
    "In RESON, 4th argument must be 0, 1 or 2" n)
  (multichan-expand "RESON" #'nyq:reson s c b n))

(setf reson-implementations
      (vector #'snd-reson #'snd-resonvc #'snd-resoncv #'snd-resonvv))

;; NYQ:RESON - bandpass filter, single channel
;;
(defun nyq:reson (signal center bandwidth normalize)
  (select-implementation-1-2 reson-implementations 
   signal center bandwidth normalize))


;; SHAPE -- waveshaper
;;
(defun shape (snd shape origin)
  (ny:assert (or (soundp snd) (multichannel-soundp snd))
    "In SHAPE, 1st argument must be a sound or multichannel sound" snd)
  (ny:assert (or (soundp shape) (multichannel-soundp shape))
    "In SHAPE, 2nd argument (shape) must be a sound or multichannel sound" shape)
  (ny:assert (or (numberp origin) (numbersp origin))
    "In SHAPE, 3rd argument (origin) must be a number or array of numbers" origin)
  (multichan-expand "SHAPE" #'snd-shape snd shape origin))


;; SLOPE -- calculate the first derivative of a signal
;;
(defun slope (s)
  (ny:assert (or (soundp s) (multichannel-soundp s))
    "In SLOPE, argument must be a sound or multichannel sound" s)
  (multichan-expand "SLOPE" #'nyq:slope s))


;; NYQ:SLOPE -- first derivative of single channel
;;
(defun nyq:slope (s)
  (let* ((sr (snd-srate s))
         (sr-inverse (/ sr)))
    (snd-xform (snd-slope s) sr 0 sr-inverse MAX-STOP-TIME 1.0)))


;; lp - lowpass filter
;; 
(defun lp (s c)
  (ny:assert (or (soundp s) (multichannel-soundp s))
    "In LP, 1st argument must be a sound or multichannel sound" s)
  (ny:assert (or (numberp c) (soundp c) (multichannelp c))
    "In LP, 2nd argument must be a number, sound, or array thereof" c)
  (multichan-expand "LP" #'nyq:lp s c))

(setf lp-implementations
      (vector #'snd-tone #'snd-tonev))

;; NYQ:lp - lowpass filter, single channel
;;
(defun nyq:lp (s c)
  (select-implementation-1-1 lp-implementations s c))



;;; fixed-parameter filters based on snd-biquad
;;; note: snd-biquad is implemented in biquadfilt.[ch],
;;; while BiQuad.{cpp,h} is part of STK

(setf Pi 3.14159265358979)

(defun square (x) (* x x))
(defun sinh (x) (* 0.5 (- (exp x) (exp (- x)))))


; remember that snd-biquad uses the opposite sign convention for a_i's 
; than Matlab does.
; 
; Stability: Based on courses.cs.washington.edu/courses/cse490s/11au/
; Readings/Digital_Sound_Generation_2.pdf, the stable region is 
;   (a2 < 1) and ((a2 + 1) > |a1|)
; It doesn't look to me like our a0, a1, a2 match the paper's a0, a1, a2,
; and I'm not convinced the paper's derivation is correct, but at least
; the predicted region of stability is correct if we swap signs on a1 and
; a2 (but due to the |a1| term, only the sign of a2 matters). This was
; tested manually at a number of points inside and outside the stable
; triangle. Previously, the stability test was (>= a0 1.0) which seems
; generally wrong. The old test has been removed.

; convenient biquad: normalize a0, and use zero initial conditions.
(defun nyq:biquad (x b0 b1 b2 a0 a1 a2)
  (if (<= a0 0.0)
      (error (format nil "a0 < 0 (unstable parameter a0 = ~A) in biquad~%" a0)))
  (let ((a0r (/ 1.0 a0)))
    (setf a1 (* a0r a1) 
          a2 (* a0r a2))
    (if (or (<= a2 -1.0) (<= (- 1.0 a2) (abs a1)))
        (error (format nil 
         "(a2 <= -1) or (1 - a2 <= |a1|) (~A a1 = ~A, a2 = ~A) in biquad~%" 
         "unstable parameters" a1 a2)))
    (snd-biquad x (* a0r b0) (* a0r b1) (* a0r b2) 
                  a1 a2 0 0)))


(defun biquad (x b0 b1 b2 a0 a1 a2 &optional (source "BIQUAD"))
  (ny:assert (or (soundp x) (multichannel-soundp x))
    (strcat "In " source ", 1st argument must be a sound or multichannel sound")
    x)
  (ny:assert (and (or (numberp b0) (numbersp b0))
                  (or (numberp b1) (numbersp b1))
                  (or (numberp b2) (numbersp b2))
                  (or (numberp a0) (numbersp a0))
                  (or (numberp a1) (numbersp a1))
                  (or (numberp a2) (numbersp a2)))
    (strcat "In " source
     ", all but 1st argument must be numbers or arrays of numbers"))
  (multichan-expand "BIQUAD" #'nyq:biquad x b0 b1 b2 a0 a1 a2))


; biquad with Matlab sign conventions for a_i's.
(defun biquad-m (x b0 b1 b2 a0 a1 a2)
  (multichan-expand "BIQUAD-M" #'nyq:biquad-m x b0 b1 b2 a0 a1 a2))

(defun nyq:biquad-m (x b0 b1 b2 a0 a1 a2 &optional (source "BIQUAD-M"))
  (nyq:biquad x b0 b1 b2 a0 (- a1) (- a2)))

; two-pole lowpass
(defun lowpass2 (x hz &optional (q 0.7071))
  (multichan-expand "LOWPASS2" #'nyq:lowpass2 x hz q))

;; NYQ:LOWPASS2 -- operates on single channel
(defun nyq:lowpass2 (x hz q)
  (ny:assert (numberp hz)
    "In LOWPASS2, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp q)
    "In LOWPASS2, 3rd argument (q) must be a number or array of numbers" q)
  (if (or (> hz (* 0.5 (snd-srate x)))
          (< hz 0))
      (error "cutoff frequency out of range" hz))
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b1 (- 1.0 cw))
         (b0 (* 0.5 b1))
         (b2 b0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "LOWPASS2")))

; two-pole highpass
(defun highpass2 (x hz &optional (q 0.7071))
  (multichan-expand "HIGHPASS2" #'nyq:highpass2 x hz q))

(defun nyq:highpass2 (x hz q)
  (ny:assert (numberp hz)
    "In HIGHPASS2, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp q)
    "In HIGHPASS2, 3rd argument (q) must be a number or array of numbers" q)
  (if (or (> hz (* 0.5 (snd-srate x)))
          (< hz 0))
      (error "cutoff frequency out of range" hz))
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b1 (- -1.0 cw))
         (b0 (* -0.5 b1))
         (b2 b0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "HIGHPASS2")))

; two-pole bandpass.  max gain is unity.
(defun bandpass2 (x hz q)
  (multichan-expand "BANDPASS2" #'nyq:bandpass2 x hz q))

(defun nyq:bandpass2 (x hz q)
  (ny:assert (numberp hz)
    "In BANDPASS2, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp q)
    "In BANDPASS2, 3rd argument (q) must be a number or array of numbers" q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b0 alpha)
         (b1 0.0)
         (b2 (- alpha)))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "BANDPASS2")))

; two-pole notch.
(defun notch2 (x hz q)
  (multichan-expand "NOTCH2" #'nyq:notch2 x hz q))

(defun nyq:notch2 (x hz q)
  (ny:assert (numberp hz)
    "In NOTCH2, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp q)
    "In NOTCH2, 3rd argument (q) must be a number or array of numbers" q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b0 1.0)
         (b1 (* -2.0 cw))
         (b2 1.0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "NOTCH2")))


; two-pole allpass.
(defun allpass2 (x hz q)
  (multichan-expand "ALLPASS2" #'nyq:allpass x hz q))

(defun nyq:allpass (x hz q)
  (ny:assert (numberp hz)
    "In ALLPASS2, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp q)
    "In ALLPASS2, 3rd argument (q) must be a number or array of numbers" q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (k (exp (* -0.5 w (/ 1.0 q))))
         (a0 1.0)
         (a1 (* -2.0 cw k))
         (a2 (* k k))
         (b0 a2)
         (b1 a1)
         (b2 1.0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "ALLPASS2")))


; bass shelving EQ.  gain in dB; Fc is halfway point.
; response becomes peaky at slope > 1.
(defun eq-lowshelf (x hz gain &optional (slope 1.0))
  (multichan-expand "EQ-LOWSHELF" #'nyq:eq-lowshelf x hz gain slope))


(defun nyq:eq-lowshelf (x hz gain slope)
  (ny:assert (soundp x)
    "In EQ-LOWSHELF, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In EQ-LOWSHELF, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp gain)
    "In EQ-LOWSHELF, 3rd argument (gain) must be a number or array of numbers" gain)
  (ny:assert (numberp slope)
    "In EQ-LOWSHELF, 4th argument (slope) must be a number or array of numbers" slope)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (sw (sin w))
         (cw (cos w))
         (A (expt 10.0 (/ gain (* 2.0 20.0))))
         (b (sqrt (- (/ (+ 1.0 (square A)) slope) (square (- A 1.0)))))
         (apc (* cw (+ A 1.0)))
         (amc (* cw (- A 1.0)))
         (bs (* b sw))

         (b0 (*      A (+ A  1.0 (- amc)    bs  )))
         (b1 (*  2.0 A (+ A -1.0 (- apc)        )))
         (b2 (*      A (+ A  1.0 (- amc) (- bs) )))
         (a0           (+ A  1.0    amc     bs  ))
         (a1 (* -2.0   (+ A -1.0    apc         )))
         (a2           (+ A  1.0    amc  (- bs) )))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))


; treble shelving EQ.  gain in dB; Fc is halfway point.
; response becomes peaky at slope > 1.
(defun eq-highshelf (x hz gain &optional (slope 1.0))
  (multichan-expand "EQ-HIGHSHELF" #'nyq:eq-highshelf x hz gain slope))

(defun nyq:eq-highshelf (x hz gain slope)
  (ny:assert (soundp x)
    "In EQ-HIGHSHELF, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In EQ-HIGHSHELF, 2nd argument (hz) must be a number or array of numbers" hz)
  (ny:assert (numberp gain)
    "In EQ-HIGHSHELF, 3rd argument (gain) must be a number or array of numbers" gain)
  (ny:assert (numberp slope)
    "In EQ-HIGHSHELF, 4th argument (slope) must be a number or array of numbers" slope)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (sw (sin w))
         (cw (cos w))
         (A (expt 10.0 (/ gain (* 2.0 20.0))))
         (b (sqrt (- (/ (+ 1.0 (square A)) slope) (square (- A 1.0)))))
         (apc (* cw (+ A 1.0)))
         (amc (* cw (- A 1.0)))
         (bs (* b sw))

         (b0 (*      A (+ A  1.0    amc     bs  )))
         (b1 (* -2.0 A (+ A -1.0    apc         )))
         (b2 (*      A (+ A  1.0    amc  (- bs) )))
         (a0           (+ A  1.0 (- amc)    bs  ))
         (a1 (*  2.0   (+ A -1.0 (- apc)        )))
         (a2           (+ A  1.0 (- amc) (- bs) )))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))
    
(defun nyq:eq-band (x hz gain width)
  (ny:assert (soundp x)
    "In EQ-BAND, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (or (numberp hz) (soundp hz))
    "In EQ-BAND, 2nd argument (hz) must be a number, sound, or array thereof" hz)
  (ny:assert (or (numberp gain) (soundp hz))
    "In EQ-BAND, 3rd argument (gain) must be a number, sound, or array thereof" gain)
  (ny:assert (or (numberp width) (soundp hz))
    "In EQ-BAND, 4th argument (width) must be a number, sound, or array thereof" width)
  (cond ((and (numberp hz) (numberp gain) (numberp width))
         (eq-band-ccc x hz gain width))
        ((and (soundp hz) (soundp gain) (soundp width))
         (snd-eqbandvvv x hz (db-to-linear gain) width))
        (t
         (error "In EQ-BAND, hz, gain, and width must be all numbers or all sounds"))))

; midrange EQ.  gain in dB, width in octaves (half-gain width).
(defun eq-band (x hz gain width)
  (multichan-expand "EQ-BAND" #'nyq:eq-band x hz gain width))
  
  
(defun eq-band-ccc (x hz gain width)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (sw (sin w))
         (cw (cos w))
         (J (sqrt (expt 10.0 (/ gain 20.0))))
         ;(dummy (display "eq-band-ccc" gain J))
         (g (* sw (sinh (* 0.5 (log 2.0) width (/ w sw)))))
         ;(dummy2 (display "eq-band-ccc" width w sw g))
         (b0 (+ 1.0 (* g J)))
         (b1 (* -2.0 cw))
         (b2 (- 1.0 (* g J)))
         (a0 (+ 1.0 (/ g J)))
         (a1 (- b1))
         (a2 (- (/ g J) 1.0)))
    (biquad x b0 b1 b2 a0 a1 a2)))

; see failed attempt in eub-reject.lsp to do these with higher-order fns:

; four-pole Butterworth lowpass
(defun lowpass4 (x hz)
  (ny:assert (soundp x)
    "In LOWPASS4, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In LOWPASS4, 2nd argument (hz) must be a number or array of numbers" hz)
  (lowpass2 (lowpass2 x hz 0.60492333) hz 1.33722126))

; six-pole Butterworth lowpass
(defun lowpass6 (x hz)
  (ny:assert (soundp x)
    "In LOWPASS6, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In LOWPASS6, 2nd argument (hz) must be a number or array of numbers" hz)
  (lowpass2 (lowpass2 (lowpass2 x hz 0.58338080) 
                                  hz 0.75932572) 
                                  hz 1.95302407))

; eight-pole Butterworth lowpass
(defun lowpass8 (x hz)
  (ny:assert (soundp x)
    "In LOWPASS8, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In LOWPASS8, 2nd argument (hz) must be a number or array of numbers" hz)
  (lowpass2 (lowpass2 (lowpass2 (lowpass2 x hz 0.57622191)
                                            hz 0.66045510) 
                                            hz 0.94276399)
                                            hz 2.57900101))

; four-pole Butterworth highpass
(defun highpass4 (x hz)
  (ny:assert (soundp x)
    "In HIGHPASS4, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In HIGHPASS4, 2nd argument (hz) must be a number or array of numbers" hz)
  (highpass2 (highpass2 x hz 0.60492333) hz 1.33722126))

; six-pole Butterworth highpass
(defun highpass6 (x hz)
  (ny:assert (soundp x)
    "In HIGHPASS6, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In HIGHPASS6, 2nd argument (hz) must be a number or array of numbers" hz)
  (highpass2 (highpass2 (highpass2 x hz 0.58338080) 
                                     hz 0.75932572) 
                                     hz 1.95302407))

; eight-pole Butterworth highpass
(defun highpass8 (x hz)
  (ny:assert (soundp x)
    "In HIGHPASS8, 1st argument must be a sound or multichannel sound" x)
  (ny:assert (numberp hz)
    "In HIGHPASS8, 2nd argument (hz) must be a number or array of numbers" hz)
  (highpass2 (highpass2 (highpass2 (highpass2 x hz 0.57622191)
                                                hz 0.66045510) 
                                                hz 0.94276399)
                                                hz 2.57900101))

; YIN
; maybe this should handle multiple channels, etc.
(defun yin (sound minstep maxstep stepsize)
  (ny:assert (soundp sound)
    "In YIN, 1st argument (sound) must be a sound" sound)
  (ny:assert (numberp minstep)
    "In YIN, 2st argument (minstep) must be a number" minstep)
  (ny:assert (numberp maxstep)
    "In YIN, 3rd argument (maxstep) must be a number" maxstep)
  (ny:assert (numberp stepsize)
    "In YIN, 4th argument (stepsize) must be a number" stepsize)
  (snd-yin sound minstep maxstep stepsize))


; FOLLOW
(defun follow (sound floor risetime falltime lookahead)
  (ny:assert (soundp sound)
    "In FOLLOW, 1st argument (sound) must be a sound" sound)
  (ny:assert (numberp floor)
    "In FOLLOW, 2st argument (floor) must be a number" floor)
  (ny:assert (numberp risetime)
    "In FOLLOW, 3rd argument (risetime) must be a number" risetime)
  (ny:assert (numberp falltime)
    "In FOLLOW, 4th argument (stepsize) must be a number" falltime)
  (ny:assert (numberp lookahead)
    "In FOLLOW, 5th argument (lookahead) must be a number" lookahead)
  ;; use 10000s as "infinite" -- that's about 2^30 samples at 96K
  (setf lookahead (round (* lookahead (snd-srate sound))))
  (extract (/ lookahead (snd-srate sound)) 10000
           (snd-follow sound floor risetime falltime lookahead)))


;; PHASE VOCODER
(defun phasevocoder (s map &optional (fftsize -1) (hopsize -1) (mode 0))
  (ny:assert (or (soundp s) (multichannel-soundp s))
    "In PHASEVOCODER, 1st argument must be a sound" s)
  (ny:assert (or (soundp map) (multichannel-soundp map))
    "In PHASEVOCODER, 2nd argument (map) must be a sound" map)
  (ny:assert (integerp fftsize)
    "In PHASEVOCODER, 3rd argument (fftsize) must be an integer" fftsize)
  (ny:assert (integerp hopsize)
    "In PHASEVOCODER, 4th argument (hopsize) must be an integer" hopsize)
  (ny:assert (integerp mode)
    "In PHASEVOCODER, 5th argument (mode) must be an integer" mode)
  (multichan-expand "PHASEVOCODER" #'snd-phasevocoder s map fftsize hopsize mode))


;; PV-TIME-PITCH
;; PV-TIME-PITCH -- control time stretch and transposition 
;;
;; stretchfn maps from input time to output time
;; pitchfn maps from input time to transposition factor (2 means octave up)
(defun pv-time-pitch (input stretchfn pitchfn dur &optional
                      (fftsize 2048) (hopsize nil) (mode 0))
  (ny:assert (or (soundp input) (multichannel-soundp input))
    "In PV-TIME-PITCH, 1st argument must be a sound" input)
  (ny:assert (or (soundp stretchfn) (multichannel-soundp stretchfn))
    "In PV-TIME-PITCH, 2nd argument (stretchfn) must be a sound" stretchfn)
  (ny:assert (or (soundp pitchfn) (multichannel-soundp pitchfn))
    "In PV-TIME-PITCH, 3rd argument (pitchfn) must be a sound" pitchfn)
  (ny:assert (numberp dur)
    "In PV-TIME-PITCH, 4th argument (dur) must be a number" dur)
  (ny:assert (integerp fftsize)
    "In PV-TIME-PITCH, 5th argument (fftsize) must be an integer" fftsize)
  (if (null hopsize) (setf hopsize (/ fftsize 8)))
  (ny:assert (integerp hopsize)
    "In PV-TIME-PITCH, 6th argument (hopsize) must be an integer" hopsize)
  (ny:assert (integerp mode)
    "In PV-TIME-PITCH, 7th argument (mode) must be an integer" mode)
  (multichan-expand "PV-TIME-PITCH" #'nyq:pv-time-pitch input
                    stretchfn pitchfn dur fftsize hopsize mode))

(defun nyq:pv-time-pitch (input stretchfn pitchfn dur fftsize hopsize mode)
  (let (wrate u v w vinv)
    (setf wrate (/ 3000  dur))
    (setf vinv (integrate (prod stretchfn  pitchfn)))
    (setf v (snd-inverse vinv (local-to-global 0) wrate))
    (setf w (integrate (snd-recip (snd-compose pitchfn v))))
    (sound-warp w (phasevocoder input v fftsize hopsize mode) wrate)))

