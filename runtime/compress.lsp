; This code implements a compressor for noisy speech audio.
; There are actually two compressors that can be used in 
; series. The first is
; a fairly standard one: it detects signal level with an RMS
; detector and used table-lookup to determine how much gain
; to place on the original signal at that point. One bit of
; cleverness here is that the RMS envelope is "followed" or
; enveloped using SND-FOLLOW, which does look-ahead to anticipate
; peaks before they happen.
;
; The other piece of high-tech is COMPRESS-MAP, which builds 
; a map in terms of compression and expansion. What I recommend
; is figure out the noise floor on the signal you are compressing.
; Use a compression map that leaves the noise alone and boosts
; signals that are well above the noise floor. Alas, the COMPRESS-MAP
; function is not written in these terms, so some head-scratching is
; involved. Maybe I'll write another map generator if someone has a
; good application to test with.

; COMPRESS-MAP -- constructs a map for the compress function
;
; The map consists of two parts: a compression part and an expansion part.
; The intended use is to compress everything above compress-threshold by
; compress-ratio, and to downward expand everything below expand-ratio
; by expand-ratio.  Thresholds are in dB and ratios are dB-per-dB.
; 0dB corresponds to a peak amplitude of 1.0 or rms amplitude of 0.7
; If the input goes above 0dB, the output can optionally be limited
; by setting :limit (a keyword parameter) to T. This effectively changes 
; the compression ratio to infinity at 0dB.  If :limit is NIL
; (the default), then the compression-ratio continues to apply above 0dB.
; 
; Another keyword parameter, :transition, sets the amount below the
; thresholds (in dB) that a smooth transition starts. The default is 0,
; meaning that there is no smooth transition.
; 
; It is assumed that expand-threshold <= compress-threshold <= 0
; The gain is unity at 0dB so if compression-ratio > 1, then gain
; will be greater than unity below 0dB

; RETURNS: a sound for use in the SHAPE function. The sound maps input
; dB to gain. Time 1.0 corresponds to 0dB, and Time 0.0 corresponds to
; -100 dB, and Time 2.0 corresponds to +100dB, so this is a 
; 100hz "sample rate" sound. The sound gives gain in dB.

; Smooth transition equations: this is a parabola that makes a
; transition between two intersecting straight lines. The parabola 
; matches the slope of the lines where it intersects them, and
; it intersects the first (left) line at location (u, v). The equation
; is:
;     y = v + m(x-u) + d(s-m)((x-u)/d - (x-u)^2/(2d^2))
;       = v + m(x-u) + (s-m)((x-u) - (x-u)^2/(2d))
;       = v + m(x-u) + (s-m)((x-u) - (x-u)^2(s-m)/(4(b-v)))
; 
; where s is the slope of the left line, the right line is expressed by
;     y = mx+b, and
; d is the duration of the transition = 2(b-v)/(s-m)
;
; To show this is correct, show that (1) at the left intersection, the left 
; line and the transition both pass through u,v and (2) have the same slope s,
; and show that (3) at the right intersection, the right line and the 
; transition both meet at u+d and (4) have the same slope m.
;
; transition runs from u,v on left line to u+d on right line
; d = 2(v - mu - f)/(m - s), 
; where right line is described by y = mx + f and left line slope = s
; c = (m - s)/2d
; b = s - 2cu
; a = v - bu - cu^2
;
; transition is y = a + bx + cx^2
;
; Now, show that curve meets left line at x = u
; (1) a + bx + cx^2 = v at x = u
; a + bu + cuu = v - bu - cuu + bu + cuu = v
;
; (2) slope at x = u is s:
; b + 2cu = s - 2cu + 2cu = s
;
; (3) curve meets right line at x = u + d
; a + b(u + d) + c(uu + 2ud + dd) =
; v - bu - cuu + bu + bd + cuu + 2cud + cdd =
; v + bd +2cud + cdd =
; v + (s - 2cu)d + 2cud + cdd =
; v + sd + cdd =
; v + sd + dd(m-s)/2d =
; v + sd + d(m-s)/2 =

; v + s(2(v - mu - f)/(m - s)) + (2(v - mu - f)/(m - s))(m-s)/2 =
; v + 2sv/(m-s) -2smu/(m-s) -2sf/(m-s) + v - mu - f =
; 2v + (2sv - 2smu - 2sf)/(m-s) - mu - f = 
; 2v + 2s(v - mu - f)/(m-s) - mu - f =
; 2v + sd - mu - f
; try subtracting mx + b':
; 2v + sd - mu - f - m(u + d) - f =
; 2v + sd - 2mu - 2f - md = 
; 2v + (s - m)d - 2mu - 2f =
; 2v + (s - m)2(v - mu - f) / (m - s) - 2mu - 2f =
; 0 
;   
(defun compress-map (compress-ratio compress-threshold expand-ratio 
                     expand-threshold &key (limit nil) (transition 0.0)
                                           (verbose t))
  ;(display "compress-map" compress-ratio compress-threshold expand-ratio 
  ;                   expand-threshold limit transition)
  (let (m s			; see equations above
        eupd 			; eu + d
        cupd			; ct1 + d
        lim			; 0dB or infinity, depends on limit
        b2			; y-intercept of the 1:1 part
        ea eb ec ca cb cc	; polynomial coefficients
        eu ev cu cv		; intersection points (u,v)
        ed cd			; d values
        lower-db upper-db	; function to compute
        map			;  samples for map
        x			; loop value
        den			; denominator
        )
    ; check input for good values:
    (cond ((> expand-threshold compress-threshold)
           (error "expand-threshold must be lower than compress threshold"))
          ((> compress-threshold 0)
           (error "compress-threshold must be at or below 0dB"))
          ((<= compress-ratio 0.0)
           (error "negative compress-ratio"))
          ((< expand-ratio 0.0)
           (error "negative expand-ratio"))
    )
    ; set some constants
    (setf eu (- expand-threshold transition))
    (setf cu (- compress-threshold transition))
    (setf m (/ 1.0 compress-ratio))
    (setf s expand-ratio)	; rename to match equations
    ; point where compression line intersects non-compression
    ; line is (* m compress-threshold), and cv is this point
    ; minus transition (since slope is one)
    (setf cv (- (* m compress-threshold) transition))
    ; slope is 1 from compress-threshold to expand-threshold 
    (setf ev (+ (* m compress-threshold)
                (- expand-threshold compress-threshold)
                (* s (- transition))))	
    ; the 1:1 part passes through cu,cv with slope of 1, so the y-intercept
    ; is cv-cu
    (setf b2 (- cv cu))
    ; d = 2(v - mu - f)/(m - s)	--note m = s, s = 1, f = 0
    (setf den (- m 1.0))
    (cond ((< (abs den) .001)
               (setf cd 0.0))
          (t
           (setf cd (* 2 (- cv (* cu m)) (/ den)))))
    (setf cupd (+ cu cd))

    (setf den (- 1.0 s))
    (cond ((< (abs den) .001)
               (setf ed 0.0))
          (t
           (setf ed (* 2 (- ev eu b2) (/ den)))))
    (setf eupd (+ eu ed))

    ; ec = (1.0 - s)/(2*ed)
    (cond ((< (abs ed) 0.001)
           (setf ec 0.0))
          (t
           (setf ec (/ (- 1.0 s) (* 2.0 ed)))))
    ; eb = s - 2*ec*eu
    (setf eb (- s (* 2.0 ec eu)))
    ; ea = ev - eb*eu - ec*eu*eu
    (setf ea (- ev (* eb eu) (* ec eu eu)))

    ; cc = (m - 1.0)/(2*cd)
    (cond ((< (abs cd) 0.001)
           (setf cc 0.0))
          (t
           (setf cc (/ (- m 1.0) (* 2.0 cd)))))
    ; cb = s - 2*cc*cu
    (setf cb (- 1.0 (* 2.0 cc cu)))
    ; ca = cv - cb*cu - cc*cu*cu
    (setf ca (- cv (* cb cu) (* cc cu cu)))


    (cond (limit ; hard limit to 0dB
               (setf lim 0.0))
          (t 	 ; no hard limit, set limit to effectively infinity
           (setf lim 10000.0)))

    ;(display "compress-map" 
    ;    m s			; see equations above
    ;    eupd 			; et1 + d
    ;    cupd			; ct1 + d
    ;    lim			; 0dB or infinity, depends on limit
    ;    b2			; y-intercept of the 1:1 part
    ;    ea eb ec ca cb cc	; polynomial coefficients
    ;    eu ev cu cv		; intersection points (u,v)
    ;    ed cd)			; d values

    ; now create function that goes 100dB below expansion threshold
    ; and up to 100dB
    (setf lower-db -100.0)
    (setf upper-db 100.0)
    (setf map (make-array 201))
    (setf x lower-db)	; this should be an even integer
    (if verbose (format t "COMPRESS-MAP~%Input dB -> Gain dB~%"))
    (dotimes (i (length map))
        (setf (aref map i) 
              (cond ((< x eu) (+ ev (* s (- x eu))))
                  ((< x eupd) (+ ea (* eb x) (* ec x x)))
                  ((< x cu) (+ cv (- x cu)))
                  ((< x cupd) (+ ca (* cb x) (* cc x x)))
                  ((< x lim) (* m x))
                  (t 0)))
        ; map[i] has the desired output dB, so subtract input dB to
        ; get gain:
        (setf (aref map i) (- (aref map i) x))
        (cond ((and verbose ; (> x (- eu 3)) (< x 0))
                    (> x -30) (< x 20))
               (format t "~A -> ~A~%" x (aref map i))))
        (setf x (+ x 1)))
    ; return a sound
    (snd-from-array 0.0 100.0 map)))


(defun db-average (input)
  (let (y)
    (setf y (mult input input)) ; first square input
    (setf y (snd-avg y 1000 500 op-average)) ; then time average
    (setf y (snd-log (scale 2.0 y))) ; peak normalization, then take log
    (setf y (scale (/ 10.0 (log 10.0)) y))  ; see below for scaling explanation
    y))


(defun compress (input map rise-time fall-time &optional (lookahead 0.0))
  ; take the square of the input to get power
  (let ((in-squared (mult input input))
        window avg env gain delay)
    (cond ((zerop lookahead) (setf lookahead rise-time)))
    ; compute the time-average (sort of a low-pass) of the square
    ; parameters give 50ms window and a 25ms step
    (setf window (round (* (snd-srate input) 0.05)))
    (setf avg (snd-avg in-squared window (/ window 2) op-average))
    ; (setf *ca* avg)
    ; use follower to anticipate rise and trail off smoothly
    ; N.B.: the floor (2nd argument to snd-follow) should be the
    ; square of the noise floor, e.g. for a noise floor of 1/2^16,
    ; use 1/2^32 = about 4E-9. If the number is too small, you will
    ; not get expansion below the square root of the floor parameter.
    ; set lookahead to be number of samples in rise time:
    (setf lookahead (round (* lookahead (snd-srate avg)))) 
    ; lookahead must be at least one analysis window sample:
    (setf lookahead (max 1 lookahead))
    (setf env (snd-follow avg 0.000001 rise-time fall-time lookahead))
    ; (setf *ce* env)
    ; take logarithm to get dB instead of linear, also adjust for
    ; peak vs. average as follows: a sinusoid with peak of 1.0 has
    ; an average amplitude of 1/sqrt(2), we squared the signal, so
    ; the average amplitude should be 1/2, so multiply by 2 so
    ; that a sine with peak amplitude of 1 will get an average of 1
    ; which will convert to 0dB
    ; logenv is natural log(avg(x^2)), not dB
    (setf logenv (snd-log (scale 2.0 env)))
    ; tricky part: map converts dB of input to desired gain in dB
    ; this defines the character of the compressor
    ; map is scaled so that (0,2) corresponds to (-100dB, 100dB)
    ; so you need to scale input by .01. But first, we need to get dB:
    ; we have log(avg(x^2)), and we want dB = 20log10(sqrt(avg(x^2)))
    ; simplify dB to 10log10(avg(x^2)) = 10log(avg(x^2))/log(10),
    ; so scale by 10/log(10) * 0.01 = 0.1/log(10)
    (setf shaped-env (shape (scale (/ 0.1 (log 10.0)) logenv) map 1.0))
    ; (setf *cs* shaped-env)
    ; Go back to linear. To get from dB to linear, use:
    ; 20log10(linear) = dB
    ; linear = 10^(dB/20) = exp(log(10) * db/20),
    ; so scale the result by log(10)/20
    (setf gain (snd-exp (scale (/ (log 10.0) 20) shaped-env)))
    ; return the scaled input sound,
    ; another trick: avg signal is advanced by 1/2 window size because 
    ; the first avg sample at 0 represents the window from 
    ; [0, windowsize], taken to be windowsize/2. Windowsize in samples
    ; is 1 because there is a 50% overlap of windows. Also, snd-follow
    ; has a delayed response because it's looking ahead in sound.
    ; The time shift is lookahead - 1 (in samples)
    ; therefore, the response delay = (lookahead - 1) / (snd-srate avg)
    (setf delay (/ (1- lookahead) (snd-srate avg)))
    ; (display "compress" lookahead (snd-srate avg) delay)
    (format t "COMPRESS delay is ~A seconds.~%" delay)
    ; (setf *cg* gain)
    (sound-srate-abs (snd-srate input) ; set default sample rate for s-rest
      (mult (seq (s-rest delay) (cue input))
            gain))))


 ; this is an automatic gain control using peak detection for 
 ; gain control -- the range parameter gives the maximum gain in dB
 ; the agc will attenuate peaks to 1.0.
 ;
 (defun agc (input range rise-time fall-time &optional (lookahead 0.0))
  ; take the square of the input to get power
  (let (window avg env gain lookahead-samples)
    (cond ((zerop lookahead) (setf lookahead rise-time)))
    ; compute the time-average (sort of a low-pass) of the square
    ; parameters give 50ms window and a 25ms step
    (setf window (round (* (snd-srate input) 0.05)))
    (setf avg (snd-avg input window (/ window 2) op-peak))
    ; use follower to anticipate rise and trail off smoothly
    ; set lookahead to be number of samples in rise time:
    (setf lookahead-samples (round (* lookahead (snd-srate avg)))) 
    (setf env (snd-follow avg (db-to-linear (- range))
                          rise-time fall-time lookahead-samples))
    (setf gain (snd-recip env))
    ; return the scaled input sound,
    ; another trick: avg signal will be delayed. Also, snd-follow
    ; has a delayed response because it's looking ahead in sound
    ; 20 = the number of samples of lookahead from snd-follow
    ; 88.2 = 44,100 (sample rate) / 500 (the step-size in avg)
    ; in other words, 44100/500 is the sample rate of the control
    ; signal looked at by follow
    (sound-srate-abs (snd-srate input) ; set default sample rate for s-rest
      (mult (seq (s-rest lookahead) (cue input)) gain))
      ;(vector ; (seq (s-rest lookahead) (cue input))
      ;	      (mult (seq (s-rest lookahead) (cue input)) gain) 
      ;        (force-srate (snd-srate input) (scale 0.3 gain))))
    ))


