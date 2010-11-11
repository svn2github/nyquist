;; Examples of how to use distributions.lsp

;;1.  Altered granulate methods based on distribution

(load "gran")

;;The deviatons in pitch and grainlength of the standard granular synthesis
;;functions in are based on the uniform random distribution.  With simple
;;modifications, these can be made to take in a distribution generator
;;function as a variable. 

;; filename -- name of the file
;; grain-dur -- the duration of a grain
;; grain-dev -- grain dur is actually grain-dur + random(0, grain-dev)
;; ioi -- the basic inter-onset-interval for grains
;; ioi-dev -- ioi is actually: ioi + random(0, ioi-dev)
;; pitch-dist -- the distribution of the alteration in pitch to the grains
;;	       the distribution values should be > 1.
;; file-start -- when to start reading the file (an offset from start)
;; file-end -- when to stop reading the file (an offset from end)

(defun pitch-dist-granulate (filename grain-dur grain-dev ioi ioi-dev
	pitch-dist &optional (file-start 0) (file-end 0))
(let (orig n env actual-grain-dur step-size
        (avg-ioi (+ ioi (/ ioi-dev 2.0)))
        (file-dur (sf-dur filename))
        (dur (get-duration 1)))
    (setf n (truncate (/ dur avg-ioi)))
    (cond ((< file-dur file-start)
           (error "sf-granulate: file-start is after end of file!"))
          ((< file-dur file-end)
           (error "sf-granulate: file-end (offset) exceeds file duration!"))
          ((< file-dur (+ file-start file-end))
           (error "sf-granulate: file-start + file-end > file duration!")))
    (setf file-dur (- file-dur file-start file-end))
    (setf step-size (/ file-dur n))
    (stretch-abs 1.0 (let ()
      (seqrep (i n) (let ()
        (setf actual-grain-dur (real-random grain-dur (+ grain-dur grain-dev)))
        (setf env (stretch actual-grain-dur (one-minus-cosine)))
        (force-srate *sound-srate*
          (stretch (funcall pitch-dist)
            (sound2
             (set-logical-stop
              (mult (cue env)
                    (s-read filename 
                            :time-offset (+ file-start (* step-size i))
                            :dur actual-grain-dur))
              (real-random ioi (+ ioi ioi-dev))))))))))))


;; filename -- name of the file
;; dist -- the distribution function that the grain sizes should follow
;; ioi -- the basic inter-onset-interval for grains
;; ioi-dev -- ioi is actually: ioi + random(0, ioi-dev)
;; pitch-dev -- grains are resampled at rate between 1 and pitch-dev
;; file-start -- when to start reading the file (an offset from start)
;; file-end -- when to stop reading the file (an offset from end)

(defun len-dist-granulate (filename dist ioi ioi-dev pitch-dev 
                     &optional (file-start 0) (file-end 0))
  (let (orig n env actual-grain-dur step-size
        (avg-ioi (+ ioi (/ ioi-dev 2.0)))
        (file-dur (sf-dur filename))
        (dur (get-duration 1)))
    (setf n (truncate (/ dur avg-ioi)))
    (cond ((< file-dur file-start)
           (error "sf-granulate: file-start is after end of file!"))
          ((< file-dur file-end)
           (error "sf-granulate: file-end (offset) exceeds file duration!"))
          ((< file-dur (+ file-start file-end))
           (error "sf-granulate: file-start + file-end > file duration!")))
    (setf file-dur (- file-dur file-start file-end))
    (setf step-size (/ file-dur n))
    (stretch-abs 1.0 (let ()
      (seqrep (i n) (let ()
        (setf actual-grain-dur (funcall dist))
        (setf env (stretch actual-grain-dur (one-minus-cosine)))
        (force-srate *sound-srate*
          (stretch (real-random 1.0 pitch-dev)
            (sound2
             (set-logical-stop
              (mult (cue env)
                    (s-read filename 
                            :time-offset (+ file-start (* step-size i))
                            :dur actual-grain-dur))
              (real-random ioi (+ ioi ioi-dev))))))))))))

;; How to use these granular-synthesis functions

;; First, make a continuation out of the distribution functions
(defun make-gauss (xmu sigma low high)
	(lambda () (gauss-dist xmu sigma low high)))

;; Second, Plug in that continuation as a variable to the granular-synthesis function
(defun try-len-dist ()
  (play (stretch 4
          (simrep (i 2)
            (len-dist-granulate "samples.wav" 
		(make-gauss 0.0 1.0 0.1 .5) 0.02 0.001 2.0 0 0)))))

;; Here's an example of changing the pitch distribution
(defun make-gamma (nu high)
	(lambda () (gamma-dist nu high)))

(defun try-pitch-dist ()
  (play (stretch 4
          (simrep (i 4)
            (pitch-dist-granulate "samples.wav" 0.04 0.0 0.02 0.001 
		(make-gamma 2.0 5.0) 0 0)))))


;; 2. Simple methods of usuing probability distribution generators
;; In general, a probability distribution generator can substitue for a
;; uniform ranom generator which is (real-random min max)

;; Use a continuous distribution generator to alter the time between sounds
(defun try-exponential ()
	(play (seqrep (i 20)
		(pluck c4 (* 0.5 (exponential-dist .25 2.0))))))

;; Use a discrete generator to alter the pitch by a whole number.
(defun try-binomial ()
	(play (seqrep (i 20)
		(pluck (+ (binomial-dist 6 .5) c4) 0.1))))


(defun dist-hist (fn n nbins low high &rest params)
  (let ((bins (make-array nbins))
        (step (/ (- high low) (float (- nbins 2)))))
    (dotimes (i nbins)
      (setf (aref bins i) 0.0))
    (dotimes (i n)
      (let ((x (apply fn params)) i)
        (cond ((< x low) (incf (aref bins 0)))
              ((>= x high) (incf (aref bins (1- nbins))))
              (t
               (setf i (truncate (1+ (/ (- x low) step))))
               (if (or (< i 1) (>= i (1- nbins)))
                   (error "unexpected bin number"))
               (incf (aref bins i))))))
    bins))


; test LINEAR-DIST
;(setf hist (dist-hist #'linear-dist 10000 100 0 4 4))
;(s-plot (scale 0.001 (snd-from-array 0.0 100 hist)))

; test EXPONENTIAL-DIST
; (setf hist (dist-hist #'exponential-dist 10000 100 0 3 1 3))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test GAMMA-DIST
;(setf hist (dist-hist #'gamma-dist 10000 100 0 10 3 4))
;(s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test BILATERAL-EXPONENTIAL-DIST
; (setf hist (dist-hist #'bilateral-exponential-dist 10000 100 0 10 4.0 1.1 0 10))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test CAUCHY-DIST
; (setf hist (dist-hist #'cauchy-dist 100000 100 -10 10 1.0 -9 6))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test HYPERBOLIC-COSINE-DIST
; (setf hist (dist-hist #'hyperbolic-cosine-dist 1000000 500 -10 10))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test LOGISTIC-DIST
; (setf hist (dist-hist #'logistic-dist 10000 100 -10 10 0.5 2 -5 1))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test GAUSSIAN-DIST
; (setf hist (dist-hist #'gaussian-dist 100000 100 0 10 5 1 2 8))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test BETA-DIST
; (setf hist (dist-hist #'beta-dist 100000 100 -0.1 1.1 0.5 0.25))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test BERNOULLI-DIST
; (setf hist (dist-hist #'bernoulli-dist 10000 100 -0.1 1.1 0.75 0.1 0.9))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test GEOMETRIC-DIST
; (setf hist (dist-hist #'geometric-dist 100000 100 0 10 0.7))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))

; test POISSON-DIST
; (setf hist (dist-hist #'poisson-dist 10000 100 0 20 4))
; (s-plot (scale 1.0 (snd-from-array 0.0 100 hist)))


