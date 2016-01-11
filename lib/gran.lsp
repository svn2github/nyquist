;; GRAN.LSP -- granular synthesis example by Roger B. Dannenberg
;;
;; This is not the ultimate granular synthesis package, so do not
;; consider this to be a stable, permanent addition to the Nyquist
;; library. You can use it as is, or use it as the basis for your
;; own custom variations.

;; ==================================================================
;; Grains are windowed with "raised cosine pulse functions." These
;; are smooth envelopes based on the function (1-cos(2*pi*t))/2.
;; To speed up computation, I save three functions with 20, 200, and
;; 2205 samples. The function one-minus-cosine selects an appropriate
;; envelope based on the duration (stretch) currently in effect.

(defun cos-pulse () (scale 0.5 (sum 1 (lfo 1 1 *sine-table* 270.0))))

;; this will be a 2205 point smooth 1-cos(x) curve:
;;
(setf *cos-pulse-2205* (cos-pulse))

;; this will be a 200 point smooth 1-cos(x) curve:
;;
(setf *cos-pulse-200* (control-srate-abs 200 (cos-pulse)))
(setf *cos-pulse-20*  (control-srate-abs 20 (cos-pulse)))


;; one-minus-cosine -- an envelope based on (1-cos(2pi*t))/2
;;
(defun one-minus-cosine ()
  (let ((max-samps (* *sound-srate* (get-duration 1))))
    (cond ((> max-samps 2205) (sound *cos-pulse-2205*))
          ((> max-samps 200) (sound *cos-pulse-200*))
          (t (sound *cos-pulse-20*)))))

'  (let ((duration (get-duration 1)))
    (scale 0.5 (sum 1 (lfo (/ duration) 1 *sine-table* 270.0))))

;; ==================================================================
;; The granulation is applied to a sound file rather than a sound.
;; This gives us the ability to access the sound file at any point
;; in time, although is is a bit less efficient because we have to
;; reopen the file hundreds or thousands of times. (On the other hand
;; the file data is likely to be cached by the OS, so it goes pretty
;; fast.)
;; Here, we define some functions for getting file information.

(defun sf-srate (filename)
  (s-read filename) ; s-read returns list of info in *rslt*
  (s-read-srate *rslt*))


(defun sf-dur (filename)
  (s-read filename)
  (s-read-dur *rslt*))

;; ============================================================
;; Define some other handy support functions

;; real-random -- pick a random real from a range
;;
(defun real-random (from to)
  (cond ((= from to) from)
          (t
         (+ from 
           (* (random 10000)
              0.0001
              (- to from))))))


;; sound2 -- like SOUND but operates on stereo signal
;;
(defun sound2 (a)
  (cond ((eq (type-of a) 'array)
         (vector (sound (aref a 0)) (sound (aref a 1))))
        (t
         (sound a))))


(defun monoize (v)
 (cond ((eq (type-of v) 'array) (aref v 0))
       (t v)))

;; ==================================================================
;; sf-granulate -- granular synthesis applied to file
;;
;; filename -- name of the file
;; grain-dur -- the duration of a grain
;; grain-dev -- grain dur is actually grain-dur + random(0, grain-dev)
;; ioi -- the basic inter-onset-interval for grains
;; ioi-dev -- ioi is actually: ioi + random(0, ioi-dev)
;; pitch-dev -- grains are resampled at rate between 1 and pitch-dev
;; file-start -- when to start reading the file (an offset from start)
;; file-end -- when to stop reading the file (an offset from end)
;; 
;; NOTES: the number of grains is based on an average grain spacing
;;  of (ioi + ioi-dev/2). The step through the file is computed
;;  by dividing the duration (file-start - file-end) by number of
;;  grains.
;;
(defun sf-granulate (filename grain-dur grain-dev ioi ioi-dev pitch-dev 
                     &optional (file-start 0) (file-end 0))
  (let (orig n step-size
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
    ;(display "sf-granulate" step-size file-dur n)
    (stretch-abs 1.0
     (set-logical-stop
      (seqrep (i n)
        (let* ((actual-grain-dur
               (real-random grain-dur (+ grain-dur grain-dev)))
              (env (stretch actual-grain-dur (one-minus-cosine)))
              (pitch-ratio (real-random 1.0 pitch-dev)))
        ;(display "gran" (local-to-global 0) i pitch-ratio)
        (set-logical-stop
          (force-srate *sound-srate*
            (stretch pitch-ratio
              (sound2
                (mult (cue env)
                      (s-read filename 
                              :time-offset (+ file-start (* step-size i))
                              :dur actual-grain-dur)))))
          (real-random ioi (+ ioi ioi-dev)))))
      dur))))

;;============================================================================
;; Here is a sample application of sf-granulate.
;; Notice that I am using simrep to mix four copies of sf-granulate output.
;; Since there are random timings involved, the layers are not identical.
;;
(setf *granfile* "../demos/audio/demo-snd.aiff")

(defun gran-test ()
  (play (stretch 4
          (simrep (i 4)
            (sf-granulate *granfile* 0.04 0.0 0.02 0.001 2.0 0 0)))))


(print "Set *granfile* and then call gran-test for an example")

