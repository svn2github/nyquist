;; some tones for testing Open Sound Control for controlling Nyquist sounds

(osc-enable t)
(autonorm-off)
(snd-set-latency 0.1)

;; TEST 1 -- using hzosc
(defun hzosc-osc ()
  (hzosc (sum 500 (mult 500 (lp (snd-slider 0 (local-to-global 0)
                                            *default-sound-srate* 10) 2.0)))))


; (play (hzosc-osc))


;; TEST 2 -- using granular synthesis
(if (not (boundp '*granfile*))
    (load "gran.lsp"))


;; this is modified from gran.lsp to allow pitch-dev to be continuous
;;
(defun sf-granulate (filename grain-dur grain-dev ioi ioi-dev pitch-dev-factor 
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
    ;(display "sf-granulate" step-size file-dur n)
    (stretch-abs 1.0 (let ()
      (seqrep (i n) (let ()
        (setf actual-grain-dur (real-random grain-dur (+ grain-dur grain-dev)))
        (setf env (stretch actual-grain-dur (one-minus-cosine)))
        (force-srate *sound-srate*
          (stretch (real-random (+ 0.1 (* pitch-dev-factor (get-slider-value 0)))
                                1)
            (sound2
             (set-logical-stop
              (mult (cue env)
                    (s-read filename 
                            :time-offset (+ file-start (* step-size i))
                            :dur actual-grain-dur))
              (real-random ioi (+ ioi ioi-dev))))))))))))


(defun gran-osc ()
  (scale 0.8 
   (stretch 4 ; total is stretch * 10 seconds, i.e. 4 -> 40s 
    (simrep (i 4)
            (sf-granulate *granfile* 0.04 0.0 0.02 0.001 10 0 0)))))

;(play (gran-osc))

;; TEST 3 - piano sequence

(defun piano-osc ()
  (if (not (fboundp 'piano-note))
      (load "pianosyn"))
  (seqrep (i 200) (piano-note (+ 0.05 (* 0.2 (get-slider-value 1)))
                               (round (+ (real-random c1 c2)
                                         (* (get-slider-value 0) 60))) 
                              100)))

;; TEST 4 - minimoog filter sweep

(defun moog-osc ()
  (setf *moog-dur* 10)
  (let ((cutoff (lp (snd-slider 0 (local-to-global 0)
                                *default-sound-srate* *moog-dur*) 2.0))
        bandwidth)
    (setf cutoff (scale 2000 cutoff))
    (setf cutoff (snd-maxv cutoff (const 20 *moog-dur*)))
    (mult (pwl 1 1 (- *moog-dur* 1) 1 *moog-dur*)
          (scale 2 (reson (stretch *moog-dur* (osc-saw 50)) 
                          cutoff (mult cutoff 0.1) 2)))))

; (scale 1000 (ramp 5)))) ; (* 1000.0 (get-slider-value 0))))

(format t "(play (hzosc-osc)) -- play oscillator controlled by slider 0\n")
(format t "(play (gran-osc)) -- play granular synthesis controlled by slider 0\n")
(format t "(play (piano-osc)) -- play piano sequence controlled by slider 0, duration by slider 1\n")
(format t "(play (moog-osc)) -- play minimoog with filter controlled by slider 0\n")
