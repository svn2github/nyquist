;; spectrum.lsp -- operations on spectral frames

(defun raised-cosine ()
  (scale 0.5 
    (sum (const 1) 
         (lfo (/ 1.0 (get-duration 1)) 1 *sine-table* 270))))

(defun fft-window (frame-size)
  (control-srate-abs frame-size (raised-cosine)))

;; fft-class -- an iterator converting sound to sequence of frames
;;
(setf fft-class (send class :new '(sound length skip window)))

(send fft-class :answer :next '() '(
    (snd-fft sound length skip window)))

(send fft-class :answer :isnew '(snd len skp) '(
    (setf sound snd)
    (setf length len)
    (setf skip skp)
    (setf window (fft-window len)) ))


(defun make-fft-iterator (sound length skip)
  (send fft-class :new (snd-copy sound) length skip))


;; conversions -- assumes frame length is even

(defun spectrum-to-amplitude (frame)
  (let* ((n (length frame))
         (half-n (/ n 2))
         (amps (make-array (1+ half-n))))
    (setf (aref amps 0) (abs (aref frame 0)))
    (dotimes (i (1- half-n))
      (let* ((i2 (+ i i))
             (c (aref frame (1+ i2)))
             (s (aref frame (+ 2 i2))))
        (setf (aref amps (1+ i))
              (sqrt (+ (* c c) (* s s))))))
    (setf (aref amps half-n) (abs (aref frame (1- n))))
    amps))

(defun spectrum-by-amplitude (frame amps)
  (let* ((n (length frame))
         (half-n (/ n 2)))
    (setf (aref frame 0) (* (aref frame 0) (aref amps 0)))
    (dotimes (i (1- half-n))
      (let* ((ip1 (1+ i))
             (i2 (+ i i))
             (i2p1 (1+ i2))
             (i2p2 (1+ i2p1)))
        (setf (aref frame i2p1) (* (aref frame i2p1)
                                   (aref amps ip1)))
        (setf (aref frame i2p2) (* (aref frame i2p2)
                                   (aref amps ip1)))))
    (setf (aref frame (1- n)) (* (aref frame (1- n))
                                 (aref amps half-n)))))


(defun spectrum-rms (frame)
  (let* ((n (length frame))
         (half-n (/ n 2))
         (sum (* (aref frame 0) (aref frame 0))))
    (dotimes (i (1- half-n))
      (let* ((i2 (+ i i))
             (c (aref frame (1+ i2)))
             (s (aref frame (+ 2 i2))))
        (setf sum (+ sum (* c c) (* s s)))))
    (setf sum (+ sum (* (aref frame (1- n)) (aref frame (1- n)))))
    (sqrt sum)))


(defun amplitude-rms (frame)
  (let* ((n (length frame))
         (sum 0))
    (dotimes (i n)
      (setf sum (+ sum (* (aref frame i) (aref frame i)))))
    (sqrt sum)))

;; SMOOTH-AMPLITUDE -- simple local averaging to smooth out
;;   an amplitude spectrum. This might be useful to broaden
;;   spectral peaks from partials to better represent vocal 
;;   formants. It would be nice to have a "width" parameter,
;;   but instead, the filter is fixed at (0.25, .5, 0.25)
;;
(defun smooth-amplitude (frame)
  (let* ((len (length frame))
         (lenm1 (1- len))
         (lenm2 (1- lenm1))
         (rslt (make-array (length frame))))
    (setf (aref rslt 0) (+ (* 0.75 (aref frame 0))
                           (* 0.25 (aref frame 1))))
    (dotimes (i lenm2)
      (let* ((ip1 (1+ i))
             (ip2 (1+ ip1)))
        (setf (aref rslt ip1) (+ (* 0.25 (aref frame i))
                                 (* 0.5 (aref frame ip1))
                                 (* 0.25 (aref frame ip2))))))
    (setf (aref rslt lenm1) (+ (* 0.25 (aref frame lenm2))
                               (* 0.75 (aref frame lenm1))))
    rslt))

;; ARRAY-SCALE -- multiply a spectral frame or amplitude frame
;;  by a scale factor
;;
(defun array-scale (frame x)
  (dotimes (i (length frame))
    (setf (aref frame i) (* (aref frame i) x))))


(defun array-copy (frame)
  (let* ((len (length frame))
         (copy (make-array len)))
    (dotimes (i len)
      (setf (aref copy i) (aref frame i)))
    copy))




(defun amplitude-plot (frame)
  (s-plot (snd-from-array 0 
            (/ (float (1- (length frame)))
               *default-sound-srate*) 
            frame)))


(defun spectrum-plot (frame)
  (amplitude-plot (spectrum-to-amplitude frame)))


(defun spectrum-ifft (iterator len skip)
  (snd-ifft (local-to-global 0.0) *sound-srate* iterator skip (fft-window len)))
