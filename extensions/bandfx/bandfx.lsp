;; bandfx -- audio effects based on separate frequency bands
;;
;; Nyquist Extension
;; Version: 1.0;
;;
;; Author Name: Michael Mishkin and Roger B. Dannenberg;
;; Author Email: rbd@cs.cmu.edu;
;; revised to create extension Aug 2018
;;
;; Additional File: autoload.lsp;
;; Additional File: nyquistwords.txt;
;;
;; End Metadata
;;
;; Usage (SAL syntax):
;; 
;;     band-2(), band-3(), band-4(), band-5() are examples
;;
;;     separate-into-bands(s, p, inc, n) - separate s into frequency bands
;;         with exponential frequency spacing from p to p + inc * n, where
;;         p and inc are in steps (not Hz)
;;     separate-into-bands-range(s, low, high, num-bands) -- separate signal
;;         s into num-bands bands from the low to high step (not Hz)
;;     reconstruct-from-bands(bands) -- reconstruct a signal from bands
;;     banded-delay(bands, del, inc, fb, wet) -- apply different delay to 
;;         each band (channel) of bands.  del is the delay for the first 
;;         band, and inc is the difference in delay for each successive 
;;         band. fb is the feedback for all delays.
;;     apply-banded-delay(s, lowp, highp, num-bands, lowd, highd, fb, wet) -
;;         apply banded delay effect to a sound s; lowp, highp gives the 
;;         pitch range for the bands; num-bands is the number of bands;
;;         lowd, highd is the range of delays; fb is the feedback for all 
;;         bands; wet (0 to 1) is the mix of dry and banded delay signals.
;;         If lowd > highd, delay decreases with increasing frequency.
;;     banded-bass-boost(bands, num-boost, gain) - boost the first num-boost
;;         bands by gain.
;;     apply-banded-bass-boost(s, lowp, highp, num-bands, num-boost, gain) -
;;         end-to-end processing using banded-bass-boost.
;;     banded-treble-boost(bands, num-boost, gain) - boost the last num-boost
;;         bands by gain.
;;     apply-banded-treble-boost(s, lowp, highp, num-bands, num-boost, gain) -
;;         end-to-end processing using banded-bass-boost.

;; SEPARATE-INTO-BANDS -- separate sound s into frequency bands with
;; exponential spacing from p to p + inc * n. Filteres have a bandwidth
;; of inc and there are n bands.
;; The last band is not filtered.
;;
(defun separate-into-bands (s p inc n)
  (let (bands width low-freq high-freq)
    (setf bands (make-array n))
    (setf high-freq (step-to-hz p))
    (dotimes (i (1- n))
      (setf low-freq high-freq)
      (setf p (+ p inc))
      (setf high-freq (step-to-hz p))
      (setf width (- high-freq low-freq))
      (setf (aref bands i) (reson s (+ low-freq (* 0.5 width)) width 1))
      (setf s (areson s (+ low-freq (* 0.5 width)) width 1)))
    (setf (aref bands (1- n)) s)
    bands))


;; SEPARATE-INTO-BANDS-RANGE -- separate signal s into num-bands bands
;; from the low to high step
;;
(defun separate-into-bands-range (s low high num-bands)
  (let ((inc (/ (- high low) num-bands)))
    (separate-into-bands s low inc num-bands)))


;; RECONSTRUCT-FROM-BANDS -- reconstruct a signal from bands
;;
(defun reconstruct-from-bands (bands)
  (let ((result (aref bands 0)))
    (dotimes (i (1- (length bands)))
      (setf result (sum result (aref bands (1+ i)))))
    result))

;; BANDED-DELAY -- apply different delay to each band (channel) of bands.
;;  del is the delay for the first band, and inc is the difference in
;;  delay for each successive band. fb is the feedback for all delays.
;;
(defun banded-delay (bands del inc fb wet)
  (let ((result (make-array (length bands))))
    (dotimes (i (length bands))
      (setf (aref result i) 
            (sum (mult (- 1 wet) (aref bands i))
                 (mult wet (feedback-delay (aref bands i) del fb))))
      (setf del (+ del inc)))
    result))

;; APPLY-BANDED-DELAY -- apply banded delay effect to a sound
;;   s is the sound to be processed
;;   lowp, highp is the pitch range for the bands
;;   num-bands is the number of bands
;;   lowd, highd is the range of delays
;;   fb is the feedback (same for all bands)
;;   (note that if lowd > highd, delay decreases with increasing frequency)
;;
(defun apply-banded-delay (s lowp highp num-bands lowd highd fb wet)
  (let (bands inc)
    (reconstruct-from-bands
      (banded-delay (separate-into-bands-range s lowp highp num-bands)
                    lowd (/ (- highd lowd) num-bands) fb wet))))

(defun banded-bass-boost (bands num-boost gain)
  (let ((result (make-array (length bands))))
    (dotimes (i (length bands))
      (setf (aref result i)
            (scale (if (< i num-boost) gain 1.0) 
                   (aref bands i))))
    result))


(defun apply-banded-bass-boost (s lowp highp num-bands num-boost gain)
  (reconstruct-from-bands 
    (banded-bass-boost 
      (separate-into-bands-range s lowp highp num-bands) 
      num-boost gain)))

(defun banded-treble-boost (bands num-boost gain)
  (let ((result (make-array (length bands)))
        (num-unity (- (length bands) num-boost)))
    (dotimes (i (length bands))
      (setf (aref result i)
            (scale (if (< i num-unity) 1.0 gain) 
                   (aref bands i))))
    result))


(defun apply-banded-treble-boost (s lowp highp num-bands num-boost gain)
  (reconstruct-from-bands 
    (banded-treble-boost 
      (separate-into-bands-range s lowp highp num-bands) 
      num-boost gain)))


;; EXAMPLES

;; First, a few helper functions

;; PN-RIFF -- make a sound to which we can add effects
;;
(defun pn-riff ()
  (seq
    (seqrep (i 20)
      (set-logical-stop 
        (piano-note 0.1 (+ (rem (* i 5) 48) c2) 100)
        0.2))
    (s-rest)))

;; Examples: band-2(), band-3(), band-4(), and band-5().


(defun band-2 ()
  (play (apply-banded-delay (pn-riff) c2 120 28 1.0 0.0 0.0 0.2)))

(defun band-3 ()
  (play (apply-banded-delay (pn-riff) c2 120 28 0.0 1.0 0.0 0.2)))

(defun band-4 ()
  (play (scale 0.4 (apply-banded-bass-boost (pn-riff) c2 120 28 5 10))))

(defun band-5 ()
  (play (scale 0.4 (apply-banded-treble-boost (pn-riff) c2 120 28 5 10))))
