;; a library of simple time delay functions (chorus, phaser, etc.)
;;
;; by Kathy Drye and Roger Dannenberg


;; phaser
;;       The phaser uses all-pass filter to create the delay

(defun phaser (s) (sim s (alpass s 1 20)))

;; an example
;(play (phaser (s-read "example1.wav")))


;; nyq:snd-tapv -- handles multichannel input sounds
;;
(defun nyq:snd-tapv (sound offset modulation maxdepth)
  (multichan-expand #'snd-tapv sound offset modulation maxdepth))


;; flanger
;;        Use the snd-tapv function which uses a variable delay

(defun delay-tapv (sound maxdepth depth rate saturation)
  ;; delay a signal by a time-varying amount controlled by an LFO (sine)
  ;;    and add to the original sound
  ;; the result is delayed by maxdepth/2
  ;; maxdepth is twice the delay and at least twice the depth
  ;; depth is the depth of modulation in seconds
  ;; rate is the frequency of the LFO
  ;; saturation is a linear pan between the original and the modulated signal
  ;; 
  (let ((modulation (prod depth (stretch-abs 10000.0 (lfo rate))))
        (offset (/ maxdepth 2.0)))
    ;; add sound with variable delay to sound with fixed delay
    (sum (prod (nyq:snd-tapv sound offset modulation maxdepth)
               saturation)
         (prod (feedback-delay sound offset 0.0)
               (sum 1.0 (prod -1.0 saturation))))))

;; flanger:
;;         The flanging effect uses a time-varied delay
;;         The delay is generally +/-30 ms with an lfo controlling the delay
;;         opearting around 0.3Hz.

(defun flange (input-sound)
  (delay-tapv input-sound .06 .03 .3 .5))


       
;; chorus effect
;;
;; chorus:
;;        The chorus effect uses a time-varied delay
;;        The delay is generally a longer delay with an lfo controlling
;;        the delay operating around 0.3Hz


(defun chor-tapv (input-sound) 
  (delay-tapv (input-sound) .1 .05 .3 .8))


(defun stereo-chorus (input-sound)
   (sim
       (pan (delay-tapv input-sound .1 .05 .4 .8) .3)
       (pan (delay-tapv input-sound .1 .05 .1 .8) .7)))

(defun nyq:chorus (sound maxdepth depth rate saturation)
  (let ((modulation (prod depth (stretch-abs 10000.0 (lfo rate))))
        chor)
    (setf chor (snd-tapv sound maxdepth modulation maxdepth))
    (sum (prod chor saturation) 
         (prod (seq (s-rest maxdepth) (cue sound))
               (sum 1.0 (prod -1.0 saturation))))))

(defun chorus (sound maxdepth depth rate saturation)
  (multichan-expand #'nyq:chorus sound maxdepth depth rate saturation))


;; examples
;(play (chorus (aref (s-read "example1.wav") 0) 0.05 0.025 0.5 0.5))
;
; you can apply different parameters to each channel, e.g. here the rate
; is different on the left and right channels (works with mono or stereo
; input!)
;(play (chorus (s-read "example1.wav") 0.1 0.05 (vector 0.4 0.1) 0.8))
;
; the STEREO-CHORUS is intended for mono input.
;(play (stereo-chorus (mono-sound))




