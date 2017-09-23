;; a library of simple time delay functions (chorus, phaser, etc.)
;;
;; by Kathy Drye and Roger Dannenberg


;; phaser
;;       The phaser uses all-pass filter to create the delay

(defun phaser (s) (sim s (alpass s 1 20)))

;; an example
;(play (phaser (s-read "example1.wav")))

;; compute the delay-tapv after multichan expanding args
;;
(defun nyq:delay-tapv (sound delay depth rate saturation
                       src phase)
  (let ((modulation (sum delay 
                         (prod depth
                               (lfo rate 10000.0 *sine-table* phase)))))
    ;; add sound with variable delay to sound with fixed delay
    (hp (sum (prod (tapv sound 0.0 modulation (+ delay depth))
                   saturation)
             sound)
        10)))
  

(defun delay-tapv (input-sound delay depth rate saturation 
                   &key (src "DELAY-TAPV") (phase 0.0))
  ;; delay a signal by delay plus a time-varying amount controlled 
  ;; by an LFO (sine) and add to the original sound
  ;; delay + depth must be greater than zero and less than maxdelay
  ;; maxdelay is a scalar
  ;; rate is the frequency of the LFO
  ;; saturation is the amount of modulated signal added to the
  ;; original (normally 0 to 1)
  ;; 
  (multichan-expand src #'nyq:delay-tapv
    '(((SOUND) "input-sound") ((NUMBER) "delay") ((NUMBER) "depth")
      ((NUMBER) "rate") ((NUMBER) "saturation") ((STRING) "src")
      ((NUMBER) "phase"))
    input-sound delay depth rate saturation src phase))
    

;; flanger:
;;         The flanging effect uses a time-varied delay
;;         This version uses 0-20ms delay modulated at 0.2Hz,
;;         with a saturation of 0.8. This flange does not use
;;         feedback.

(defun flange (input-sound)
  (delay-tapv input-sound .01 .01 0.2 0.9 :src "FLANGE"))


       
;; chorus effect
;;
;; chorus:
;;        The chorus effect uses a time-varied delay
;;        The delay is generally a longer delay with an lfo controlling
;;        the delay operating around 0.3Hz


(defun chorus (input-sound &key (delay 0.03) (depth 0.003) 
                                (rate 0.3) (saturation 1.0)
                                (phase 0.0)) 
  (delay-tapv input-sound 
              delay depth rate saturation :src "CHORUS" :phase phase))


(defun stereo-chorus (input-sound &key (delay 0.03) (depth 0.003) 
                                       (rate1 0.3) (rate2 0.1)
                                       (saturation 1.0))
   (sim
       (pan (chorus input-sound :delay delay :depth depth :rate rate1
                    :saturation saturation) .3)
       (pan (chorus input-sound :delay delay :depth depth :rate rate2
                    :saturation saturation :phase 180.0) .7)))


;; examples
;(play (chorus (aref (s-read "example1.wav") 0)))
;
; you can apply different parameters to each channel using delay-tapv, 
; e.g. here the rate is different on the left and right channels 
; (works with mono or stereo input!)
;(play (delay-tapv (s-read "example1.wav") 0.1 0.05 0.005 (vector 0.4 0.1) 0.8))
;
; the STEREO-CHORUS is intended for mono input.
;(play (stereo-chorus (mono-sound))




