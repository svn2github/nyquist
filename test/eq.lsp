;; test file for eq-band function

;; NOTE: eq-band is happy if you give it all numerical arguments or if you give
;; it a set of time-varying arguments. It will not run with a mixture of scalar
;; and SOUND arguments. Use the Nyquist function CONST to coerce a scalar into
;; a constant-valued SOUND. Be careful to note that CONST returns to zero at the
;; default stop time, e.g. (const 5) lasts 1 second, (stretch 2 (const 5)) lasts
;; 2 seconds, etc.

(play (eq-band (scale 0.1 (noise)) 1000 30 0.3)) ;; 20 dB gain

(play (eq-band (scale 0.1 (noise)) (const 1000) (const 30) (const 0.3)))

; different code is executed if the source has no scale factor...
(play (scale 0.1 (eq-band (noise) (const 1000) (const 30) (const 0.3))))

(play (eq-band (scale 0.1 (noise)) 
               (pwlv 800 1 1200) ; center frequency
               (const 30) 
               (const 0.3)))

(play (stretch 5 
       (eq-band (scale 0.1 (noise)) 
                (const 1000) 
                (pwlv -30 1 30) ; gain
                (const 0.3))))

(play (stretch 5 
       (eq-band (scale 0.1 (noise)) 
                (const 1000) 
                (const 30) 
                (pwev 2 1 0.1)))) ; bandwidth in octaves

