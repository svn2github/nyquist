;; tests for alpass filters

(autonorm-off)

;; create a sum of sine tones signal, every 3rd harmonic to get some bw
(defun sine-src ()
  (scale 0.1
    (simrep (i 5)
      (let ()
        ;(display "simrep" *warp*)
        (osc (hz-to-step (* (1+ i) (step-to-hz c4))))))))

(defun osc-src ()
  (scale 0.5 (osc c4)))
        
(defun s-rest-d ()
  (let ()
    ; (display "s-rest-d" *warp*)
    (setf *srest* (s-rest))))
    
; (play (sine-src))

(defun source () (seq (stretch 2 (sine-src)) (s-rest-d)))

; (play (source))
;(play (seq (source) (source) (source) (source)))

; play sound followed by alpassed sound with different parameters

(defun test1 ()
  (seq (source)
       (alpass (source) 0.05 100)
       (alpass (source) 0.5  100)
       (alpass (source) 0.05 1000)
       (alpass (source) 0.5  1000)))
       
;(play (test1))

(defun test2 ()
  (seq (source)
       (alpass (source) 0.05 (pwlv 50 3 200) 50)
       (alpass (source) 0.5 (pwlv 50 3 200) 50)
       (alpass (source) 0.05 (pwlv 250 3 1000) 250)
       (alpass (source) 0.5 (pwlv 250 3 1000) 250)))
       
;(play (test2))

(defun test2b ()
  (seq (source)
       (setf xxx (comb (source) (pwlv 0.02 3 0.1) 100))
       (setf yyy (comb (source) (pwlv 0.2 3 1.0) 100))
       (setf zzz (comb (setf www (source)) 
                         (setf vvv (pwlv 0.02 3 0.1)) 1000))
       (alpass (source) (pwlv 0.2 3 1.0) 1000)))

;(play (test2b))


(defun test3 ()
  (seq (source)
       (setf xxx (alpass (source) (pwlv 0.02 3 0.1) 100))
       (setf yyy (alpass (source) (pwlv 0.2 3 1.0) 100))
       (setf zzz (alpass (setf www (source)) 
                         (setf vvv (pwlv 0.02 3 0.1)) 1000))
       (alpass (source) (pwlv 0.2 3 1.0) 1000)))

;(play (test3))

(defun test4 ()
  (seq (source)
       (alpass (source) (pwlv 0.02 3 0.1) (pwlv 50 3 200) 50)
       (alpass (source) (pwlv 0.2 3 1.0) (pwlv 50 3 200) 50)
       (alpass (source) (pwlv 0.02 3 0.1) (pwlv 500 3 2000) 500)
       (alpass (source) (pwlv 0.2 3 1.0) (pwlv 500 3 2000) 500)))

(play (test4))

(defun pulses () (scale 0.5 (stretch 3 (buzz 400 (hz-to-step 2) (pwl 1)))))
;(play (pulses))

(defun test4 ()
  (comb (pulses) (pwlv 0.6 3 0.1) 1000))

;(play (test4))




