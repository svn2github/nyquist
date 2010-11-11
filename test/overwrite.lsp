;; overwrite test

;; 1) add sine to existing sine
;;
(defun ow-test-1 ()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite1.wav" :format snd-head-wave 
                               :mode snd-mode-pcm :bits 16)
  (print "called s-save with overwrite1.wav")
  (s-add-to (scale 0.5 (osc bf4)) ny:all "overwrite1.wav")
  (print "called s-add-to with overwrite1.wav")
  (play-file "overwrite1.wav")
  )

;; 2) add sine to existing sine, extend beyond previous duration
;;
(defun ow-test-2 ()
  (print "calling s-save with overwrite2.wav")
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite2.wav" :format snd-head-wave 
                               :mode snd-mode-pcm :bits 16)
  (print "called s-save with overwrite2.wav")
  (s-add-to (scale 0.5 (osc bf4 2)) ny:all "overwrite2.wav")
  (play-file "overwrite2.wav")
  )


;; 3) add sine to existing sine, end within existing sound
(defun ow-test-3 ()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite3.wav" :format snd-head-wave 
                               :mode snd-mode-pcm :bits 16)
  (s-add-to (mult (pwl 0 1 0.5 1 0.51) 0.5
                  (osc bf4)) ny:all "overwrite3.wav" 0.25)
  (play-file "overwrite3.wav")
  )


;; 4) add sine beyond previous duration (extend by adding zeros first)
(defun ow-test-4 ()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite4.wav" :format snd-head-wave 
                               :mode snd-mode-pcm :bits 16)
  (s-add-to (s-rest 3) ny:all "overwrite4.wav")
  (s-add-to (mult (pwl 0 0.5 0.5 0.5 0.51)
                  (osc bf4)) ny:all "overwrite4.wav" 2)
  (play-file "overwrite4.wav")
  )


;; 5) (1) with offset, and extend beyond previous duration
(defun ow-test-5 ()
  (s-save (mult (pwl 0 0.5 0.99 0.5 1.0) (osc c4)) 
          ny:all "overwrite5.wav" :format snd-head-wave 
          :mode snd-mode-pcm :bits 16)
  (s-add-to (mult (pwl 0.01 0.5 0.99 0.5 1) (osc bf4)) 
            ny:all "overwrite5.wav" 0.5)
  (play-file "overwrite5.wav")
  )

;; 6) (1) with floats
(defun ow-test-6 ()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite6.wav" :format snd-head-wave
                               :mode snd-mode-float)
  (s-add-to (scale 0.5 (osc bf4)) ny:all "overwrite6.wav")
  (play-file "overwrite6.wav")
  )

;; 7) (2) with floats
;; add sine to existing sine, extend beyond previous duration
;;
(defun ow-test-7 ()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite7.wav" :format snd-head-wave
                               :mode snd-mode-float)
  (s-add-to (scale 0.5 (osc bf4 2)) ny:all "overwrite7.wav")
  (play-file "overwrite7.wav")
  )

;; 8) (3) with raw floats
;; add sine to existing sine, end within existing sound
(defun ow-test-8()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite8.wav" :format snd-head-wave 
                               :mode snd-mode-float)
  (s-add-to (mult (pwl 0 1 0.5 1 0.51) 0.5
                  (osc bf4)) ny:all "overwrite8.wav" 0.25)
  (play-file "overwrite8.wav")
  )

;; 9) (4) with floats
;; add sine beyond previous duration (extend by adding zeros first)
(defun ow-test-9 ()
  (s-save (scale 0.5 (osc c4)) ny:all "overwrite9.wav" :format snd-head-wave
                               :mode snd-mode-float)
  (s-add-to (s-rest 3) ny:all "overwrite9.wav")
  (s-add-to (mult (pwl 0 0.5 0.5 0.5 0.51)
                  (osc bf4)) ny:all "overwrite9.wav" 2)
  (play-file "overwrite9.wav")
  )


;; 10) (5) wtih floats
;; overwrite with offset, and extend beyond previous duration
(defun ow-test-10 ()
  (s-save (mult (pwl 0 0.5 0.99 0.5 1.0) (osc c4)) 
          ny:all "overwrite10.wav" :format snd-head-wave 
          :mode snd-mode-float)
  (s-add-to (mult (pwl 0.01 0.5 0.99 0.5 1) (osc bf4)) 
            ny:all "overwrite10.wav" 0.5)
  (play-file "overwrite10.wav")
  )

;; 11) overwrite to a raw file of floats
(defun ow-test-11 ()
  (s-save (scale 0.5 (osc c4))
          ny:all "overwrite11.raw" :format snd-head-raw
          :mode snd-mode-float :bits 32)
  (print (snd-overwrite '(scale 0.5 (osc bf4 0.4))
                        ny:all "/tmp/overwrite11.raw" 0.3
          SND-HEAD-RAW SND-MODE-FLOAT 32 0))
  (display "ow-test-11" *rslt*)
  (play (s-read "overwrite11.raw" :format snd-head-raw
                :mode snd-mode-float :bits 32)))
