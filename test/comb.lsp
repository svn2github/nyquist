(load "rbd")

(defun comb-test ()
  ;(comb (seq (noise) (s-rest 10)) 500 5))
  (sim
   (setf dur 5)
   (comb (seq (noise) (s-rest dur)) (step-to-hz c4) dur)
   (comb (seq (noise) (s-rest dur)) (step-to-hz e4) dur)
   (comb (seq (noise) (s-rest dur)) (step-to-hz d5) dur)))

(setf bf3hz (step-to-hz bf3))
(setf c4hz (step-to-hz c4))
(setf bf4hz (step-to-hz bf4))
(setf c5hz (step-to-hz c5))
(setf d5hz (step-to-hz d5))
(setf f5hz (step-to-hz f5))
(setf g5hz (step-to-hz g5))
(setf a5hz (step-to-hz a5))
(setf b5hz (step-to-hz b5))

(defun pwl-step (fr to)
  (pwl 0 fr 15 fr 16 to 35 to 35))

(defun reson-test ()
  (setf dur 4)
  (let (center (snd (s-read "./test/snd/test2.snd")))
    (setf snd (seq (cue snd) (s-rest 5)))
    (setf center (scale 0.01 (at 0.05 (cue snd))))
    (vector
     (sim
      (scale 0.01 snd)
      center
      (reson snd (pwl-step b5hz a5hz) 1)
      (reson snd (pwl-step c5hz bf4hz) 1))
     (sim
      (scale 0.01 (at 0.11 (cue snd)))
      center
      (scale 0.1 (reson snd (pwl-step c4hz bf3hz) 0.5))
      (reson snd (pwl-step g5hz f5hz) 1)
      (reson snd (pwl-step d5hz c5hz) 1)))))


(defun reson-test-1 ()
  (setf dur 4)
  (sim
   (reson (seq (noise) (s-rest dur)) (step-to-hz c5) 1)
   (reson (seq (noise) (s-rest dur)) (step-to-hz g5) 1)
   (reson (seq (noise) (s-rest dur)) (step-to-hz d5) 1)))

(defun convert-file ()
  (s-save (force-srate 22050 (jam-srate (aref (s-read "./test/snd/test1.snd") 0) 48000))
        1000000 "./test/snd/test2.snd"))

(defun jam-srate (snd rate)
  (snd-xform snd rate (snd-time snd) 
             MIN-START-TIME MAX-STOP-TIME 1.0))

;(setf xxx (comb-test))
(defun g () 
  (setf xxx (reson-test))
  (play-xxx))

(defun play-xxx ()
  (cond ((soundp xxx) (setf scale-factor (s-max xxx 1000000)))
        ((arrayp xxx)
         (setf scale-factor 0)
         (dotimes (i (length xxx))
                  (setf scale-factor 
                        (max scale-factor 
                             (s-max (aref xxx i) 1000000)))))
        (t (error "bad type" xxx)))
  (format t "Maximum amplitude before scaling: ~A~%" scale-factor)
  (play (scale (/ scale-factor) xxx)))

(g)
