(defun test ()
  (stretch 3
     (sim (at 0 (mynote))
          (at 1 (mynote))
          (at 2 (mynote)))))

(defun mynote ()
  (display "mynote" (local-to-global 0))
  (stretch-abs 1
               (snd-compose (s-read "tri.snd")
                            (ramp))))

(set-sound-srate 100)
(set-control-srate 100)

(setf *plotscript-file* "../sys/unix/rs6k/plotscript")


(defun kuu-up (dur rate)
 (stretch-abs 1
  (snd-compose (kuu dur)
               (scale (* 0.5 rate)
                 (sum
                  (pwe 7 7 7)
                  (control-srate-abs 22050 (pwl 7 7 7)))))))
