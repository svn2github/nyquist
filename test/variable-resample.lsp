;; variable resample test

(set-sound-srate 200.0)

(defun test ()
  (sound-warp (pwl 10 10 10) (stretch 10 (hzosc 5)) 10.0))

(play (test))

