;(setf *default-plot-file* "/afs/cs/usr/rbd/tmp/points.dat")
;(setf *default-sf-dir* "/afs/cs.cmu.edu/user/rbd/tmp/")
;(setf *default-sound-file* "test")

(setf *default-plot-file* "/space/rbd/tmp/points.dat")
(setf *default-sf-dir* "/space/rbd/tmp/")
(setf *default-sound-file* "rbd-temp.snd")

(set-sound-srate 22050)
(set-control-srate 2205)

(defun ask (string default)
  (let (inp)
    (format t "~A: [~A]" string default)
    (setf inp (read-line))
    (cond ((equal inp "") default)
          (t (intern inp)))))

(if (ask "turn off audio?" t)
  (defun r () t)    ; turn off audio output
  (print "!!!AUDIO OUTPUT TURNED OFF!!!"))


