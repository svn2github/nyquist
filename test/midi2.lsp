(defun midi2 () ; this was original test file, here wrapped in
                ; defun so that it doesn't execute

;(set-sound-srate 22050.0)

(setf *default-plot-file* "/tmp/points.dat")

(setf my-seq (seq-create))
(setf midifile (open "/afs/andrew.cmu.edu/usr15/sr4r/public/testmidi.mid"))

(seq-read-smf my-seq midifile)

(close midifile)

)


(defun my-note-2 (p)
  (display "my-note-2" p (get-duration 1.0) (local-to-global 0.0))
  (scale 0.2 (osc p)))

(defun my-note (p)
  (display "my-note" p (get-duration 1.0) (local-to-global 0.0))
  (scale 0.2 (mult (pwl 0.1 1 0.9 1 1) (osc p))))

(defun ss () (seq-midi my-seq
                       (note (chan pitch velocity) (my-note pitch))))

(defun ss1 () (seq-midi my-seq
                       (note (chan pitch velocity) (my-note-2 (- pitch 84)))))

(defun ss2 ()
  (sim (at .75 (Stretch .333 (my-note-2 -19)))
       (at 1.0 (stretch .333 (my-note-2 -15)))
       (at 1.5 (stretch .333 (my-note-2 -12)))
       (at 2.0 (stretch .333 (my-note-2 -12)))
       (at 2.75 (stretch .333 (my-note-2 -10)))
       (at 3.625 (stretch .333 (my-note-2 -7)))))

(defun ss3 ()
  (sim (at .75 (Stretch .333 (my-note-2 -19)))
       (at 1.0 (stretch .333 (my-note-2 -15)))))


(defun ss4 ()
  (sim (at .75 (Stretch .333 (my-note 65)))
       (at 1.0 (stretch .333 (my-note 69)))
       (at 1.5 (stretch .333 (my-note 72)))
       (at 2.0 (stretch .333 (my-note 72)))
       (at 2.75 (stretch .333 (my-note 74)))
       (at 3.625 (stretch .333 (my-note 77)))))

(defun ss5 ()
  (seq
           (set-logical-stop (stretch .333 (my-note 65)) .25)
           (set-logical-stop (stretch .333 (my-note 69)) .5)
           (set-logical-stop (stretch .333 (my-note 72)) .5)
           (set-logical-stop (stretch .333 (my-note 72)) .75)
           (set-logical-stop (stretch .333 (my-note 74)) .875)
           (stretch .333 (my-note 77))))

(defun ss6 ()
       (seq (set-logical-stop (stretch .333 (my-note 65)) .5)
           (stretch .333 (my-note 77))))

(defun ss7 ()
       (seq (set-logical-stop (stretch .333 (my-note -19)) .5)
           (stretch .333 (my-note -7))))

(defun lowrates ()
(set-sound-srate 100)
(set-control-srate 100)
)


(defun pulse ()
  (display "pulse" (local-to-global 0.0) (get-duration 1.0))
  (pwl 0 1 1))

(defun t1 () (seq-midi my-seq
                   (note (chan pitch velocity) (pulse))))


;=============================new test for Windows==================


(defun wt ()

        (setf my-seq (seq-create))
        (setf midifile (open "..\\test\\test.gio"))
        (seq-read my-seq midifile)
        (close midifile)
        (seq-midi my-seq (note (chan pitch vel) (my-note pitch)))
)
