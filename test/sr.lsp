;; scott raymond's midi code

(set-sound-srate 22050.0)

(setf my-seq (seq-create))
(setf midifile (open "/afs/andrew.cmu.edu/usr15/sr4r/public/testmidi.mid"))

(seq-read-smf my-seq midifile)

(close midifile)

(defun my-note (p) (scale 0.2(osc p)))

(play (seq-midi my-seq
           (note (chan pitch velocity) (my-note pitch))))

(defun (srl) () (load "test/sr"))
