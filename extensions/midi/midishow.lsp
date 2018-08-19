(defun midi-show-file (score-name &optional (out-file t))
  (let ((infile (open-binary score-name :direction :input)))
    (setf my-seq (seq-create))
    (seq-read-smf my-seq infile)
    (close infile)
    (midi-show my-seq out-file)))


;iterate over midi sequence and prints events
;
(defun midi-show (the-seq &optional (out-file t))
  (prog (event)
    (seq-reset the-seq)
loop
    (setf event (seq-get the-seq))
    (if (eq (car event) seq-done-tag)
        (go exit))
    (midi-show-event event out-file)
    (seq-next the-seq)
    (go loop)
exit
  ))

; midi-show-event -- ascii format an event
;
(defun midi-show-event (ev file)
  (let ((tag (seq-tag ev)))
    (cond ((= tag seq-note-tag)
               (format file "Note@~A ch:~A pitch:~A vel:~A line:~A dur:~A~%"
                           (seq-time ev) (seq-channel ev) (seq-pitch ev) (seq-velocity ev)
                           (seq-line ev) (seq-duration ev)))
              ((= tag seq-ctrl-tag)
               (format file "Ctrl@~A ch:~A num:~A val:~A line:~A~%"
                                  (seq-time ev) (seq-channel ev) (seq-control ev)
                                  (seq-value ev) (seq-line ev)))
              ((= tag seq-touch-tag)
               (format file "Aftr@~A ch:~A val:~A line:~A~%"
                                  (seq-time ev) (seq-channel ev) (seq-touch ev) (seq-line ev)))
              ((= tag seq-bend-tag)
               (format file "Bend@~A ch:~A val:~A line:~A~%"
                                  (seq-time ev) (seq-channel ev) (seq-bend ev) (seq-line ev)))
              ((= tag seq-prgm-tag)
               (format file "Prgm@~A ch:~A num:~A line:~A~%"
                                  (seq-time ev) (seq-channel ev) (seq-program ev) (seq-line ev)))
              ((= tag seq-other-tag)
               (format file "Othr~%"))
              (t
               (format file "????: ~A~%" ev)))))
