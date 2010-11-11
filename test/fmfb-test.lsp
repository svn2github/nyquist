; SND-FMFB ARGS: t0 hz sr index dur

(defun feedback-fm (pitch index d)
  (let ((hz (step-to-hz (+ pitch (get-transpose))))
        (dur (get-duration d)))
    (snd-fmfb (local-to-global 0)  hz *sound-srate*  index dur))) 


(play (feedback-fm a4  1.1 1))

(exit)



