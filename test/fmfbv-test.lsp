; SND-FMFBV ARGS: t0 hz sr index-sound

; index > 1.1 gets noisy

(defun feedbackv-fm (pitch index-sound)
  (let ((hz (step-to-hz (+ pitch (get-transpose)))))
    (snd-fmfbv (local-to-global 0)  hz *sound-srate*  index-sound))) 


(play (seq
        (mult (pwl 5.0 1.0 10.0)
	      (feedbackv-fm a4  (pwl  10 1.1 10.01)))
	(mult (pwl 5.0 1.0 10.0)
	      (feedbackv-fm a3  (pwlv 1.1 10 0.0)))))

(exit)



