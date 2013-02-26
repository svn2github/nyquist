; init.lsp -- default Nyquist startup file
(load "nyinit.lsp")

; add your customizations here:
;    e.g. (setf *default-sf-dir* "...")
(setf *default-sf-dir* "/space/rbd/tmp/")
;(load "tp")

;(load "gab")

; here's where the error occurs:
;(play (trumpet c4))


;(set-sound-srate 4)
;(set-control-srate 4)
;(Defun xx () (snd-samples (sum (set-logical-stop (osc c4 3) 2) (const 10)) 20))
;(defun yy () (snd-samples (sum (osc c4 2) (const 10)) 20))
;(defun zz () (snd-samples (snd-prod (set-logical-stop (osc c4 3) 2) (stretch 3 (const 10))) 20))
;(defun ww () (snd-samples (snd-prod (osc c4 3) (stretch 3 (const 10))) 20))

; here's where the error occurs (at sample rate of 4)
;(play (trumpet c4))
;(load "natbug")
;(load "sampler-bomb")
