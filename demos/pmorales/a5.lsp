;;; SIMPLE SYNTHESIS
;;; Waveform + Envelope. Modulating the frequency
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(defun whiny (dur frq)
  (let ((lfo-f (step-to-hz frq)))
    (mult (pwl 0.1 1 (- dur 0.1) 1 dur)
          (fmosc frq (pwl (* 0.1 dur) (/ lfo-f -2.0)
                          (* 0.25 dur) (* lfo-f 2.0)
                          (* 0.3 dur) (* lfo-f 1.5)
                          (* 0.7 dur) (* lfo-f -7.0 (/ 8.0))
                          dur (* lfo-f -15.0 (/ 16.0))
                          ))))) 

 (ss (whiny 10 a5))
