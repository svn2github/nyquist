;;; SIMPLE SYNTHESIS
;;; Waveform + Envelope. Modulating the envelope with noise
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(defun shiver (dur frq noise-percent noise-frq)
  (mult (osc frq dur)
        (sum (pwlv 5e-2 300e-3 1.0 (- dur 300e-3) 1.0 dur 2e-3)
             (mult (/ noise-percent 100.0) (randi1 noise-frq dur)))))

; when noise-percent is too big (> 40), there is a click risk at the
; beginning and the end of the note
; this would be avoided if randi function were multiplied by a smooth envelope
; WARNING: randi1 is defined in PJMG.LSP

(ss (seq (shiver 1 c5 20 40)
         (shiver 1 b4 50 40)
         (shiver 1 a4 80 40)
         (shiver 1 g4 20 300)
         (shiver 1 f4 50 300)
         (shiver 1 d4 80 300)))
