;; audio.lsp
;;
;; simple audio output debugging test
;;
(defun test () (stretch (/ 3.0 44100) (control-srate-abs 44100.0 (ramp)))))
(play (vector (test) (test)))

(play (test))

