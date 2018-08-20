;; build html for nyquist manual
(load "latex")
(load "s2h")
(expand 10)
;; sourcedir source       destdir  dest
(g "../nyquist" "nyquistman" "../../doc" "home" t nil)
; (setf *token-trace* t)
(print "**** STARTING SECOND PASS ****")
(g "../nyquist" "nyquistman" "../../doc" "home" t t)
