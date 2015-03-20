;; build html for nyquist manual
(load "md")
(load "s2h")
(expand 10)
;; sourcedir source       destdir  dest
(setf *md* nil)
(g "../nyquist" "nyquistman" "../../doc" "home" t)
(setf *md* t) ;; turn on meta-doc output
; (setf *token-trace* t)
(g "../nyquist" "nyquistman" "../../doc" "home" t)
