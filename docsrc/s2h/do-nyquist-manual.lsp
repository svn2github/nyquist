;; build html for nyquist manual
(load "s2h")
(expand 10)
;; sourcedir source       destdir  dest
(g "../nyquist" "nyquistman" "../../doc" "home" t)

