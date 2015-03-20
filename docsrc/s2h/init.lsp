(setf ff (open "s2h.lsp"))
(dotimes (i 100) (setf xx (read ff)) (print xx))
(close ff)

(load "nyinit")
(load "../../sys/unix/osx/system")
(load "do-nyquist-manual")
(exit)
