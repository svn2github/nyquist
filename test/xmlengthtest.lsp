;; xmlengthtest.lsp - make-length (length pattern generator) test
;; 
;; Roger B. Dannenberg
;; Feb, 2018

;; length test

(setf lenp (make-length (make-cycle '(a b c d)) 5))
(setf lenpp (next lenp t))
(xmassert (equal '(a b c d a) lenpp) "lenpp")
(setf lenpp (next lenp t))
(xmassert (equal '(b c d a b) lenpp) "lenpp2")

;; use length-pattern
(setf lenfp (make-length (make-cycle '(a b c d)) (make-cycle '(5 6))))
(setf lenfpp (next lenfp t))
(xmassert (equal '(a b c d a) lenfpp) "lenfpp")
(setf lenfpp (next lenfp t))
(xmassert (equal '(b c d a b c) lenfpp) "lenfpp2")
(setf lenfpp (next lenfp t))
(xmassert (equal '(d a b c d) lenfpp) "lenfpp3")

