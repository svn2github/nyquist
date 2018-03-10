;; xmmarkovtest.lsp - make-random (random pattern generator) test
;; 
;; Roger B. Dannenberg
;; Mar, 2018

;; test basic markov chain:
;;    A -> B
;;    B -> A
;;    B -> C
;;    C -> B
;; should visit B twice as much as A or C

(setf mp (make-markov '((A -> B) (B -> A) (B -> C) (C -> B))))
(dotimes (i 30)
  (display "markov" (next mp)))
