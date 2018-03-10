;; xmevaltest.lsp - make-eval (eval pattern generator) test
;; 
;; Roger B. Dannenberg
;; Mar, 2018

(setf a 10 b 20 c 30 d 40)

(setf evalp (make-eval 'xx))
(setf evalpp nil)
(dotimes (i 20)
  (setf xx i)
  (setf evalpp (cons (next evalp) evalpp)))
(setf evalpp (reverse evalpp))
(print evalpp)
(xmassert (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) evalpp)
          "evalpp")
(setf xx 20)
(setf evalpp (next evalp t))
(xmassert (equal nil evalpp) "evalpp2")
(setf evalpp (next evalp t))
(xmassert (equal '(20) evalpp) "evalpp2")

;; use eval-pattern
(setf evalfp (make-eval (make-cycle '(a b c d))))
(setf evalfpp (next evalfp t))
(xmassert (equal '(10) evalfpp) "evalpp1")
(setf evalfpp (next evalfp t))
(xmassert (equal '(20) evalfpp) "evalpp2")
(setf evalfpp (next evalfp t))
(xmassert (equal '(30) evalfpp) "evalpp3")
(setf evalfpp (next evalfp t))
(xmassert (equal '(40) evalfpp) "evalpp4")
(setf evalfpp (next evalfp t))
(xmassert (equal '(10) evalfpp) "evalpp5")

;; use eval pattern and for
(setf evalfp (make-eval (make-cycle '(a b c d)) :for 3))
(setf evalfpp (next evalfp t))
(xmassert (equal '(10 20 30) evalfpp) "evalfp1")
(setf evalfpp (next evalfp t))
(xmassert (equal '(40 10 20) evalfpp) "evalfp2")

;; use eval pattern and for pattern
(setf evalfp (make-eval (make-cycle '(a b c d)) :for (make-cycle '(2 3))))
(setf evalfpp (next evalfp t))
(xmassert (equal '(10 20) evalfpp) "evalfpp1")
(setf evalfpp (next evalfp t))
(xmassert (equal '(30 40 10) evalfpp) "evalfpp2")
(setf evalfpp (next evalfp t))
(xmassert (equal '(20 30) evalfpp) "evalfpp3")
