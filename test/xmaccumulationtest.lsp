;; xmaccumulationtest.lsp -- run some tests on xm.lsp patterns
;;
;; Roger B. Dannenberg, Feb 2018

(setf acp (make-accumulation '(a b c d e)))

(setf acpout nil)
(dotimes (i 4)
  (setf acpout (cons (next acp t) acpout)))
(xmassert (equal acpout '((A A B A B C A B C D A B C D E) 
                          (A A B A B C A B C D A B C D E) 
                          (A A B A B C A B C D A B C D E) 
                          (A A B A B C A B C D A B C D E))) "acp")


(setf acpcp (make-accumulation
             (make-cycle (list (make-cycle '(a b c)) (make-cycle '(d e f))))))

(setf acpcpout nil)
(dotimes (i 4)
  (setf acpcpout (cons (next acpcp t) acpcpout)))
(setf acpcpout (reverse acpcpout))
(print acpcpout)
(xmassert (equal acpcpout '((A A B A B C) 
                            (D D E D E F) 
                            (A A B A B C) 
                            (D D E D E F))) "acpcp")

