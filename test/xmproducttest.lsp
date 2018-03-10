;; xmprod.lsp -- run some tests on xm.lsp patterns
;;
;; Roger B. Dannenberg, Mar 2018

;; natural sub-pattern, no for
(setf prp (make-product (make-cycle '(1 2 4))
                    (make-cycle '(3 5 7 9))))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t)))
(xmassert (equal prpout '((3 10 28) (9 6 20) (7 18 12) (5 14 36)))
          "prp-nat-n")

;; for sub-pattern, no for
(setf prp (make-product (make-cycle '(1 2 4) :for 2)
                    (make-cycle '(3 5 7 9 11))))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t t)))
(xmassert (equal prpout '((3 10) (7 18) (11 6) (5 14) (9 22)))
          "prp-for-n")

;; natural sub-pattern, no for, num
(setf prp (make-product (make-cycle '(1 2 4)) 10))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t)))
(xmassert (equal prpout '((10 20 40) (10 20 40) (10 20 40) (10 20 40)))
          "prp-nat-n-num")

;; for sub-pattern, no for, num
(setf prp (make-product (make-cycle '(1 2 4) :for 2) 10))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t t)))
(xmassert (equal prpout '((10 20) (10 20) (10 20) (10 20) (10 20)))
          "prp-for-n-num")



;; natural sub-pattern, for
(setf prp (make-product (make-cycle '(1 2 4))
                    (make-cycle '(3 5 7 9)) :for 5))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t)))
(xmassert (equal prpout '((3 10 28 9 6) (5 14 36 3 10) 
                          (7 18 12 5 14) (9 6 20 7 18)))
          "prp-nat-t")

;; for sub-pattern, for
(setf prp (make-product (make-cycle '(1 2 4) :for 2)
                    (make-cycle '(3 5 7 9)) :for 5))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t t)))
(xmassert (equal prpout '((3 10 7 18 3) (5 14 9 6 5) (7 18 3 10 7) 
                          (9 6 5 14 9) (3 10 7 18 3)))
          "prp-for-t")


;; natural sub-pattern, for, num
(setf prp (make-product (make-cycle '(1 2 4)) 10 :for 5))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t)))
(xmassert (equal prpout '((10 20 40 10 20) (10 20 40 10 20) 
                          (10 20 40 10 20) (10 20 40 10 20)))
          "prp-nat-t-num")

;; for sub-pattern, for, num
(setf prp (make-product (make-cycle '(1 2 4) :for 2) 10 :for 4))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t t)))
(xmassert (equal prpout '((10 20 10 20) (10 20 10 20) (10 20 10 20) 
                          (10 20 10 20) (10 20 10 20)))
          "prp-for-t-num")

;;---- now with patterns ---------

;; natural sub-pattern, forpat
(setf prp (make-product (make-cycle '(1 2 4))
                    (make-cycle '(3 5 7 9)) :for (make-cycle '(1 3 5))))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t)))
(xmassert (equal prpout '((3) (5 14 36) (3 10 28 9 6) (5)))
          "prp-nat-forpat")

;; for sub-pattern, forpat
(setf prp (make-product (make-cycle '(1 2 4) :for 2)
                    (make-cycle '(3 5 7 9)) :for (make-cycle '(1 3 5))))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t t)))
(xmassert (equal prpout '((3) (5 14 9) (3 10 7 18 3) (5) (7 18 3)))
          "prp-for-forpat")

;; natural sub-pattern, forpat, num
(setf prp (make-product (make-cycle '(1 2 -4)) 10  :for (make-cycle '(1 3 5))))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t)))
(xmassert (equal prpout '((10) (10 20 -40) (10 20 -40 10 20) (10)))
          "prp-nat-forpat-num")

;; for sub-pattern, forpat, num
(setf prp (make-product (make-cycle '(1 -2 4) :for 2) 10 
               :for (make-cycle '(1 3 5))))
(setf prpout (mapcar #'(lambda (x) (next prp t)) '(t t t t t)))
(print prpout)
(xmassert (equal prpout '((10) (10 -20 10) (10 -20 10 -20 10) (10) (10 -20 10)))
          "prp-for-forpat-num")

