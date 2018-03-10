;; xmsum.lsp -- run some tests on xm.lsp patterns
;;
;; Roger B. Dannenberg, Mar 2018

;; natural sub-pattern, no for
(setf smp (make-sum (make-cycle '(1 2 4))
                    (make-cycle '(3 5 7 9))))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t)))
(xmassert (equal smpout '((4 7 11) (10 5 9) (8 11 7) (6 9 13)))
          "smp-nat-n")

;; for sub-pattern, no for
(setf smp (make-sum (make-cycle '(1 2 4) :for 2)
                    (make-cycle '(3 5 7 9 11))))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t t)))
(xmassert (equal smpout '((4 7) (8 11) (12 5) (6 9) (10 13)))
          "smp-for-n")

;; natural sub-pattern, no for, num
(setf smp (make-sum (make-cycle '(1 2 4)) 10))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t)))
(xmassert (equal smpout '((11 12 14) (11 12 14) (11 12 14) (11 12 14)))
          "smp-nat-n-num")

;; for sub-pattern, no for, num
(setf smp (make-sum (make-cycle '(1 2 4) :for 2) 10))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t t)))
(xmassert (equal smpout '((11 12) (11 12) (11 12) (11 12) (11 12)))
          "smp-for-n-num")



;; natural sub-pattern, for
(setf smp (make-sum (make-cycle '(1 2 4))
                    (make-cycle '(3 5 7 9)) :for 5))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t)))
(xmassert (equal smpout '((4 7 11 10 5) (6 9 13 4 7)
                          (8 11 7 6 9) (10 5 9 8 11)))
          "smp-nat-t")

;; for sub-pattern, for
(setf smp (make-sum (make-cycle '(1 2 4) :for 2)
                    (make-cycle '(3 5 7 9)) :for 5))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t t)))
(xmassert (equal smpout '((4 7 8 11 4) (6 9 10 5 6) (8 11 4 7 8) 
                          (10 5 6 9 10) (4 7 8 11 4)))
          "smp-for-t")


;; natural sub-pattern, for, num
(setf smp (make-sum (make-cycle '(1 2 4)) 10 :for 5))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t)))
(xmassert (equal smpout '((11 12 14 11 12) (11 12 14 11 12) 
                          (11 12 14 11 12) (11 12 14 11 12)))
          "smp-nat-t-num")

;; for sub-pattern, for, num
(setf smp (make-sum (make-cycle '(1 2 4) :for 2) 10 :for 4))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t t)))
(xmassert (equal smpout '((11 12 11 12) (11 12 11 12) (11 12 11 12) 
                          (11 12 11 12) (11 12 11 12)))
          "smp-for-t-num")

;;---- now with patterns ---------

;; natural sub-pattern, forpat
(setf smp (make-sum (make-cycle '(1 2 4))
                    (make-cycle '(3 5 7 9)) :for (make-cycle '(1 3 5))))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t)))
(xmassert (equal smpout '((4) (6 9 13) (4 7 11 10 5) (6)))
          "smp-nat-forpat")

;; for sub-pattern, forpat
(setf smp (make-sum (make-cycle '(1 2 4) :for 2)
                    (make-cycle '(3 5 7 9)) :for (make-cycle '(1 3 5))))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t t)))
(xmassert (equal smpout '((4) (6 9 10) (4 7 8 11 4) (6) (8 11 4)))
          "smp-for-forpat")

;; natural sub-pattern, forpat, num
(setf smp (make-sum (make-cycle '(1 2 -4)) 10  :for (make-cycle '(1 3 5))))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t)))
(xmassert (equal smpout '((11) (11 12 6) (11 12 6 11 12) (11)))
          "smp-nat-forpat-num")

;; for sub-pattern, forpat, num
(setf smp (make-sum (make-cycle '(1 -2 4) :for 2) 10 
               :for (make-cycle '(1 3 5))))
(setf smpout (mapcar #'(lambda (x) (next smp t)) '(t t t t t)))
(print smpout)
(xmassert (equal smpout '((11) (11 8 11) (11 8 11 8 11) (11) (11 8 11)))
          "smp-for-forpat-num")

