;; xmaccumulate.lsp -- run some tests on xm.lsp patterns
;;
;; Roger B. Dannenberg, Mar 2018

;; natural sub-pattern, no for
(setf acc (make-accumulate (make-cycle '(1 2 4))))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 7) (8 10 14) (15 17 21) (22 24 28)))
          "acc-nat-n")

;; for sub-pattern, no for
(setf acc (make-accumulate (make-cycle '(1 2 4) :for 2)))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 3) (4 6) (7 9) (10 12) (13 15))) 
          "acc-for-n")

;; natural sub-pattern, no for, max
(setf acc (make-accumulate (make-cycle '(1 2 4)) :max 16))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 7) (8 10 14) (15 16 16) (16 16 16)))
          "acc-nat-n-max")

;; for sub-pattern, no for, max
(setf acc (make-accumulate (make-cycle '(1 2 4) :for 2) :max 11))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 3) (4 6) (7 9) (10 11) (11 11)))
          "acc-for-n-max")

;; natural sub-pattern, no for, min
(setf acc (make-accumulate (make-cycle '(1 2 -4)) :min -3))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 -1) (0 2 -2) (-1 1 -3) (-2 0 -3)))
          "acc-nat-n-min")

;; for sub-pattern, no for, min
(setf acc (make-accumulate (make-cycle '(1 -2 4) :for 2) :min -3))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 -1) (0 -2) (-1 -3) (-2 -3) (-2 -3)))
          "acc-for-n-min")



;; natural sub-pattern, for
(setf acc (make-accumulate (make-cycle '(1 2 4))))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 7) (8 10 14) (15 17 21) (22 24 28)))
          "acc-nat-t")

;; for sub-pattern, for
(setf acc (make-accumulate (make-cycle '(1 2 4) :for 2)))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 3) (4 6) (7 9) (10 12) (13 15)))
          "acc-for-t")


;; natural sub-pattern, for, max
(setf acc (make-accumulate (make-cycle '(1 2 4)) :max 16))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 7) (8 10 14) (15 16 16) (16 16 16)))
          "acc-nat-t-max")

;; for sub-pattern, for, max
(setf acc (make-accumulate (make-cycle '(1 2 4) :for 2) :max 11))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 3) (4 6) (7 9) (10 11) (11 11)))
          "acc-for-t-max")

;; natural sub-pattern, for, min
(setf acc (make-accumulate (make-cycle '(1 -4 4)) :for 5 :min -8))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 -3 1 2 -2) (-1 -5 -1 0 -4) 
                          (-3 -7 -3 -2 -6) (-5 -8 -4 -3 -7)))
          "acc-nat-t-min")

;; for sub-pattern, for, min
(setf acc (make-accumulate (make-cycle '(1 -4 4) :for 2) :for 5 :min -8))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 -3 -2 -6 -5) (-4 -8 -7 -8 -7) 
                         (-6 -8 -7 -8 -7) (-6 -8 -7 -8 -7) 
                         (-6 -8 -7 -8 -7)))
          "acc-for-t-min")

;;---- now with patterns ---------

;; natural sub-pattern, no for, maxpat
(setf acc (make-accumulate (make-cycle '(1 2 4))
              :max (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 7) (8 10 12) (13 15 15) (10 10 10)))
          "acc-nat-n-maxpat")

;; for sub-pattern, no for, maxpat
(setf acc (make-accumulate (make-cycle '(1 2 4) :for 2)
              :max (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 3) (4 6) (7 9) (10 10) (11 12)))
          "acc-for-n-maxpat")

;; natural sub-pattern, no for, minpat
(setf acc (make-accumulate (make-cycle '(1 2 -4))
              :min (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((10 12 10) (12 14 12) (15 17 15) (16 18 14)))
          "acc-nat-n-minpat")

;; for sub-pattern, no for, minpat
(setf acc (make-accumulate (make-cycle '(1 -2 4) :for 2)
              :min (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((10 10) (12 12) (15 15) (16 14) (15 13)))
          "acc-for-n-minpat")

;; natural sub-pattern, for, maxpat
(setf acc (make-accumulate (make-cycle '(1 2 4))
              :max (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
(xmassert (equal accout '((1 3 7) (8 10 12) (13 15 15) (10 10 10)))
          "acc-nat-t-maxpat")

;; for sub-pattern, for, maxpat
(setf acc (make-accumulate (make-cycle '(1 2 4) :for 2)
              :max (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((1 3) (4 6) (7 9) (10 10) (11 12)))
          "acc-for-t-maxpat")

;; natural sub-pattern, for, minpat
(setf acc (make-accumulate (make-cycle '(1 -4 4)) :for 5
              :min (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t)))
;; note: The accumulate pattern gets reset after 5, and :min
;;   is recomputed every period of 3 and also after 5. You
;;   could update :min only every 5 items by using make-length
;;   to regroup the cycle output into periods of 5 items
(xmassert (equal accout '((10 10 14 15 12) (15 15 19 20 16) 
                          (17 13 17 18 15) (16 12 16 17 13)))
          "acc-nat-t-minpat")

;; for sub-pattern, for, minpat
(setf acc (make-accumulate (make-cycle '(1 -4 4) :for 2) :for 5 
              :min (make-cycle '(10 12 15) :name "maxpat")))
(setf accout (mapcar #'(lambda (x) (next acc t)) '(t t t t t)))
(xmassert (equal accout '((10 10 12 12 15) (16 12 13 12 15) 
                          (16 12 13 12 15) (16 12 13 12 15)
                          (16 12 13 12 15)))
          "acc-for-t-minpat")

