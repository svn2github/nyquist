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

(setf mp (make-markov '((A -> B) (B -> A C) (C -> B)) :past '(B)))
(setf mpseq nil)
(setf mpn 100)
(dotimes (i mpn)
  (setf mpseq (cons (next mp) mpseq)))
(setf mpseq (reverse mpseq))

(defun mpcount (lis item)
  (let ((n 0))
    (mapcar (lambda (x) (if (eql x item) (incf n))) lis)
    n))

(defun mpcheck (lis)
  (cond ((null (cdr lis)) t)
        (t (let ((one (first lis)) (two (second lis)))
             (cond ((and (eq one 'A) (eq two 'B)) t)
                   ((and (eq one 'B) (eq two 'A)) t)
                   ((and (eq one 'B) (eq two 'C)) t)
                   ((and (eq one 'C) (eq two 'B)) t)
                   (t nil))))))

(xmassert (not (member nil (maplist #'mpcheck mpseq))) "mpseq")

(setf acount (mpcount mpseq 'A))
(setf bcount (mpcount mpseq 'B))
(setf ccount (mpcount mpseq 'C))
(xmassert (< acount (/ mpn 2)) "mpseq <a")
(xmassert (< bcount mpn) "mpseq <b")
(xmassert (< ccount (/ mpn 2)) "mpseq <c")
(xmassert (> acount (/ mpn 8)) "mpseq >a")
(xmassert (> bcount (/ mpn 4)) "mpseq >b")
(xmassert (> ccount (/ mpn 8)) "mpseq >c")

;; markov with probabilities on transitions

(setf mpr (make-markov '((A -> B) (B -> (A 10) C) (C -> B)) :past '(B)))
(setf mprseq nil)
(setf mprn 200)
(dotimes (i mprn)
  (setf mprseq (cons (next mpr) mprseq)))
(setf mprseq (reverse mprseq))


(defun mprcheck (lis)
  (cond ((null (cdr lis)) t)
        (t (let ((one (first lis)) (two (second lis)))
             (cond ((and (eq one 'A) (eq two 'B)) t)
                   ((and (eq one 'B) (eq two 'A)) t)
                   ((and (eq one 'B) (eq two 'C)) t)
                   ((and (eq one 'C) (eq two 'B)) t)
                   (t nil))))))

(xmassert (not (member nil (maplist #'mprcheck mprseq))) "mprseq")

(setf acount (mpcount mprseq 'A))
(setf bcount (mpcount mprseq 'B))
(setf ccount (mpcount mprseq 'C))
;(display "mpr" acount bcount ccount)
(xmassert (< acount mprn) "mprseq <a")
(xmassert (< bcount mprn) "mprseq <b")
(xmassert (< ccount (/ mprn 10)) "mprseq <c")
(xmassert (> acount (/ mprn 4)) "mprseq >a")
(xmassert (> bcount (/ mprn 4)) "mprseq >b")
(xmassert (> ccount 0) "mprseq >c")


;; markov with probability pattern on transition

(setf mprp (make-markov `((A -> B) (B -> (A ,(make-cycle '(0 1))) C) (C -> B))
      :past '(B)))
(setf mprpseq nil)
(setf mprpn 200)
(dotimes (i mprpn)
  (setf mprpseq (cons (next mprp) mprpseq)))
(setf mprpseq (reverse mprpseq))

(defun mprpcheck (lis)
  (cond ((null (cddr lis)) t)
        (t (let ((one (first lis)) (two (second lis))
                 (three (third lis)))
             (cond ((and (eq one 'A) (eq two 'B) (eq three 'C)) t)
                   ((and (eq one 'B) (eq two 'A) (eq three 'B)) t)
                   ((and (eq one 'B) (eq two 'C)) t)
                   ((and (eq one 'C) (eq two 'B)) t)
                   (t nil))))))

(xmassert (not (member nil (maplist #'mprpcheck mprpseq))) "mprpseq")

(setf acount (mpcount mprpseq 'A))
(setf bcount (mpcount mprpseq 'B))
(setf ccount (mpcount mprpseq 'C))
;(display "mprp" acount bcount ccount)
(xmassert (< acount (/ mprpn 2)) "mprpseq <a")
(xmassert (< bcount mprpn) "mprpseq <b")
(xmassert (< ccount mprpn) "mprpseq <c")
(xmassert (> acount (/ mprpn 20)) "mprpseq >a")
(xmassert (> bcount (/ mprpn 4)) "mprpseq >b")
(xmassert (> ccount (/ mprpn 4)) "mprpseq >c")


;; markov with probability pattern on output

(setf mprpo (make-markov `((A -> B) (B -> (A ,(make-cycle '(0 1))) C) (C -> B))
      :past '(B) :produces `(A 1 B 2 C ,(make-cycle '(3 4)))))
(setf mprposeq nil)
(setf mprpon 200)
(dotimes (i mprpon)
  (setf mprposeq (cons (next mprpo) mprposeq)))
(setf mprposeq (reverse mprposeq))

(defun mprpocheck (lis)
  (cond ((null (cddr lis)) t)
        (t (let ((one (first lis)) (two (second lis))
                 (three (third lis)))
             (cond ((and (eql one 1) (eql two 2) 
                         (or (eql three 3) (eql three 4))) t)
                   ((and (eql one 2) (eql two 1) (eql three 2)) t)
                   ((and (eql one 2) (or (eql two 3) (eql two 4))) t)
                   ((and (or (eql one 3) (or (eql one 4))) (eql two 2)) t)
                   (t nil))))))

(xmassert (not (member nil (maplist #'mprpocheck mprposeq))) "mprposeq")

(setf acount (mpcount mprposeq 1))
(setf bcount (mpcount mprposeq 2))
(setf c3count (mpcount mprposeq 3))
(setf c4count (mpcount mprposeq 4))
(setf ccount (+ c3count c4count))
;(display "mprpo" acount bcount ccount c3count c4count)
(xmassert (< acount (/ mprpon 2)) "mprposeq <a")
(xmassert (< bcount mprpon) "mprposeq <b")
(xmassert (< ccount mprpon) "mprposeq <c")
(xmassert (> acount (/ mprpon 20)) "mprposeq >a")
(xmassert (> bcount (/ mprpon 4)) "mprposeq >b")
(xmassert (> ccount (/ mprpon 4)) "mprposeq >c")
(xmassert (> c3count (/ c4count 2)) "mprposeq >c3")
(xmassert (> c4count (/ c3count 2)) "mprposeq >c4")
