;; vectors.lsp -- a small simple vector package
;;
;; vectors are lists, not arrays
;; probably one should be able to use either form (list or array)
;; and functions should accept either

(defun vector-from-array (x)
  (let (v (n (length x)))
    (dotimes (i n)
      (setf v (cons (aref x (- n i 1)) v)))
    v))

(defun vector-cosine (x y)
  (/ (vector-dot x y) (vector-norm x) (vector-norm y)))

(defun vector-dot (x y)
  (let ((d 0))
    (dolist (e x)
      (setf d (+ d (* e (car y))))
      (setf y (cdr y)))
    d))

;; VECTOR-NORM -- also Euclidean distance
;;
(defun vector-norm (x)
  (sqrt (float (vector-sum-elements (vector-square x)))))


(defun vector-sum-elements (x)
  (let ((sum 0))
    (dolist (e x)
      (setf sum (+ sum e)))
    sum))

(defun vector-sum (a b)
  (let (v)
    (dolist (e a)
      (setf v (cons (+ e (car b)) v))
      (setf b (cdr b)))
    (reverse v)))

(defun vector-mean (x)
  (/ (vector-sum-elements x) (length x)))


;; vector-median uses statistics.lsp -- you must load this explicitly
;; before calling vector-median
;;
(defun vector-median (x)
  (let ((stats (send statistics-class :new t)))
    (dolist (e x) (send stats :point e))
    (send stats :get-median)))


(defun vector-offset (x c)
  (let (v)
    (dolist (e x)
      (setf v (cons (+ e c) v)))
    (reverse v)))

(defun vector-difference (a b)
  (let (v)
    (dolist (e a)
      (setf v (cons (- e (car b)) v))
      (setf b (cdr b)))
    (reverse v)))


(defun vector-divide (a b)
  (let (v)
    (dolist (e a)
      (setf v (cons (/ e (car b)) v))
      (setf b (cdr b)))
    (reverse v)))


(defun vector-scale (x c)
  (let (v)
    (dolist (e x)
      (setf v (cons (* e c) v)))
    (reverse v)))


(defun vector-zero (len)
  (let (v)
    (dotimes (i len) (setf v (cons 0.0 v)))))


(defun vector-square (a)
  (let (v)
    (dolist (e a)
      (setf v (cons (* e e) v)))
    (reverse v)))


(defun vector-variance (x)
  (let ((n (length x))
        (sum 0.0)
        (sum-sqr 0.0))
    (dotimes (i n)
      (setf sum (+ sum (car x)))
      (setf sum-sqr (+ sum-sqr (* (car x) (car x))))
      (setf x (cdr x)))
    (/ (- sum-sqr (/ (* sum sum) n)) (1- n))))
    

(defun vector-stddev (x)
  (sqrt (vector-variance x)))

;; compute autocorrelation with lag1 <= lag < lag2
;; note that because of different overlap, the autocorrelation
;; will be over different numbers of points (but normalized
;; by dividing by the length). Note: It should be true that
;; 0 <= lag1 < lag2 < length(x)
;; Otherwise, nil is returned.
;;
;; Algorithm notes: len is length of input,
;; rsltlen is length of result. The range of lags is
;; from 0 to len - 1.
;;
(defun vector-autocorrelation (x lag1 lag2)
  (prog ((len (length x)) rsltlen rslt y)
    ;; return nil if lag conditions are not satisfied
    (if (and (<= 0 lag1) (< lag1 lag2) (< lag2 len))
        'ok (return nil))
    (setf rsltlen (- lag2 lag1))
    (setf y (nthcdr lag1 x))
    (dotimes (i rsltlen)
      (let ((xp x) (yp y) (sum 0.0)
            (alen (- len (+ lag1 i))))
        (dotimes (j alen)
          (setf sum (+ sum (* (car xp) (car yp))))
          (setf xp (cdr xp) yp (cdr yp)))
        (setf rslt (cons (/ sum alen) rslt))
        (setf y (cdr y))))
    (return (reverse rslt))))

