(defun pwltest ()
  (define-env 'bar (make-big-envelope)))

(defun make-big-envelope ()
  (let (tim val lis (n 500))
    (dotimes (i n)
      (setf tim (* (1+ i) 0.01))
      (setf val (rrandom))
      (setf lis (cons val (cons tim lis))))
    (setf lis (cons (* (1+ n) 0.01) lis))
    (cons 'pwl (reverse lis))))

;(print (make-big-envelope))

(pwltest)