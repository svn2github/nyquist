; this test should display a plot of x^2
; it gives an example of constructing a DSP primitive
; (in this case, product) using Lisp rather than C
; for the computation

(setf product-class (send class :new '(s1 s2)))

(send product-class :answer :next '()
  '((let ((f1 (snd-fetch s1))
          (f2 (snd-fetch s2)))
      (cond ((and f1 f2)
             (* f1 f2))
            (t nil)))))

(send product-class :answer :isnew '(p1 p2)
  '((setf s1 (snd-copy p1))
    (setf s2 (snd-copy p2))))

(defun snd-product (s1 s2)
  (let (obj)
    (setf obj (send product-class :new s1 s2))
    (snd-fromobject (snd-t0 s1) (snd-srate s1) obj)))

(set-control-srate 100)

(s-plot (snd-product (ramp) (ramp)))




