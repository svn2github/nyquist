;; Andreas Pfenning, 26 Apr 05
;; modified by Roger B. Dannenberg, 25 Jun 05

;; Probability distribution functions and demo

#|
This is a library of various probability distribution generators.
The functions output values based on the probability distributions
and the input parameters that scale the distributions.  Plots 
of the various distributions are shown in the documentation.
The user has option of adding bounds to the output values,
especially in the cases where high or low outliers are expected.
When a distribution returns a value outside of the given bounds,
the value is rejected and another value is generated. Both
discrete (whole number) and continuous distributions are
available. For continous distributions, the probability of
outputing a value between any two points on the x axis is equal
to the area under the curve between those two points. Essentially,
the higher the curve is for a an area of the x axis, the more
likely it is that the generator will output a value in that area.
Discrete generators output values based on how high the "bar" is
at that location. The documentation shows the behavior generated
by 100 trials of discrete probability distributioin generators.
|#

;;Library of Continuous Probability Distribution Generators

(defun linear-dist (g)
  (* g (- 1.0 (sqrt (rrandom)))))

(defun exponential-dist (delta &optional high)
  (cond ((and high (<= high 0))
         (error "exponential-dist: high value must be positive")))
  (loop 
    (let ((expv (* (/ -1.0 delta) (log (rrandom)))))
      (cond ((or (null high)
                 (<= expv high))
             (return expv))))))

(defun gamma-dist (nu &optional high)
  (cond ((and high (<= high 0)
         (error "gamma-dist: high value must be positive"))))
  (loop
    (let* ((summ 1)
           (summ2 (dotimes (count nu summ)
	          (setf summ (* summ (rrandom)))))
           (gamv (* -1.0 (log summ2))))
      (cond ((or (null high)
                 (<= gamv high))
             (return gamv))))))


(defun bilateral-exponential-dist (xmu tau &optional low high)
  (cond ((and high low (<= high low)) 
         (error "bilateral-exponential-dist: high must be greater than low")))
  (loop
    (let* ((u (* (rrandom) 2.0))
           (bev (if (> u 1.0) 
                    (+ (* -1.0 tau (log (- u 1.0))) xmu)
                    (+ (* tau (log u)) xmu))))
      (cond ((and (or (null high) (< bev high))
                  (or (null low) (> bev low)))
             (return bev)))))) 


(defun cauchy-dist (tau &optional low high)
  (cond ((and high low (<= high low))
         (error "cauchy-dist: high must be greater than low")))
  (loop
    (let* ((u (* PI (rrandom)))
	 (cauv (* tau (/ (sin u) (cos u)))))
      (cond ((and (or (null high) (< cauv high))
                  (or (null low) (> cauv low)))
             (return cauv))))))


(defun hyperbolic-cosine-dist (&optional low high)
  (cond ((and high low (<= high low))
         (error "hyperbolic-cosine-dist: high must be greater than low")))
  (loop
    (let* ((hcv (log (tan (/ (* PI (rrandom)) 2.0)))))
      (cond ((and (or (null high) (< hcv high))
                  (or (null low) (> hcv low)))
             (return hcv))))))


(defun logistic-dist (alpha beta &optional low high)
  (cond ((and high low (<= high low))
         (error "logistic-dist: high must be greater than low")))
  (loop
    (let (rand lgv)
      (setf rand (rrandom))
      (cond ((zerop rand)) ; try again -- do not use zero
            (t
             (setf rand (- (/ rand) 1.0))
             (cond ((zerop rand)) ; try again -- do not use zero
                   (t
                    (setf lgv (/ (- (+ beta (log rand))) 
                               alpha))
                    (cond ((and (or (null high) (< lgv high))
                                (or (null low) (> lgv low)))
                           (return lgv))))))))))


(defun gaussian-dist (xmu sigma &optional low high)
  (cond ((and high low (<= high low))
         (error "gauss-dist: high must be greater than low")))
  (loop
    (let* ((s 0.0)
           (s2 (dotimes (i 12 s)
                        (setq s (+ s (rrandom)))))
           (gsv (+ (* sigma (- s2 6.0)) xmu)))
      (cond ((and (or (null high) (< gsv high))
                  (or (null low) (> gsv low)))
             (return gsv))))))


(defun beta-help (ea eb)
  (loop
    (let ((y1 (power (rrandom) ea))
          (y2 (power (rrandom) eb)))
      (if (<= (+ y1 y2) 1.0)
          (return (/ y1 (+ y1 y2)))))))


(defun beta-dist (a b)
  (let ((ea (/ 1.0 a)) (eb (/ 1.0 b)))
    (beta-help ea eb)))


;;Library of Discrete Probability Distribution Generators

(defun bernoulli-dist (px1 &optional (x1 1) (x2 0))
  (let ((u (rrandom)))
    (if (< u px1) x1 x2)))

(defun binomial-dist (n p)
  (let ((suc 0))
    (dotimes (count n suc)
      (setf suc (+ suc (bernoulli-dist p))))))

(defun geometric-dist (p &optional (count 0))
  (loop
    (cond ((= (bernoulli-dist p) 1) 
           (return count)))
    (setf count (1+ count))))

(defun poisson-dist (delta)
  (let ((x 0) (t 0.0))
    (loop
      (setf t (- t (/ (log (rrandom)) delta)))
      (cond ((> t 1) (return x)))
      (setf x (1+ x)))))


