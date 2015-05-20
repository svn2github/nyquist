;; statistics.lsp -- simple statistics functions

;; to compute statistics, create an object:
;;    (setf stats (send statistics-class :new t))
;; use t to retain the data and nil to not retain the data
;; then call (send stats :point x) for each x in the data set
;; call (send stats :print-stats) to print some statistics
;; see methods below for other methods, e.g. :get-mean
;;
;; to compute histograms, see comments below

;; this function is just here so we can write:
;;   (require-from 'statistics "statistics.lsp")
(defun statistics () (print "See statistics.lsp"))

(require-from vector-from-array "vectors.lsp")

(setf statistics-class (send class :new '(count sum sum-sqr max min retain data)))

(send statistics-class :answer :isnew '(ret) '((send self :init ret)))

(send statistics-class :answer :init '(ret) '(
    (setf count 0 sum 0 sum-sqr 0 data nil 
          max nil min nil retain ret data nil)))

(send statistics-class :answer :point '(x) '(
    (incf count)
    (setf sum (+ sum x))
    (setf sum-sqr (+ sum-sqr (* x x)))
    (setf max (if max (max max x) x))
    (setf min (if min (min min x) x))
    (if retain (push x data))))

(send statistics-class :answer :get-count '() '(count))
(send statistics-class :answer :get-data '() '(data))
(send statistics-class :answer :get-min '() '(min))
(send statistics-class :answer :get-max '() '(max))

(send statistics-class :answer :get-mean '() '(
    (if (> count 0) (/ (float sum) count)
                    nil)))


(send statistics-class :answer :get-stddev '() '(
  (if (> count 1) (sqrt (send self :get-variance)) nil)))


(send statistics-class :answer :get-variance '() '(
  (if (> count 1)
      (/ (- sum-sqr
            (/ (* sum sum) (float count)))
         (1- count))
      nil)))


(send statistics-class :answer :print-stats '() '(
    (format t "Number of points: ~A~%Max: ~A~%Min: ~A~%" count max min)
    (if retain
        (format t "Median: ~A~%" (send self :get-median)))
    (format t "Mean: ~A~%Std.Dev.: ~A~%"
              (send self :get-mean) (send self :get-stddev))
    ))


(send statistics-class :answer :get-data '() '(data))
                                               

(send statistics-class :answer :get-median '() '(
  (let (i)
    (cond ((not retain) nil) ;; no data retained to examine
          ((< count 1) nil) ;; no data to compute from
          (t
           (setf data (bigsort data '<))
           (cond ((oddp count)
                  (nth (/ count 2) data))
                 (t
                  (setf i (/ count 2))
                  (* 0.5 (+ (nth i data) (nth (1- i) data))))))))))

;; This is the "usual estimator of the population kurtosis" based
;; on Wikipedia. In order for this to work, the statistics object
;; must be initialized to *retain* the data
;;
(send statistics-class :answer :get-kurtosis '() '(
  (let ((x4 0) x2
        (n (float count)) ; "n" is just a new name for count
        (mean (send self :get-mean))
        (variance (send self :get-variance)))
    (dolist (x data)
      (setf x2 (* (- x mean) (- x mean)))
      (setf x4 (+ x4 (* x2 x2))))
    (display "kurtosis" x4 (* variance variance) n)
    (if (> n 3)
        (- (* (/ (* (1+ n) n)
                 (* (1- n) (- n 2) (- n 3)))
              (/ x4 (* variance variance)))
           (/ (* 3 (1- n) (1- n))
              (* (- n 2) (- n 3))))
        nil))))

;; :FRACTION-IN-RANGE -- proportion of values in a range
;;
(send statistics-class :answer :fraction-in-range '(low high) '(
  (let ((n 0))
    (dolist (d data)
      (if (and (<= low d) (< d high)) (setf n (1+ n))))
    (/ (float n) count))))

            
;; The histogram-class. Make a histogram from data.
;; Depends on "vectors.lsp" so please (load "vectors")
;;
;; To use histogram-class, first make an instance:
;;    (setf my-histogram (send histogram-class :new))
;; Then add points to the histogram. For each point x:
;;    (send my-histogram :point x)
;; You can make a default histogram by calling:
;;    (send my-histogram :configure-bins)
;; This will create the square root of N bins where N is the
;; number of points. The bins are evenly distributed across
;; the range of the data.
;; Alternatively, you can provide your own thresholds to
;; determine the bins by calling:
;;    (send my-histogram :set-thresholds an-array)
;; Each element of an-array represents the lower bound for
;; elements in that bin. E.g. if x is a point, it goes in
;; bin 3 if (aref an-array 3) <= x < (aref an-array 4)
;; Note that nothing goes into bin L-1 where L is the length
;; of an-array.
;; To actually compute the histogram, call
;;    (send my-histogram :make-hist)
;; And then you can print or plot it with:
;;    (send my-histogram :print-hist) or
;;    (send my-histogram :plot-hist) or
;; If you (sal-load "gnuplot"), you can also make a plot file with:
;;    (send my-histogram :gnu-plot filename xlabel ylabel title [categories])
;; where categories is true by default and says whether the histogram plot
;; bars should be centered on the low value of threshold range or span from
;; the low value to the high value. If this is a histogram of integer values
;; and each bin represents one of the integers, then use t. If this is a 
;; histogram of continuous real values that fall in a range, use nil (false).
;; You can change the thresholds with :set-thresholds or
;; :configure-bins without re-inserting all the points.
;; You can start over by calling
;;    (send my-histogram :init)
;; but this probably has no advantage over making a new
;; instance.

(setf histogram-class (send class :new '(stats counts thresholds)))

(send histogram-class :answer :isnew '() '((send self :init)))

(send histogram-class :answer :init '() '(
    (setf counts nil thresholds nil)
    ; create stats object and tell it to retain points
    (setf stats (send statistics-class :new t))))

(send histogram-class :answer :point '(x) '(
    (send stats :point x)))

(send histogram-class :answer :configure-bins '() '(
    (let* ((nbins (round (sqrt (float (send stats :get-count)))))
           (minthreshold (send stats :get-min))
           (step (/ (- (send stats :get-max) (send stats :get-min))
                    (float nbins))))
      (setf thresholds (make-array (1+ nbins)))
      (dotimes (i (1+ nbins))
        (setf (aref thresholds i) (+ minthreshold (* i step))))
      ;; make last bin a little bigger to contain max value:
      ;; note: (I first tried +1e-6, but then when the threshold was
      ;;        1e6, this must have underflowed and didn't work)
      (setf (aref thresholds nbins) (* (aref thresholds nbins) 1.000001))
      (display "configure-bins" minthreshold (send stats :get-max) thresholds)
      thresholds)))

(send histogram-class :answer :set-thresholds '(array) '(
    (setf counts nil)
    (setf thresholds array)))


(send histogram-class :answer :make-hist '(&key (verbose t)) '(
    (let* ((data (send stats :get-data))
           (counter 0) (data-position 0))
      (if (null thresholds)
          (send self :configure-bins))
      (cond ((null counts)
             (setf counts (make-array (1- (length thresholds))))
             (dotimes (i (length counts))
                      (setf (aref counts i) 0))))
      (dolist (x data)
        (cond ((and verbose (> counter 100000))
               (format t "make-hist ~A% done\n"
                         (* 100
                            (/ data-position (float (send stats :get-count)))))
               (setf counter 0)))
        ; increment the right bin -- allows different bin sizes but
        ; could use a binary search for the right bin
        (dotimes (i (length counts))
          (incf counter)
          (cond ((and (< x (aref thresholds (1+ i)))
                      (>= x (aref thresholds i)))
                 (incf (aref counts i))
                 (return))))
        (incf data-position)) )))


(send histogram-class :answer :print-hist '() '(
    (if (null counts) (send self :make-hist))
    (dotimes (i (length counts))
             (format t "~A to ~A: ~A~%" 
                     (aref thresholds i) (aref thresholds (1+ i))
                     (aref counts i)))))

(send histogram-class :answer :plot-hist '(&optional (offset 0)) '(
  (let (args time cnt) ; compute args in order, push, then reverse
    (if (null counts) (send self :make-hist))
    ;; make arguments for pwl-list
    (setf time (float (aref thresholds 0)))
    (setf args (list time))
    (setf args (cons 0.0 args))
    (dotimes (i (length counts))
      (setf cnt (float (aref counts i)))
      (setf args (cons time args))
      (setf args (cons cnt args))
      (setf time (float (aref thresholds (1+ i))))
      (setf args (cons time args))
      (setf args (cons cnt args)))
    (s-plot (pwl-list (reverse args)) time))))



(send histogram-class :answer :gnu-plot '(filename xlabel ylabel title 
                                          &optional (categories t)) '(
  (let ((thresh-list (vector-from-array thresholds))
        (counts-list (vector-from-array counts))
        (low-wid (- (aref thresholds 1) (aref thresholds 0)))
        (high-wid (- (aref thresholds (- (length thresholds) 1))
                     (aref thresholds (- (length thresholds) 2))))
        xrange x)
    (display "gnu-plot" thresh-list counts-list low-wid high-wid)
    ;; I think histogram bars on the top and bottom are the width of the
    ;; distance to the next or previous bin, but centered on the data point,
    ;; so we have to adjust the xrange to accommodate fat bars
    (cond (categories
           (setf xrange (list (- (apply 'min thresh-list) (/ low-wid 2.0))
                              (+ (apply 'max thresh-list) (/ high-wid 2.0)))))
          (t
           (setf xrange (list (apply 'min thresh-list) 
                              (apply 'max thresh-list)))))
    (gp-init filename :xlabel xlabel :ylabel ylabel :title title
             :style :histogram
             :xrange xrange
             :yrange (list 0 (apply 'max counts-list)))
    (gp-newcurve)
    (dotimes (i (length counts))
      (setf x (pop thresh-list))
      (if (not categories) (setf x (* 0.5 (+ x (car thresh-list)))))
      (gp-point x (pop counts-list)))
    (gp-endcurve)
    (gp-endplot))))



(send histogram-class :answer :get-min '() '(
    (send stats :get-min)))

(send histogram-class :answer :get-max '() '(
    (send stats :get-max)))

(send histogram-class :answer :get-count '() '(
    (send stats :get-count)))

(send histogram-class :answer :get-counts '() '(
    counts))

(send histogram-class :answer :get-thresholds '() '(
    thresholds))

(send histogram-class :answer :get-stats '() '(
    stats))

      
;; Pearson correlation - direct (unstable) algorithm
;;
;; I created this to get the "true" answer when I was trying to
;; debug the more complex version below. All three algorithms here
;; now agree (within numerical roundoff), and I believe the
;; pearson-class below is the best implementation. -RBD
;;
;(setf upearson-class (send class :new '(sumxy sumx sumy sumxx sumyy n)))
;
;(send upearson-class :answer :isnew '() '((send self :init)))
;(send upearson-class :answer :init '() '(
;      (setf sumxy 0 sumx 0 sumy 0 sumxx 0 sumyy 0 n 0)))
;(send upearson-class :answer :points '(x y) '(
;      (setf sumxy (+ sumxy (* x y)))
;      (setf sumx (+ sumx x))
;      (setf sumy (+ sumy y))
;      (setf sumxx (+ sumxx (* x x)))
;      (setf sumyy (+ sumyy (* y y)))
;      (setf n (+ n 1))))
;(send upearson-class :answer :correlation '() '(
;      (/ (- (* n sumxy) (* sumx sumy))
;         (* (sqrt (- (* n sumxx) (* sumx sumx)))
;            (sqrt (- (* n sumyy) (* sumy sumy)))))))

;; Pearson correlation
;;
(setf pearson-class (send class :new '(sum-sq-x sum-sq-y sum-coproduct
                                       mean-x mean-y n)))
(send pearson-class :answer :isnew '() '((send self :init)))
(send pearson-class :answer :init '() '(
    (setf n 0)
    (setf sum-sq-x 0 sum-sq-y 0 sum-coproduct 0)))

(send pearson-class :answer :points '(x y) '(
    (cond ((zerop n)
           (setf mean-x x mean-y y n 1))
          (t
           (setf n (1+ n))
           (let* ((sweep (/ (- n 1.0) n))
                  (delta-x (- x mean-x))
                  (delta-y (- y mean-y)))
             (setf sum-sq-x (+ sum-sq-x (* delta-x delta-x sweep)))
             (setf sum-sq-y (+ sum-sq-y (* delta-y delta-y sweep)))
             (setf sum-coproduct (+ sum-coproduct (* delta-x delta-y sweep)))
             (setf mean-x (+ mean-x (/ delta-x n)))
             (setf mean-y (+ mean-y (/ delta-y n))))))))

(send pearson-class :answer :correlation '() '(
    (let* ((pop-sd-x (sqrt (/ sum-sq-x n)))
           (pop-sd-y (sqrt (/ sum-sq-y n)))
           (cov-x-y (/ sum-coproduct n)))
      (/ cov-x-y (* pop-sd-x pop-sd-y)))))

;; This is a very direct implementation of the algorithm below,
;; but it stores the points -- I created this for debugging but
;; I don't see any reason to use it now. -RBD
;(setf npearson-class (send class :new '(pts)))
;(send npearson-class :answer :isnew '() '((send self :init)))
;(send npearson-class :answer :init '() '((setf pts nil)))
;(send npearson-class :answer :points '(x y) '(
;      (setf pts (cons (cons x y) pts))))
;(send npearson-class :answer :correlation '() '(
;  (setf pts (reverse pts))
;  (let ((sum-sq-x 0) (sum-sq-y 0) (sum-coproduct 0) (mean-x (caar pts))
;        (mean-y (cdar pts)) i (n (length pts)))
;    (dotimes (j (1- n))
;      (let* ((i (+ j 2))
;             (sweep (/ (- i 1.0) i))
;             (delta-x (- (car (nth (1- i) pts)) mean-x))
;             (delta-y (- (cdr (nth (1- i) pts)) mean-y)))
;        (setf sum-sq-x (+ sum-sq-x (* delta-x delta-x sweep)))
;        (setf sum-sq-y (+ sum-sq-y (* delta-y delta-y sweep)))
;        (setf sum-coproduct (+ sum-coproduct (* delta-x delta-y sweep)))
;        (setf mean-x (+ mean-x (/ delta-x i)))
;        (setf mean-y (+ mean-y (/ delta-y i)))))
;    (let ((pop-sd-x (sqrt (/ sum-sq-x n)))
;          (pop-sd-y (sqrt (/ sum-sq-y n)))
;          (cov-x-y (/ sum-coproduct n)))
;      (/ cov-x-y (* pop-sd-x pop-sd-y))))))

;; the algorithm (from Wikipedia)
;sum_sq_x = 0
;sum_sq_y = 0
;sum_coproduct = 0
;mean_x = x[1]
;mean_y = y[1]
;for i in 2 to N:
;    sweep = (i - 1.0) / i
;    delta_x = x[i] - mean_x
;    delta_y = y[i] - mean_y
;    sum_sq_x += delta_x * delta_x * sweep
;    sum_sq_y += delta_y * delta_y * sweep
;    sum_coproduct += delta_x * delta_y * sweep
;    mean_x += delta_x / i
;    mean_y += delta_y / i 
;pop_sd_x = sqrt( sum_sq_x / N )
;pop_sd_y = sqrt( sum_sq_y / N )
;cov_x_y = sum_coproduct / N
;correlation = cov_x_y / (pop_sd_x * pop_sd_y)

;; Welch's t-test to test the null hypothesis that 2 population means are
;; equal when the variances might be unequal
;;
;; returns list: (welchs-t degrees-of-freedom)
;;
(defun welchs-t-test (mean1 stddev1 n1 mean2 stddev2 n2)
  (let* ((var1 (* stddev1 stddev1))
         (var2 (* stddev2 stddev2))
         (num (- mean1 mean2))
         (den (sqrt (+ (/ var1 n1)
                       (/ var2 n2))))
         (welchs-t (/ num den))
         (dof-a (+ (/ var1 n1) (/ var2 n2)))
         (dof-num (* dof-a dof-a))
         (dof-den (+ (/ (* var1 var1) (* n1 n1 (- n1 1)))
                     (/ (* var2 var2) (* n2 n2 (- n2 1)))))
         (dof (/ dof-num dof-den)))
    (list welchs-t dof)))

;; Levene's test to assess the equality of variances in different samples
;; based on Wikipedia article. This implementation is for 2 groups. If the
;; 2 groups can be assumed to be normal (Gaussian), then the F-test should
;; be considered.
;;
;; A variation on Levene's test is the Brown-Forsythe test, which uses
;; medians instead of means. The optional parameter, brown-forsythe can
;; be set to true to get a Browne-Forsythe test instead of Levene's test.
;;
;; The verbose flag defaults to t and prints some useful information
;;
;; The input to levenes-test is a pair of lists of samples. The return
;; value is W (see Wikipedia for details)
;;
(defun levenes-test (y1 y2 &optional brown-forsythe (verbose t))
  (let* ((n1 (float (length y1)))
         (n2 (float (length y2)))
         (n (+ n1 n2))
         m1 m2 z1 z2 z.. z1. z2. stat (den 0) w)
    ;; compute means or medians
    (cond (brown-forsythe
           (setf m1 (vector-median y1))
           (setf m2 (vector-median y2)))
          (t
           (setf m1 (vector-mean y1))
           (setf m2 (vector-mean y2))))
    ;; compute zij (lists z1 and z2)
    (dolist (y1j y1) (push (abs (- y1j m1)) z1))
    (dolist (y2j y2) (push (abs (- y2j m2)) z2))

    ;; compute zi. sums
    (setf z1. (vector-sum-elements z1))
    (setf z2. (vector-sum-elements z2))

    ;; compute z..
    (setf z.. (/ (+ z1. z2.) n))

    ;; convert zi. variables from sums to means
    (setf z1. (/ z1. n1))
    (setf z2. (/ z2. n2))

    ;; compute the big denominator term
    (dolist (z1j z1)
      (let ((diff (- z1j z1.)))
        (setf den (+ den (* diff diff)))))
    (dolist (z2j z2)
      (let ((diff (- z2j z2.)))
        (setf den (+ den (* diff diff)))))

    ;; compute w
    (setf w (* (- n 2) (/ (+ (* n1 (* (- z1. z..) (- z1. z..)))
                             (* n2 (* (- z2. z..) (- z2. z..))))
                          den)))
    ;; print info if verbose
    (cond (verbose
           (format t "Summary of ~A test results:
    Size of group 1: ~A, ~A: ~A
    Size of group 2: ~A, ~A: ~A
    W (result): ~A
    The significance of W is tested against F(alpha, 1, ~A),
    where alpha is the level of significance (usually 0.05 or
    0.01), and ~A is N-2.~%"
                   (if brown-forsythe "Brown-Forsythe" "Levene's")
                   n1 (if brown-forsythe "Median" "Mean") m1
                   n2 (if brown-forsythe "Median" "Mean") m2
                   w
                   (- n 2) (- n 2))))
    w))


;; a simple test for levenes-test
;; this program uses distributions.lsp, which must be explicitly loaded
;;
(defun levenes-test-test ()
  (let (y1 y2 y3)
    ;; make some data with sigma 0.1 and 0.2
    (dotimes (i 50)
      (push (gaussian-dist 1.0 0.1) y1))
    (dotimes (i 75)
      (push (gaussian-dist 1.0 0.2) y2))
    (dotimes (i 75)
      (push (gaussian-dist 1.0 0.1) y3))
    (format t "\nTHE FOLLOWING HAVE UNEQUAL VARIANCE\n")
    (levenes-test y1 y2) ;; levene's test
    (format t "\n")
    (levenes-test y1 y2 t) ;; brown-forsythe test
    (format t "\nTHE FOLLOWING HAVE EQUAL VARIANCE\n")
    (levenes-test y1 y3) ;; levene's test
    (format t "\n")
    (levenes-test y1 y3 t) ;; brown-forsythe test
    (format t "\n")
    'done
  ))
