;; xm-test.lsp
;;
;; ============== some test code for xm.lsp ===============
;;

;(load "xm")

'(setf pat (make-heap '(a b c d e)))
'(dotimes (i 10) (print (next pat)))
'(print "heap test done: ")
'(read)


(defun about-equal (x y)
  (cond ((null x) (null y))
	((consp x)
	 (and  (consp y)
	       (about-equal (car x) (car y))
	       (about-equal (cdr x) (cdr y))))
	((numberp x)
	 (and (numberp y)
	      (< (abs (- x y)) 0.000001)))
	(t
	 (equal x y))))


(defun test (name &rest pairs)
  (format t "TEST ~A : " name)
  (loop
    (cond ((or (null pairs) (null (cdr pairs)))
	   (format t " --PASSED--~%")
	   (return))
	  ((not (about-equal (car pairs) (cadr pairs)))
	   (format t " --FAILED-- ~A returned instead of ~A~%"
		   (car pairs) (cadr pairs))
	   (break "a test failed")
	   (return)))
    (setf pairs (cddr pairs))))
      


(setf xx (make-cycle '(x y z) :name "xx" :trace nil))

(test "test" (next xx) 'x (next xx) 'y (next xx t) '(z) (next xx t) '(x y z))

(setf aa (make-cycle '(a b) :name "aa" :trace nil))

(setf zz (make-cycle (list xx aa)))

(test "test2" (next zz) 'x (next zz) 'y (next zz t) '(z) (next zz t) '(a b))

(setf zz1 (make-cycle (list xx aa) :for 1 :name "zz1"))

(setf zz10 (make-cycle (list xx aa) :for 10 :name "zz10" :trace nil))

(test "test3" (next zz1 t) '(x y z) (next zz1 t) '(a b) 
      (next zz10 t) '(x y z)
      (next zz10 t) '(a b))

(setf aa1 (make-cycle '(a b) :for 1 :name "aa1"))

(setf zza1 (make-cycle (list xx aa1) :name "zza1"))

(test "test4" (next zza1 t) '(x y z) (next zza1 t) '(a)
      (next zza1 t) '(x y z) (next zza1 t) '(b))


(setf zz2 (make-cycle (list xx aa) :for 1 :name "zz2"))
(setf zzz (make-cycle (list zz2 'q) :name "zzz"))

(test "test5" (next zzz t) '(x y z) (next zzz t) '(q)
      (next zzz t) '(a b) (next zzz t) '(q)
      (next zzz t) '(x y z) (next zzz t) '(q))

 ; test using cpat as items list
(setf cpat (make-cycle '(a b) :name "cpat" :trace nil))
(setf recycpat (make-cycle cpat :for 3 :name "recycpat" :trace nil))

(test "test6" (next recycpat t) '(a b a)
              (next recycpat t) '(a b a))

; test length-class
(setf lpat (make-length (make-cycle '(a b c)) 2))
(test "length test 1" (next lpat t) '(a b) (next lpat t) '(c a))

(setf lpat2 (make-length (make-cycle '(a b c)) (make-cycle '(2 1 0))))
(test "length test 2" (next lpat2 t) '(a b) (next lpat2 t) '(c)
      (next lpat2 t) '() (next lpat2 t) '(a b))


(setf pp1 (make-palindrome '(a b c) 
	   :elide (make-cycle '(:first :last t nil) :name "pp1-elide-pattern")
	   :name "pp1"))
(test "palindrome test" (next pp1 t) '(a b c c b) (next pp1 t) '(a b c b a)
      (next pp1 t) '(a b c b) (next pp1 t) '(a b c c b a) 
      (next pp1 1) '(a b c c b))


(setf pp2 (make-palindrome '(a b c) 
			   :elide (make-cycle '(:first :last t)) :for 3))
(test "palindrome test 2" (next pp2 t) '(a b c) (next pp2 t) '(c b a)
      (next pp2 t) '(a b c) (next pp2 t) '(b a b))


(setf pp3 (make-palindrome '(a) :elide (make-cycle '(:first :last t nil))))
(test "palindrome test 3" (next pp3 t) '(a) (next pp3 t) '(a)
      (next pp3 t) nil (next pp3 t) '(a a))


(setf pp4 (make-palindrome '(a b c) :elide (make-cycle '(:first :last)) 
			   :for 6))
(test "palindrome test 4" (next pp4 t) '(a b c c b a)
      (next pp4 t) '(b c b a a b))

(setf ll1 (make-line '(a b c d)))
(test "line test" (next ll1 t) '(a b c d) (next ll1 t) '(d d d d))

(setf nn1 (make-cycle (list 
                       (make-cycle (list
                                    (make-cycle '(a b) :name "ab")
                                    (make-cycle '(c) :name "c"))
                                    :name "ab-c")
                       (make-cycle (list
                                    (make-cycle '(x y) :name "xy")
                                    (make-cycle '(u v) :name "uv"))
                                    :name "xy-uv"))
                       :name "ab-c-xy-uv"))

(test "nested test" (next nn1 t) '(a b) (next nn1 t) '(c) (next nn1 t) '(x y)
      (next nn1 t) '(u v) (next nn1 t) '(a b) (next nn1 t) '(c))

(setf win1 (make-window (make-cycle '(a b c)) 3 1))

(test "window test 1" (next win1 t) '(a b c)
      (next win1 t) '(b c a) (next win1 t) '(c a b))

(setf win2 (make-window (make-cycle '(a b c)) ; source
			(make-cycle '(3 1))   ; length
			(make-cycle '(1 1 3)))) ; skip

(test "window test 2" (next win2 t) '(a b c)
      (next win2 t) '(b)  ; skip 1 length 1
      (next win2 t) '(a b c) ; skip 1 length 3
      (next win2 t) '(a) ; skip 3 length 1
      (next win2 t) '(b c a) ; skip 1 length 3
      (next win2 t) '(c)  ; skip 1 length 1
      (next win2 t) '(a b c)  ; skip 3 length 3
      (next win2 t) '(b) ; skip 1 length 1
      (next win2 t) '(a b c))   ; skip 1 length 3

(defun only-abc-n (lis n)
  (and (= (length lis) n)
       (dolist (item lis t)
	 (cond ((not (member item '(a b c)))
		(return nil))))))

(defun abc2 (lis) (only-abc-n lis 2))

(defun abc3 (lis) (only-abc-n lis 3))

(setf rr1 (make-random '(a b c) :name "rr1"))
(display "rr1" (next rr1 t))
(test "random 1" (abc3 (next rr1 t)) t (abc3 (next rr1 t)) t)

(setf rr2 (make-random '(a b c) :for 2 :name "rr2" :trace nil))
(test "random 2" (abc2 (next rr2 t)) t (abc2 (next rr2 t)) t)


; random using weights
(setf rr3 (make-random '((a :weight 1) (b :weight 10))))
(setf a-count 0 b-count 0)
(setf *num-trials* 10000) ;; NORMALLY, SET THIS TO 10000
(setf rr3-all nil)
(dotimes (i *num-trials*) 
  ;(display "weight test" i)
  (if (eq (setf rr3-out (next rr3)) 'a) (incf a-count) (incf b-count))
  (push rr3-out rr3-all))
;(print (reverse rr3-all))

(setf rr-ratio  (/ (float a-count) b-count))
(format t "test rr3 a/b should be 0.091, actual ratio is ~A~%" rr-ratio)
(test "weight test" (and (< rr-ratio 0.12) (> rr-ratio 0.08)) t)


; random using :min
(setf rr4 (make-random '((a :weight 1 :min 2) (b :weight 10 :min 1))))
(setf sum 0)
(setf a-count 0 b-count 0)
(dotimes (i *num-trials*) 
    ;(display "min test" (next rr4))
    (if (eq (next rr4) 'a) (incf a-count) (incf b-count))
)
(setf rr-ratio  (/ (float a-count) b-count))
(format t "test rr4 a/b should be about 0.191, actual ratio is ~A~%" rr-ratio)
(test "min test" (and (< rr-ratio 0.22) (> rr-ratio 0.16)) t)


; random using :max
(setf rr5 (make-random '((a :weight 1 :min 2) (b :weight 10 :max 5))))
(setf sum 0)
(setf rr5-all nil)
(setf a-count 0 b-count 0)
(dotimes (i *num-trials*) 
    ;(display "max test" (next rr5))
    (if (eq (setf rr5-out (next rr5)) 'a) (incf a-count) (incf b-count))
    (push rr5-out rr5-all))
(setf rr5-all (reverse rr5-all))
(setf rr5-state 'a2)
(setf rr5-count 0)
(dolist (s rr5-all)
  (incf rr5-count)
  (cond ((and (eq rr5-state 'a1) (eq s 'a))
	 (setf rr5-state 'a2))
	((and (eq rr5-state 'a2) (eq s 'a)))
	((and (eq rr5-state 'a2) (eq s 'b))
	 (setf rr5-state 'b1))
	((and (eq rr5-state 'b1) (eq s 'b))
	 (setf rr5-state 'b2))
	((and (eq rr5-state 'b2) (eq s 'b))
	 (setf rr5-state 'b3))
	((and (eq rr5-state 'b3) (eq s 'b))
	 (setf rr5-state 'b4))
	((and (eq rr5-state 'b4) (eq s 'b))
	 (setf rr5-state 'b5))
	((and (eq rr5-state 'b1) (eq s 'a))
	 (setf rr5-state 'a1))
	((and (eq rr5-state 'b2) (eq s 'a))
	 (setf rr5-state 'a1))
	((and (eq rr5-state 'b3) (eq s 'a))
	 (setf rr5-state 'a1))
	((and (eq rr5-state 'b4) (eq s 'a))
	 (setf rr5-state 'a1))
	((and (eq rr5-state 'b5) (eq s 'a))
	 (setf rr5-state 'a1))
	(t
	 (error "bad state"))))

(setf rr-ratio  (/ (float a-count) b-count))
(format t "test rr5 a/b should be 0.503613, actual ratio is ~A~%" rr-ratio)
(test "max test" (and (> rr-ratio 0.4) (< rr-ratio 0.6)) t)


(setf hh1 (make-heap '(a b c)))
(test "heap 1" (abc3 (next hh1 t)) t (abc3 (next hh1 t)) t)

(setf hh2 (make-heap '(a b c) :for 2 :name "hh2" :trace nil))
(test "heap 2" (abc2 (next hh2 t)) t (abc2 (next hh2 t)) t)

(setf xx (make-markov `((a -> a (b ,(make-cycle '(1 2)))) (b -> a))
                      :past '(b)
                      :produces `(a ,(make-cycle '(0))
                                  b ,(make-cycle '(1 2 3 4) 
                                      :trace nil :name "b-produces"))
                      :is-nested t
                      :trace nil :name "markov"))
(setf xx-out nil)
(dotimes (i 12) (push (next xx) xx-out))
(setf xx-out (reverse xx-out))
(print xx-out)
;; this is a special test to see if the output is plausible
(defun markov-test-xx (lis)
  (let (a b)
    (setf a (car lis))
    (setf lis (cdr lis))
    (cond ((null lis) t)
          ((and (setf b (car lis))
                (eq a 0) (member b '(0 1)))
           (markov-test-xx lis))
          ((and (eq a 1) (eq b 2))
           (markov-test-xx lis))
          ((and (eq a 2) (eq b 3))
           (markov-test-xx lis))
          ((and (eq a 3) (eq b 4))
           (markov-test-xx lis))
          ((and (= a 4) (= b 0))
           (markov-test-xx lis))
          (t nil))))
(format t "TEST markov test :  ~A~%" (if (markov-test-xx xx-out) "--PASSED--"
                                       (break "markov test failed")))


(setf cc (make-copier (make-cycle '(a b) :name "ab" :trace nil) 
                      :repeat 3 :name "copier" :trace nil :merge t))

(test "copier test 1" (next cc t) '(a b a b a b) (next cc t) '(a b a b a b))

(setf cc2 (make-copier (make-cycle '(a b) :name "ab" :trace nil) 
                      :repeat 3 :name "copier" :trace nil))

(test "copier test 2" (next cc2 t) '(a b) (next cc2 t) '(a b) (next cc2 t) '(a b))

(setf cc3 (make-copier (make-cycle (list (make-cycle '(a)) (make-cycle '(b))))
                       :repeat 3 :merge t))

(test "copier test 3" (next cc3 t) '(a a a) (next cc3 t) '(b b b)
                      (next cc3 t) '(a a a))

(setf cc4 (make-copier (make-cycle '(a b c d) :for 1)
		       :repeat (make-cycle '(2 -2 3 -3)) 
		       :merge t))
(test "copier test 4" (next cc4 t) '(a a) (next cc4 t) '(d d d)
      (next cc4 t) '(d d) (next cc4 t) '(c c c))

(setf cc5 (make-copier (make-cycle '(a b c d) :for 1)
		       :repeat 3))
(test "compier test 5" (next cc5 t) '(a) (next cc5 t) '(a)
      (next cc5 t) '(a) (next cc5 t) '(b) (next cc5 t) '(b)
      (next cc5 t) '(b) (next cc5 t) '(c) (next cc5 t) '(c)
      (next cc5 t) '(c) (next cc5 t) '(d) (next cc5 t) '(d) 
      (next cc5 t) '(d) (next cc5 t) '(a) (next cc5 t) '(a))


(setf acc1 (make-accumulate (make-cycle '(1 2 -3))))

(test "accumulate test 1" (next acc1 t) '(1 3 0) (next acc1 t) '(1 3 0))

(setf acc2 (make-accumulate (make-cycle '(1 2 -3) :for 2)))

(test "accumulate test 2" (next acc2 t) '(1 3) (next acc2 t) '(0 1)
      (next acc2 t) '(3 0))

(setf sum1 (make-sum (make-cycle '(1 2)) (make-cycle '(3 4 5))))

(test "sum test 1" (next sum1 t) '(4 6) (next sum1 t) '(6 5)
                   (next sum1 t) '(5 7))

(setf sum2 (make-sum (make-cycle '(1 2)) (make-cycle '(3 4 5)) :for 4))

(test "sum test 2" (next sum2 t) '(4 6 6 5) (next sum2 t) '(5 7 4 6))

(setf prod1 (make-product (make-cycle '(1 2)) (make-cycle '(3 4 5))))

(test "prod test 1" (next prod1 t) '(3 8) (next prod1 t) '(5 6)
                    (next prod1 t) '(4 10))

(setf prod2 (make-product (make-cycle '(1 2)) (make-cycle '(3 4 5)) :for 4))

(test "prod test 2" (next prod2 t) '(3 8 5 6) (next prod2 t) '(4 10 3 8))

(setf eval1 (make-eval '(+ 1 2) :for 3))

(test "eval test 1" (next eval1 t) '(3 3 3))

#|

(setf testscore '((0 0 (score-begin-end 0 10))
		  (0.1 1 (note :pitch 60 :vel 100))
		  (1 1 (note :pitch 61 :vel 91))
		  (1 0.5 (note :pitch 68 :vel 92))
		  (1.5 0.5 (note :pitch 67 :vel 93))
		  (2  1 (note :pitch 62 :vel 94))))

(test "basic1" (score-get-begin testscore) 0)
(test "basic2" (score-get-end testscore) 10)
(test "basic3" (score-set-begin testscore 1) 
               (cons '(0 0 (score-begin-end 1 10)) (cdr testscore)))
(test "basic4" (score-set-end testscore 4)
               (cons '(0 0 (score-begin-end 0 4)) (cdr testscore)))
(test "basic5" (score-get-begin (cdr testscore)) 0.1)
(test "basic6" (score-get-end (cdr testscore)) 3)

(test "score-shift" (score-shift testscore 5)
      '((0 0 (SCORE-BEGIN-END 0 15))
	(5.1 1 (NOTE :PITCH 60 :VEL 100))
	(6 0.5 (NOTE :PITCH 68 :VEL 92))
	(6 1 (NOTE :PITCH 61 :VEL 91))
	(6.5 0.5 (NOTE :PITCH 67 :VEL 93))
	(7 1 (NOTE :PITCH 62 :VEL 94))
	))

(test "score-stretch1" (setf xx (score-stretch testscore 2))
      (setf yy '((0 0 (SCORE-BEGIN-END 0 20))
	(0.2 2 (NOTE :PITCH 60 :VEL 100))
	(2 2 (NOTE :PITCH 61 :VEL 91))
	(2 1 (NOTE :PITCH 68 :VEL 92))
	(3 1 (NOTE :PITCH 67 :VEL 93))
	(4 2 (NOTE :PITCH 62 :VEL 94))
	)))

(test "score-stretch2" (score-stretch testscore 2 :dur nil)
      '((0 0 (SCORE-BEGIN-END 0 20))
	(0.2 1 (NOTE :PITCH 60 :VEL 100))
	(2 1 (NOTE :PITCH 61 :VEL 91))
	(2 0.5 (NOTE :PITCH 68 :VEL 92))
	(3 0.5 (NOTE :PITCH 67 :VEL 93))
	(4 1 (NOTE :PITCH 62 :VEL 94))
	))

(test "score-stretch3" (score-stretch testscore 2 :from-index 2 :to-index 3)
      '((0 0 (SCORE-BEGIN-END 0 10.5))
	(0.1 1.1 (NOTE :PITCH 60 :VEL 100))
	(1 1.5 (NOTE :PITCH 61 :VEL 91))
	(1 1 (NOTE :PITCH 68 :VEL 92))
	(2 0.5 (NOTE :PITCH 67 :VEL 93))
	(2.5 1 (NOTE :PITCH 62 :VEL 94))
	))

(test "score-stretch4" (score-stretch testscore 2 :from-time 1.5 :to-time 2.5)
      '((0 0 (SCORE-BEGIN-END 0 11))
	(0.1 1 (NOTE :PITCH 60 :VEL 100))
	(1 1.5 (NOTE :PITCH 61 :VEL 91))
	(1 0.5 (NOTE :PITCH 68 :VEL 92))
	(1.5 1 (NOTE :PITCH 67 :VEL 93))
	(2.5 1.5 (NOTE :PITCH 62 :VEL 94))
	))


(test "score-transpose1" (score-transpose testscore :pitch 5)
      '((0 0 (score-begin-end 0 10))
	(0.1 1 (note :pitch 65 :vel 100))
	(1 1 (note :pitch 66 :vel 91))
	(1 0.5 (note :pitch 73 :vel 92))
	(1.5 0.5 (note :pitch 72 :vel 93))
	(2  1 (note :pitch 67 :vel 94))))

(test "score-transpose1" (score-transpose testscore :pitch 5 
					  :from-index 2 :to-index 3)
      '((0 0 (score-begin-end 0 10))
	(0.1 1 (note :pitch 60 :vel 100))
	(1 1 (note :pitch 66 :vel 91))
	(1 0.5 (note :pitch 73 :vel 92))
	(1.5 0.5 (note :pitch 67 :vel 93))
	(2  1 (note :pitch 62 :vel 94))))

(test "score-scale1" (score-scale testscore :vel 0.5)
      '((0 0 (SCORE-BEGIN-END 0 10))
	(0.1 1 (NOTE :PITCH 60 :VEL 50))
	(1 1 (NOTE :PITCH 61 :VEL 45.5))
	(1 0.5 (NOTE :PITCH 68 :VEL 46))
	(1.5 0.5 (NOTE :PITCH 67 :VEL 46.5))
	(2 1 (NOTE :PITCH 62 :VEL 47))
	))

(test "score-scale2" (score-scale testscore :vel 0.5
				 :from-time 1 :to-time 1.5)
      '((0 0 (SCORE-BEGIN-END 0 10))
	(0.1 1 (NOTE :PITCH 60 :VEL 100))
	(1 1 (NOTE :PITCH 61 :VEL 45.5))
	(1 0.5 (NOTE :PITCH 68 :VEL 46))
	(1.5 0.5 (NOTE :PITCH 67 :VEL 93))
	(2 1 (NOTE :PITCH 62 :VEL 94))
	))

(test "score-sustain1" (score-sustain testscore 2) 
      '((0 0 (SCORE-BEGIN-END 0 10)) 
	(0.1 2 (NOTE :PITCH 60 :VEL 100)) 
	(1 2 (NOTE :PITCH 61 :VEL 91))
	(1 1 (NOTE :PITCH 68 :VEL 92)) 
	(1.5 1 (NOTE :PITCH 67 :VEL 93)) 
	(2 2 (NOTE :PITCH 62 :VEL 94))))

(test "score-sustain2" (score-sustain testscore 2
				      :from-index 0 :to-index 1)
      '((0 0 (SCORE-BEGIN-END 0 10)) 
	(0.1 2 (NOTE :PITCH 60 :VEL 100)) 
	(1 1 (NOTE :PITCH 61 :VEL 91))
	(1 0.5 (NOTE :PITCH 68 :VEL 92)) 
	(1.5 0.5 (NOTE :PITCH 67 :VEL 93)) 
	(2 1 (NOTE :PITCH 62 :VEL 94))))

(test "score-voice1" (score-voice testscore '((note violin)))
      '((0 0 (SCORE-BEGIN-END 0 10)) 
	(0.1 1 (VIOLIN :PITCH 60 :VEL 100)) 
	(1 1 (VIOLIN :PITCH 61 :VEL 91))
	(1 0.5 (VIOLIN :PITCH 68 :VEL 92)) 
	(1.5 0.5 (VIOLIN :PITCH 67 :VEL 93)) 
	(2 1 (VIOLIN :PITCH 62 :VEL 94))))

(test "score-voice2" (score-voice testscore '((flute horn) (note violin))
				      :from-index 0 :to-index 10)
      '((0 0 (SCORE-BEGIN-END 0 10)) 
	(0.1 1 (VIOLIN :PITCH 60 :VEL 100)) 
	(1 1 (VIOLIN :PITCH 61 :VEL 91))
	(1 0.5 (VIOLIN :PITCH 68 :VEL 92)) 
	(1.5 0.5 (VIOLIN :PITCH 67 :VEL 93)) 
	(2 1 (VIOLIN :PITCH 62 :VEL 94))))

'(score-print (score-merge testscore 
				  (score-shift 
				   (score-voice testscore '((note violin)))
				   0.1)))


(test "score-merge1" (score-merge testscore 
				  (score-shift 
				   (score-voice testscore '((note violin)))
				   0.1))
      '((0 0 (SCORE-BEGIN-END 0 10.1))
	(0.1 1 (NOTE :PITCH 60 :VEL 100))
	(0.2 1 (VIOLIN :PITCH 60 :VEL 100))
	(1 1 (NOTE :PITCH 61 :VEL 91))
	(1 0.5 (NOTE :PITCH 68 :VEL 92))
	(1.1 0.5 (VIOLIN :PITCH 68 :VEL 92))
	(1.1 1 (VIOLIN :PITCH 61 :VEL 91))
	(1.5 0.5 (NOTE :PITCH 67 :VEL 93))
	(1.6 0.5 (VIOLIN :PITCH 67 :VEL 93))
	(2 1 (NOTE :PITCH 62 :VEL 94))
	(2.1 1 (VIOLIN :PITCH 62 :VEL 94))
	))

(test "score-merge2" (score-merge 
                       '((0 0 (SCORE-BEGIN-END 0 10))
                         (0 1 (NOTE :PITCH 60 :VEL 100)))
                       '((0 0 (SCORE-BEGIN-END 0 10))
                         (1 1 (NOTE :PITCH 61 :VEL 91)))
                       '((0 0 (SCORE-BEGIN-END 0 10))
                         (2 1 (NOTE :PITCH 62 :VEL 94))))
      '((0 0 (SCORE-BEGIN-END 0 10))
        (0 1 (NOTE :PITCH 60 :VEL 100))
        (1 1 (NOTE :PITCH 61 :VEL 91))
        (2 1 (NOTE :PITCH 62 :VEL 94))))

(test "score-append1" 
      (score-append testscore testscore) 
      '((0 0 (SCORE-BEGIN-END 0 20))
        (0.1 1 (NOTE :PITCH 60 :VEL 100))
        (1 1 (NOTE :PITCH 61 :VEL 91))
        (1 0.5 (NOTE :PITCH 68 :VEL 92))
        (1.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (2 1 (NOTE :PITCH 62 :VEL 94))
        (10.1 1 (NOTE :PITCH 60 :VEL 100))
        (11 1 (NOTE :PITCH 61 :VEL 91))
        (11 0.5 (NOTE :PITCH 68 :VEL 92))
        (11.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (12 1 (NOTE :PITCH 62 :VEL 94))))


(test "score-select1"
      (score-select testscore t :from-time 1 :to-time 1.5)
      '((0 0 (SCORE-BEGIN-END 0 10))
        (1 1 (NOTE :PITCH 61 :VEL 91))
        (1 0.5 (NOTE :PITCH 68 :VEL 92))))

(test "score-select2"
      (score-select testscore 
          '(lambda (time dur expr) (eql (expr-get-attr expr :pitch) 61))
          :from-time 1 :to-time 1.5)
      '((0 0 (SCORE-BEGIN-END 0 10))
        (1 1 (NOTE :PITCH 61 :VEL 91))))

(test "score-select3"
      (score-select testscore 
        '(lambda (time dur expr) (eql (expr-get-attr expr :pitch) 61))
        :reject t)
      '((0 0 (SCORE-BEGIN-END 0 10)) 
        (0.1 1 (NOTE :PITCH 60 :VEL 100)) 
        (1 0.5 (NOTE :PITCH 68 :VEL 92)) 
        (1.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (2 1 (NOTE :PITCH 62 :VEL 94))))

(test "score-filter-length"
      (score-filter-length testscore 1.5)
      '((0 0 (SCORE-BEGIN-END 0 10))
        (0.1 1 (NOTE :PITCH 60 :VEL 100))
        (1 0.5 (NOTE :PITCH 68 :VEL 92))))

(test "score-repeat"
      (score-repeat testscore 3)
      '((0 0 (SCORE-BEGIN-END 0 30))
        (0.1 1 (NOTE :PITCH 60 :VEL 100))
        (1 0.5 (NOTE :PITCH 68 :VEL 92))
        (1 1 (NOTE :PITCH 61 :VEL 91))
        (1.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (2 1 (NOTE :PITCH 62 :VEL 94))
        (10.1 1 (NOTE :PITCH 60 :VEL 100))
        (11 1 (NOTE :PITCH 61 :VEL 91))
        (11 0.5 (NOTE :PITCH 68 :VEL 92))
        (11.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (12 1 (NOTE :PITCH 62 :VEL 94))
        (20.1 1 (NOTE :PITCH 60 :VEL 100))
        (21 1 (NOTE :PITCH 61 :VEL 91))
        (21 0.5 (NOTE :PITCH 68 :VEL 92))
        (21.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (22 1 (NOTE :PITCH 62 :VEL 94))))

(test "score-stretch-to-length"
      (score-stretch-to-length testscore 20)
      '((0 0 (SCORE-BEGIN-END 0 20))
        (0.2 2 (NOTE :PITCH 60 :VEL 100))
        (2 2 (NOTE :PITCH 61 :VEL 91))
        (2 1 (NOTE :PITCH 68 :VEL 92))
        (3 1 (NOTE :PITCH 67 :VEL 93))
        (4 2 (NOTE :PITCH 62 :VEL 94))
        ))

(test "score-filter-overlap"
      (score-filter-overlap testscore)
      '((0 0 (SCORE-BEGIN-END 0 10))
        (0.1 1 (NOTE :PITCH 60 :VEL 100))
        (1.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (2 1 (NOTE :PITCH 62 :VEL 94))))

(test "score-adjacent-events1"
      (score-adjacent-events testscore #'(lambda (a b c) b))
      testscore)

(test "score-adjacent-events2"
      (score-adjacent-events testscore
         #'(lambda (a b c) 
            (if (eql (event-get-attr b :pitch) 61)
	      nil b))) ; remove pitches with 61
      '((0 0 (SCORE-BEGIN-END 0 10)) 
        (0.1 1 (NOTE :PITCH 60 :VEL 100)) 
        (1 0.5 (NOTE :PITCH 68 :VEL 92)) 
        (1.5 0.5 (NOTE :PITCH 67 :VEL 93))
        (2 1 (NOTE :PITCH 62 :VEL 94))))

(test "score-indexof"
      (score-indexof testscore #'(lambda (time dur expr)
                                  (eql (expr-get-attr expr :pitch) 61)))
      2)

(test "score-last-indexof"
      (score-last-indexof testscore #'(lambda (time dur expr)
                                  (> (expr-get-attr expr :pitch) 65)))
      4)


|#



