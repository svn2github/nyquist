; Here is some LISP code to test the convolve function:

;; First, implement convolution in XLISP on vectors to generate correct results
;;   that we can compare to the output of snd-convolve:

(defun s2v (s) (snd-samples s 10000)) ;; sound to vector

;; zero a vector
(defun zerov (v)
  (dotimes (i (length v)) (setf (aref v i) 0)))

;; add vectors with offset
(defun addv (av bv boffset)
  (let ((cv (make-array (max (length av) (+ (length bv) boffset)))))
    (zerov cv)
    (dotimes (i (length av))
      (setf (aref cv i) (aref av i)))
    (dotimes (i (length bv))
      (setf (aref cv (+ i boffset))
            (+ (aref cv (+ i boffset)) (aref bv i))))
    cv))

;; convolve two vectors
(defun correct-convolve (av bv)
  (let ((cv (make-array (+ (length av) (length bv)))))
    (zerov cv)
    (dotimes (i (length av))
      (dotimes (j (length bv))
        (setf (aref cv (+ i j))
              (+ (aref cv (+ i j))
                 (* (aref av i) (aref bv j))))))
    cv))

;; check if two vectors match within epsilon
(setf epsilon 1.0e-5)
(defun ~= (a b) (< (abs (- a b)) epsilon))
(defun check-result (a b m)
  (cond ((/= (length a) (length b))
         (setf m (strcat "!!! ERROR in " m " - length mismatch")))
        ((dotimes (i (length a))
           (cond ((not (~= (aref a i) (aref b i)))
                  (setf m (format nil "!!! ERROR in ~A - differ at ~A~%" m i))
                  (return t)))))
        (t (format t "\n*** ~A - PASSED\n\n" m)
           (setf m nil)))
  (cond (m
          (format t "test data: ~A (length ~A)~%" a (length a))
          (format t "good data: ~A (length ~A)~%" b (length b))
          (error m))))


(print "TEST 1 - computing convolution")
(setf inp1 (snd-from-array 0 44100.0 
      (setf inpv1
       (vector 1 2 3 4 5 6 5 4 3 2 1 0 0 0 0 1 2 3 4 5 6 7 6 5 4 3 2 1 0 0 0 0 0))))

(setf imp1 (snd-from-array 0 44100.0 
      (setf impv1
       (vector 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(setf resv1 (s2v (snd-convolve inp1 imp1)))


(check-result resv1 (correct-convolve inpv1 impv1) "test 1")

(print "TEST 2 - more convolution ---------------------------------------")
(setf impv2 (make-array 10))        ; an array with 10 elements
(dotimes (i 10)
       (setf (aref impv2 i) (float i)))   ; fill array with i
(display "convolve test 2"  impv2)
(print "----------------------------------------------------------------")
(setf imp2 (snd-from-array 0.0 100.0 impv2)) ; make a sound x from impv2

(setf inpv2 (make-array 20))           ; an array with 10 elements
(dotimes (i 20)
       (setf (aref inpv2 i) 0.0))   ; fill array with 0.0
(setf (aref inpv2 0) 1.0)           ; set first element to 1
(setf (aref inpv2 15) 1.0)
(display "convolve test" inpv2)
(print "convolve test--------------------------------------------------")
(setf inp2 (snd-from-array 0.0 100.0 inpv2))   ; make a sound imp2 from inp2

(setf res2 (snd-convolve inp2 imp2))        ; perform convolution

; convert output to an array and print:
(display "convolve test" (setf resv2 (s2v res2)))

(check-result resv2 (correct-convolve inpv2 impv2) "convolution test 2")

(print "TEST 3 -------------------------------------------------------")
(print "    Verify proper logical stop time using seq to add samples")
(setf tenv (make-array 10))
(dotimes (i 10)
       (setf (aref tenv i) 10.0))
(setf ten (snd-from-array 0.0 100.0 tenv))
;; add ten at the logical stop time
(setf resv3 (s2v (seq (cue res2) (cue ten))))
;; make the correct version
(setf corv3 (addv resv2 tenv (length inpv2)))

(check-result resv3 corv3 "logical stop time test")

; more tests for logical stop time and termination
; 4) set the logical stop time to be the terminate time
; 5) set the logical stop time to be after the terminate time
; 6) set the logical stop time to be before the terminate time
; 7) set the input length to be a multiple of the fft block size (16)

(print "TEST 4 --------------------------------------------------------")

(setf res4 (snd-convolve (set-logical-stop inp2 0.30) imp2))

(display "test 4 lst = term time"
  (setf resv4 (s2v (seq (cue res4) (cue ten)))))

(check-result resv4 (addv resv2 tenv (length resv2))
              "test 4 lst = term time")

(print "TEST 5 --------------------------------------------------------")

(setf res5 (snd-convolve (set-logical-stop inp2 0.40) imp2))

(display "test 5 lst > term time"
  (setf resv5 (s2v (seq (cue res5) (cue ten)))))

(check-result resv5 (addv resv2 tenv 40) "logical stop time test (5)")

(print "TEST 6 --------------------------------------------------------")

(setf res6 (snd-convolve (set-logical-stop inp2 0.10) imp2))

(display "test 6 lst < term time"
  (setf resv6 (s2v (seq (cue res6) (cue ten)))))

(check-result resv6 (addv resv2 tenv 10) "logical stop time test (6)")

(print "TEST 7 --------------------------------------------------------")
; (4)
(setf inpv7 (make-array 32))           ; an array with 32 elements
(dotimes (i 32)
  (setf (aref inpv7 i) 0.0))   ; fill array with 0.0
(setf (aref inpv7 0) 1.0)           ; set first element to 1
(setf (aref inpv7 15) 1.0)
(display "convolve test" inpv7)
(setf inp7 (snd-from-array 0.0 100.0 inpv7))   ; make a sound h from inp2
(setf imp7 imp2)
(setf impv7 impv2)

(setf res7 (snd-convolve inp7 imp7))

(display "test 7 input is multiple of fft block size"
  (setf resv7 (s2v (seq (cue res7) (cue ten)))))

(setf corv7 (correct-convolve inpv7 impv7))

(check-result resv7 (addv corv7 tenv (length inpv7)) "test 7 input len 32")

(print "BLOCK-TEST -------------------------------------------------------")

(setf impv8 (make-array 8))
(dotimes (i 8) (setf (aref impv8 i) i))
(setf res8 (snd-convolve inp7 (snd-from-array 0.0 100.0 impv8)))
(display "block-test" (setf resv8 (s2v res8)))

(check-result resv8 (correct-convolve inpv7 impv8) "block test")

(defun long-reverb ()
  (snd-convolve (sim (pluck c4 0.1) (at 5 (pluck c5 0.1)))
                (mult (noise 10) (pwev 1 10 0.001))))

(play (long-reverb))
