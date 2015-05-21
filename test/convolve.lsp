; Here is some LISP code that was used to test the function:

(print "----------------------------------------------------------------")
(setf testArray (make-array 10))        ; an array with 10 elements
(dotimes (i 10)
       (setf (aref testArray i) (float i)))   ; fill array with i
(display "convolve test"  testArray)
(print "----------------------------------------------------------------")
(setf h (snd-from-array 0.0 100.0 testArray)) ; make a sound x from testArray

(setf xArray (make-array 20))           ; an array with 10 elements
(dotimes (i 20)
       (setf (aref xArray i) 0.0))   ; fill array with 0.0
(setf (aref xArray 0) 1.0)           ; set first element to 1
(setf (aref xArray 15) 1.0)
(display "convolve test" xArray)
(print "convolve test--------------------------------------------------")
(setf x (snd-from-array 0.0 100.0 xArray))   ; make a sound h from xArray

(setf output (snd-convolve x h))        ; perform convolution

; convert output to an array and print:
(display "convolve test" (snd-samples output 100))
(print "test -----------------------------------------------------------")

(print "Verify proper logical stop time using seq to add samples")
(setf yArray (make-array 10))
(dotimes (i 10)
       (setf (aref yArray i) 10.0))
(setf y (snd-from-array 0.0 100.0 yArray))
(display "test" (snd-samples (seq (cue output) (cue y)) 100))
(print "test (1)--------------------------------------------------------")

; more tests for logical stop time and termination
; 1) set the logical stop time to be the terminate time
; 2) set the logical stop time to be after the terminate time
; 3) set the logical stop time to be before the terminate time
; 4) set the input length to be a multiple of the fft block size (16)

; (1)
(setf output (snd-convolve (set-logical-stop x 0.30) h))

(display "test (1) lst = term time"
         (snd-samples (seq (cue output) (cue y)) 100))
(print "test (2) --------------------------------------------------------")

; (2)
(setf output (snd-convolve (set-logical-stop x 0.40) h))

(display "test (2) lst > term time"
         (snd-samples (seq (cue output) (cue y)) 100))
(print "test (3) --------------------------------------------------------")


; (3)
(setf output (snd-convolve (set-logical-stop x 0.10) h))

(display "test (3) lst < term time"
         (snd-samples (seq (cue output) (cue y)) 100))
(print "test (4) --------------------------------------------------------")



; (4)
(setf xArray (make-array 32))           ; an array with 10 elements
(dotimes (i 32)
       (setf (aref xArray i) 0.0))   ; fill array with 0.0
(setf (aref xArray 0) 1.0)           ; set first element to 1
(setf (aref xArray 15) 1.0)
(display "convolve test" xArray)
(setf x (snd-from-array 0.0 100.0 xArray))   ; make a sound h from xArray

(setf output (snd-convolve x h))

(display "test (4) input is multiple of fft block size"
         (snd-samples (seq (cue output) (cue y)) 100))
(print "block-test -------------------------------------------------------")

(defun block-test ()
  (setf hArray (make-array 8))
  (dotimes (i 8) (setf (aref hArray i) i))
  (setf output (snd-convolve x (snd-from-array 0.0 100.0 hArray)))
  (display "block-test" (snd-samples output 100)))

(block-test)

(defun long-reverb ()
  (snd-convolve (sim (pluck c4 0.1) (at 5 (pluck c5 0.1)))
                (mult (noise 10) (pwev 1 10 0.001))))

; (play (long-reverb))
