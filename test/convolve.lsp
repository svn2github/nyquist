; Here is some LISP code that was used to test the function:

(setf testArray (make-array 10))        ; an array with 10 elements
(dotimes (i 10)
       (setf (aref testArray i) (float i)))   ; fill array with i
(display "convolve test"  testArray)
(setf h (snd-from-array 0.0 100.0 testArray)) ; make a sound x from testArray

(setf xArray (make-array 20))           ; an array with 10 elements
(dotimes (i 20)
       (setf (aref xArray i) 0.0))   ; fill array with 0.0
(setf (aref xArray 0) 1.0)           ; set first element to 1
(setf (aref xArray 15) 1.0)
(display "convolve test" xArray)
(setf x (snd-from-array 0.0 100.0 xArray))   ; make a sound h from xArray

(setf output (snd-convolve x h))        ; perform convolution

; convert output to an array and print:
(display "convolve test" (snd-samples output 100))

(print "Verify proper logical stop time using seq to add samples")
(setf yArray (make-array 10))
(dotimes (i 10)
       (setf (aref yArray i) 10.0))
(setf y (snd-from-array 0.0 100.0 yArray))
(display "test" (snd-samples (seq (cue output) (cue y)) 100))




