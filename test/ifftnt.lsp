;; test code for ifft framework

; The interface to SND-IFFT is:
;   (snd-ifft t0 sr iterator)
; where t0 is the starting time,
; sr is the sample rate, and 
; iterator is an XLisp object

; the iterator object must return an array of samples when :next is sent
; or return NIL to end the sound
; The sound returned by SND-IFFT (for now) is simply the concatenation 
; of all samples returned in arrays returned from :next.

; TEST IT....

; first, make a class:

(setf iter-class (send class :new '(count len)))

; now define some methods
; for this test, we'll return "count" arrays of length "len" and
; we'll fill the arrays with a ramp from -1 to +1 so that the final
; result will be a sawtooth wave

(send iter-class :answer :set-count '(c) '((setf count c)))
(send iter-class :answer :set-len '(l) '((setf len l)))
(send iter-class :answer :next '() '(
    (format t "iter-class got :next\n")
    (cond ((<= count 0) nil)
          (t
            (let (res)
              (setf count (- count 1))
              (setf res (make-ramp-array len))
              (display "iter-class returns" res)
              res)))))

(defun make-ramp-array (len)
  (let (ar)
    (setf ar (make-array len))
    (dotimes (i len)
       (setf (aref ar i) (+ -1 (* i (/ 2.0 len)))))
    ar))

; now try calling SND-IFFT with an object
(setf iter (send iter-class :new))
(send iter :set-count 10)
(send iter :set-len 20)

(print "Select SPLIT SCREEN item in CONTROL menu on Macs for good screen layout")

(defun ifft-test ()
  (s-save (snd-ifft 0.0 100 iter) ny:all "test.wav"))


;; fft code: make an object that returns ffts when called with :NEXT
;;
(setf fft-class (send class :new '(sound length skip)))

(send fft-class :answer :next '() '((snd-fft sound length skip)))
; there's a way to do this with new, but I forgot how...
(send fft-class :answer :init '(snd len skp) '(
    (setf sound snd)
    (setf length len)
    (setf skip skp)))
    
(defun make-fft-iterator (sound length skip)
  (let (iter)
     (setf iter (send fft-class :new))
     ; make a copy because the snd-fft will modify the sound:
     (send iter :init (snd-copy sound) length skip)
     iter))

;; print ffts of a short ramp:
(defun fft-test ()
  (let (fft-iter)
    (setf fft-iter (control-srate-abs 100
                     (make-fft-iterator (pwl 1 1 1 0) 32 32)))
    (dotimes (i 5) (print (send fft-iter :next)))))
                     
;; now try running the ffts through the ifft:
(defun fft-ifft-test ()
  (let (fft-iter ifft-snd)
    (setf fft-iter (control-srate-abs 100
                     (make-fft-iterator (pwl 1 1 1 0) 32 32)))
    (setf ifft-snd (snd-ifft 0 100 fft-iter))
    (display "fft-ifft" (snd-length ifft-snd 200))
    (display "fft-ifft" (snd-samples ifft-snd 200))
    (s-save ifft-snd ny:all "test.wav")))

   
