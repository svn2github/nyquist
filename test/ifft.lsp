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
        (setf count (- count 1))
        (make-ramp-array len)))))

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
  ;(s-plot (snd-ifft 0.0 100 iter nil)))
  (play (snd-ifft 0.0 44100.0 iter 20 nil)))


;; fft code: make an object that returns ffts when called with :NEXT
;;
(setf fft-class (send class :new '(sound length skip)))

(send fft-class :answer :next '() '((snd-fft sound length skip nil)))
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
    (dotimes (i 5) (print (list 'fft (send fft-iter :next))))))
             
;; now try running the ffts through the ifft:
(defun fft-ifft-test ()
  (let (fft-iter ifft-snd)
    (setf fft-iter (control-srate-abs 100
             (make-fft-iterator (pwl 1 1 1 0) 32 32)))
    (setf ifft-snd (snd-ifft 0 100 fft-iter))
    (display "fft-ifft" (snd-length ifft-snd 200))
    (display "fft-ifft" (snd-samples ifft-snd 200))
    (s-plot ifft-snd)))

;(fft-ifft-test)

(defun square (x) (* x x))

(defun amplitude-spectrum (spectrum)
  (let ((result (make-array (+ 1 (/ (length spectrum) 2)))))
    (setf (aref result 0) (aref spectrum 0))
    (dotimes (i (/ (- (length spectrum) 1) 2))
      (setf (aref result (1+ i)) (sqrt (+ (square (aref spectrum (+ 1 (* 2 i))))
                                     (square (aref spectrum (+ 2 (* 2 i)))) ))))
    (cond ((evenp (length spectrum))
           (setf (aref result (/ (length spectrum) 2))
                 (aref spectrum (- (length spectrum) 1)))))
    result))


;; test fft on sinusoids
;;
;; length 32 ffts, lfo has period 16
;; should show as 2nd harmonic
;;
(defun sin-test ()
  (let (fft-iter)
    (setf fft-iter (control-srate-abs 32 
                    (make-fft-iterator (stretch 4 (lfo 2)) 32 32)))
    (dotimes (i 5) (print (list 'fft-sin-test-amplitude-spectrum 
                                (amplitude-spectrum 
                                 (cadr (print (list 'sin-test-spectrum
                                  (send fft-iter :next))))))))))

(setf spectrum (vector 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(dotimes (i 32) (setf (aref spectrum i) (float (aref spectrum i))))

(setf sine-class (send class :new '(count len)))

; now define some methods
; for this test, we'll return "count" arrays of length "len" 

(send sine-class :answer :set-count '(c) '((setf count c)))
(send sine-class :answer :set-len '(l) '((setf len l)))
(send sine-class :answer :next '() '(
    (format t "sine-class got :next\n")
    (cond ((<= count 0) nil)
      (t
        (setf count (- count 1))
        spectrum))))

(setf sin-iter (send sine-class :new))
(send sin-iter :set-count 10)
(send sin-iter :set-len 20)

(defun sin-gen-test ()
  (play (snd-ifft 0.0 44100.0 sin-iter 20 nil)))

;;TODO: (sin-gen-test) should generate a sinusoid tone with a period of 16 samples
