;; this sample code is described in fft_tutorial.htm

(setf fft1-class (send class :new '(sound length skip)))

(send fft1-class :answer :next '() '(
    (snd-fft sound length skip nil)))

(send fft1-class :answer :isnew '(snd len skp) '(
    (setf sound snd)
    (setf length len)
    (setf skip skp)))

(defun make-fft1-iterator (sound length skip)
  (send fft1-class :new (snd-copy sound) length skip))

;; create a 1-second sinusoid with points samples at cycles hz:
(defun short-sine (points cycles)
  (control-srate-abs points (lfo cycles)))

(defun fft-test ()
  (let (fft-iter)
    ;; signal will have 4 cycles in 32 points:
    (setf fft-iter (make-fft1-iterator (short-sine 32 4) 32 32))
    (display "fft-test" (send fft-iter :next))))

(defun ifft-test ()
  (let (fft-iter ifft-snd)
    (setf fft-iter (make-fft1-iterator (short-sine 32 4) 32 32))
    (setf ifft-snd (snd-ifft 0 32 fft-iter 32 NIL))
    (display "fft-ifft" (snd-length ifft-snd 200))
    (display "fft-ifft" (snd-samples ifft-snd 200)) ))

(fft-test)
(ifft-test)

;; try non-power-of-two
(defun fft-test2 ()
  (let (fft-iter)
    ;; signal will have 4 cycles in 32 points:
    (setf fft-iter (make-fft1-iterator (short-sine 25 4) 25 25))
    (display "fft-test2" (send fft-iter :next))))
    
;(fft-test2) -- should report error


; Test fft-test and ifft-test on a cosine using this redefinition:
;
; (defun short-sine (points cycles)
;   (control-srate-abs points (lfo cycles 1.0 *sine-table* 90.0)))


(defun file-fft1 (filename frame-length skip)
  (make-fft1-iterator (s-read filename) frame-length skip))


(defun play-fft1 (iterator skip)
  (play (snd-ifft 0 *sound-srate* iterator skip NIL)))

;; a convenient sound file name (change this to one of your soundfiles):
(setf sfn "D:\\brain\\outro\\soup.wav")

(defun file-test () (play-fft1 (file-fft1 sfn 512 512) 512))


(setf fft-hp-class (send class :new '(source bins)))


(send fft-hp-class :answer :next '() '(
  (let ((frame (send source :next)))
    (cond (frame
           (dotimes (i bins)
             (setf (aref frame i) 0.0))))
    frame)))      


(send fft-hp-class :answer :isnew '(s b) '(
    (setf source s)
    (setf bins b)))


(defun make-fft-hp (source bins)
  (send fft-hp-class :new source bins))


(defun hp-test ()
  (play-fft1 (make-fft-hp (file-fft sfn 512 512) 11) 512))


(defun fm-tone (step mi1 mi2 mi3)
  (let ((hz (step-to-hz step)))
    (setf mi1 (* mi1 hz))
    (setf mi2 (* mi2 hz))
    (setf mi3 (* mi3 hz))
    (fmosc c4 (partial step 
                     (control-srate-abs *sound-srate* 
                       (pwl 0 mi1 0.5 mi2 1 mi3 1))))))


(defun mod-snd () (fm-tone c3 5 7 5)) ;; adjust FM parameters here


(setf fft-modulator-class (send class :new '(src1 src2)))


(send fft-modulator-class :answer :isnew '(s1 s2) '(
    (setf src1 s1)
    (setf src2 s2)))


(send fft-modulator-class :answer :next '() '(
  (let ((frame1 (send src1 :next))
        (frame2 (send src2 :next))
        n half_n)
    (cond ((and frame1 frame2)
           ; multiply frame2 by the amplitude coefficients of frame1
           (setf (aref frame2 0) (* (aref frame2 0) (aref frame1 0))) ;; DC
           (setf n (- (length frame1) 1))
           ; Subtracted 1 because we already took care of DC component
           (setf half_n (/ n 2)) ; integer divide
           (dotimes (i half_n)
             (let* ((i2 (+ i i 2))
                    (i2m1 (- i2 1))
                    (amp (sqrt (+ (* (aref frame1 i2m1) (aref frame1 i2m1))
                                  (* (aref frame1 i2)   (aref frame1 i2))))))                (setf (aref frame2 i2m1) (* (aref frame2 i2m1) amp))
               (setf (aref frame2 i2) (* (aref frame2 i2) amp))))
           (cond ((= n (+ half_n half_n 2)) ;; n is even -> nyquist component
                  (setf (aref frame2 n) (* (aref frame2 n) (aref frame1 n)))))
           frame2)
          (t nil)))))


(defun make-fft-modulator (src1 src2)
  (send fft-modulator-class :new src1 src2))


(defun mod-test ()
  (let ((fs 512)) ;; frame size
    (play-fft1 (make-fft-modulator 
                (make-fft1-iterator (mod-snd) fs fs)
                (file-fft1 sfn fs fs))
              fs)))


(defun raised-cosine ()
  (scale 0.5 
    (sum (const 1) 
         (lfo (/ 1.0 (get-duration 1)) 1 *sine-table* 270))))


(defun fft-window (frame-size)
  (control-srate-abs frame-size (raised-cosine)))


(defun play-fft (iterator frame-size skip)
  (play (snd-ifft 0 *sound-srate* iterator 
                  skip (fft-window frame-size))))


(defun mod-test-w ()
  (let ((fs 512)) ;; frame size
    (play-fft (make-fft-modulator 
                  (make-fft1-iterator (mod-snd) fs (/ fs 2))
                  (file-fft1 sfn fs (/ fs 2)))
                fs (/ fs 2))))


(setf fft-class (send class :new '(sound length skip window)))

(send fft-class :answer :next '() '(
    (snd-fft sound length skip window)))

(send fft-class :answer :isnew '(snd len skp) '(
    (setf sound snd)
    (setf length len)
    (setf skip skp)
    (setf window (fft-window len)) ))

(defun make-fft-iterator (sound length skip)
  (send fft-class :new (snd-copy sound) length skip))

(defun file-fft (filename frame-length skip)
  (make-fft-iterator (s-read filename) frame-length skip))

(defun mod-test-ww ()
  (let ((fs 512)) ;; frame size
    (play-fft (make-fft-modulator 
                (make-fft-iterator (mod-snd) fs (/ fs 2))
                (file-fft sfn fs (/ fs 2)))
              fs (/ fs 2))))

(defun fft-test-w ()
  (let (fft-iter)
    ;; signal will have 4 cycles in 32 points:
    (setf fft-iter (make-fft-iterator (short-sine 32 4) 32 32))
    (display "fft-test-w" (send fft-iter :next))))

(defun ifft-test-w ()
  (let (fft-iter ifft-snd)
    ;; this will generate two frames rather than one as before, and
    ;; with a higher harmonic to help detect the window:
    (setf fft-iter (make-fft1-iterator (short-sine 64 8) 32 32))
    ;; window the result:
    (setf ifft-snd (snd-ifft 0 32 fft-iter 24 (fft-window 32)))
    (display "fft-ifft" (snd-length ifft-snd 200))
    (display "fft-ifft" (snd-samples ifft-snd 200))
    ifft-snd ))


(defun mod-snd-2 () (fm-tone d3 15 27 15)) ;; adjust FM parameters here


(defun mod-test-wwst ()
  (let ((fs 1024)) ;; frame size
    (play-fft (make-fft-modulator 
                (file-fft sfn fs (/ fs 8))
                (make-fft1-iterator (transpose 2 (mod-snd-2)) fs (/ fs 8)))
              fs (/ fs 2))))

;; do analysis synthesis
;;
(defun fft-ifft (sound)
  (play-fft (make-fft-iterator sound 1024 512) 1024 512))

