;; spectral-process.lsp -- functions to simplify spectral processing
;;   in SAL
;;
;; Roger B. Dannenberg
;; Mar 2013

;; API:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; set sp-obj = sp-init(sa-obj, fn-name, [p1 [, p2 ...]])
;;
;; sp-init() creates a spectral-processing object.
;;   sa-obj is a spectral analysis object returned by sa-init()
;;     (see spectral-analyis.lsp for details.)
;;   fn-name is a symbol naming a function with at least 2
;;     parameters, which are sa-obj and a spectral frame
;;     obtained by calling sa-next(sa-obj).
;;   p1, p2, etc. are additional optional parameters. If present,
;;     they are also passed to the function named by fn-name.
;;   The value returned by fn-name is a list consisting of a
;;     spectral frame, p1, p2, etc. If there are no optional
;;     parameters p1, p2, etc., the function should still return
;;     a list, only there will be just one element, the frame.
;;
;;   A stream of spectral frames is created by first calling
;; sa-next(sa-obj) to obtain the next spectral frame from sa-obj.
;; The function named by fn-name is called with sa-obj, the
;; spectral frame, and p1, p2, ... . The function returns a
;; frame and new values for p1, p2, ..., all in one list.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set sa-frame = sa-next(sp-obj)
;;
;; An sp-obj created by sp-init() has the same interface as
;; an sa-obj created by sa-init(), so in particular, sa-next()
;; can be used to retrieve the next frame. You can also pass
;; an sp-obj to sp-init() to chain spectral processes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set sound = sp-to-sound(sp-obj, skip-period: <seconds>,
;;                                 window: <window-type>)
;;
;; Reconstruct a sound at the default sound sample rate
;; from a series of spectral frames.
;;
;; sp-obj is a spectral-processing object created by
;;     sp-init() or sa-init()
;; skip-period is the time interval in seconds between
;;     successive overlapping inverse FFT windows. The
;;     default is the source (analysis) skip period.
;;     Time stretching or compression will result if
;;     this value differs from the skip period used for
;;     analysis.
;; window specifies the type of window. The default is raised
;;     cosine (Hann or "Hanning") window. Options include
;;     :hann, :hanning, :hamming, :none, nil, where :none and
;;     nil mean a rectangular window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "spectral-analysis.lsp")

(setf sp-class (send class :new '(src fn-name state)))

(send sp-class :answer :isnew '(source function-name parameters) '(
    (setf state parameters)
    (setf src source)
    (setf fn-name function-name)))


(send sp-class :answer :next '() '(
  (let ((frame (sa-next src))
        rslt)
    (cond (frame
            (setf rslt (apply fn-name (cons src (cons frame state))))
            (setf state (cdr rslt))
            (car rslt))
          (t nil)))))


(defun sp-init (src fn-name &rest state)
  (setf *sp-init-fn-name* fn-name) ;; special addition to support grading
  (send sp-class :new src fn-name state))

(send sp-class :answer :info '() '(
  (format t "Spectral Processing object (instance of sp-class):~%")
  (format t "  processing function: ~A~%" fn-name)
  (format t "  current state: ~A~%" state)
  (format t "  the following describes the source of frames:~%")
  (send src :info)))

(send sp-class :answer :plot '(frame) '(
  (send src :plot frame)))

(send sp-class :answer :get-bin-width '() '(
  (send src :get-bin-width)))

(send sp-class :answer :get-fft-size '() '(
  (send src :get-fft-size)))

(send sp-class :answer :get-fft-dur '() '(
  (send src :get-fft-dur)))                                        

(send sp-class :answer :get-fft-window '() '(
  (send src :get-fft-window)))

(send sp-class :answer :get-skip-period '() '(
  (send src :get-skip-period)))

(send sp-class :answer :get-fft-skip-size '() '(
  (send src :get-fft-skip-size)))

(send sp-class :answer :get-sample-rate '() '(
  (send src :get-sample-rate)))

(defun sp-to-sound(sp-obj &key skip-period window)
  (let ((len (send sp-obj :get-fft-size))
        (sr (send sp-obj :get-sample-rate))
        skip)
    (cond ((null skip-period)
           (setf skip-period (send sp-obj :get-skip-period))))
    (setf skip (round (* skip-period sr)))
    (cond ((null window)
           (setf window :hann)))
    (display "sp-to-sound" sr skip window)
    (setf window (sa-get-window-type window))
    (snd-ifft (local-to-global 0) sr
              sp-obj skip (sa-compute-window len window))))


;;;;;;;;;;; TESTS ;;;;;;;;;;;

;; Function used to process frames. In this simple case there is one state
;; variable, count. Note that the return value is a list of the frame and
;; the state variables. The frame here is returned unaltered.
;;
(defun reconstruct (src frame count)
  ;(display "reconstruct" count)
  (list frame (1+ count)))

(defun onepulse (before after)
  (seq (stretch before (hzosc 0))
       (stretch (/ 2.0 44100.0) (hzosc 11025))
       (stretch after (hzosc 0))))

; this test uses an fft window of size len, with a step size
; of len / in-over on the input file, and a step of
; len / out-over on the output synthesis
;
(defun reconstruct-test (len in-over out-over)
  (setf sa (sa-init :input "./rpd-cello.wav"
                    :fft-dur (/ len 44100.0)
                    :skip-period (/ len in-over 44100.0)
                    :window :hann))
  (setf sp (sp-init sa 'reconstruct 0))
  (play (sp-to-sound sp :skip-period (/ len out-over 44100.0))))

; (reconstruct-test 4096 8 4)

