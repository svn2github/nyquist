;; reverse.lsp -- reverse sounds and files
;;

(setf *max-reverse-samples* 25000000) ;; about 100MB of memory
(setf *reverse-blocksize* 10000) ;; how many to reverse at a time

(defun s-reverse (snd) (multichan-expand #'nyq:s-reverse snd))

(defun nyq:s-reverse (snd)
  (let ((now (local-to-global 0)))
    (setf len (snd-length snd *max-reverse-samples*))
    (cond ((= len *max-reverse-samples*)
           (error 
            "s-reverse cannot reverse a sound longer than *max-reverse-samples*")))
    (abs-env (at-abs now (nyq:s-reverse-from snd len)))))

(defun nyq:s-reverse-from (snd len)
  (cond ((> len *reverse-blocksize*)
         (seq (nyq:reverse-some-samples snd (- len *reverse-blocksize*) 
                                    *reverse-blocksize*)
              (nyq:s-reverse-from snd (- len *reverse-blocksize*))))
        (t
         (nyq:reverse-some-samples snd 0 len))))

(defun nyq:reverse-some-samples (snd offset len)
  (display "reverse-some-samples" (snd-length snd 20000) offset len)
  (let ((samps (snd-samples  (nyq:extract-samples snd offset len) len))
        (i2 (1- len)))
    (display "reverse-some-samples" (length samps))
    (dotimes (i1 (/ len 2))
      (let ((s1 (aref samps i1))
            (s2 (aref samps i2)))
        (setf (aref samps i1) s2)
        (setf (aref samps i2) s1)
        (setf i2 (1- i2))))
    (snd-from-array (local-to-global 0) (snd-srate snd) samps)))

(defun nyq:extract-samples (snd offset len)
  (let (start stop)
    (setf start (/ offset (snd-srate snd)))
    (setf stop (+ start (/ len (snd-srate snd))))
    (display "nyq:extract-samples" start stop (snd-t0 snd))
    (extract-abs start stop snd)))

;(play (s-reverse (s-read "sample.wav")))

(defun s-read-reverse (filename &key (time-offset 0) (srate *sound-srate*)
                       (dur 10000) (nchans 1) (format *default-sf-format*)
                       (mode *default-sf-mode*) (bits *default-sf-bits*)
                       (endian nil))
  (let (fsrate fdur channels rslt)
    ;; first, read the sound just to get the duration and rate of the file
    (setf rslt (s-read filename :time-offset time-offset :srate srate :dur dur 
                     :nchans nchans :format format :mode mode 
                     :bits bits :endian endian))
    (if (null rslt) (error "s-read-reverse could not open file" filename))
    (setf channels (cadr *rslt*))
    (setf *rslt* (cddddr *rslt*))
    (setf fsrate (cadr *rslt*))
    (display "s-read-reverse" filename srate channels)
    (setf fdur (caddr *rslt*))
    (setf time-offset (max 0 (min fdur time-offset)))
    (setf dur (max 0 (min (- fdur time-offset) dur)))
    (cond ((> channels 1)
           (setf rslt (make-array channels))
           (dotimes (i channels)
             (setf (aref rslt i)
                   (nyq:s-reverse-file filename time-offset fsrate dur 
                                       channels format mode bits endian i)))
           rslt)
          (t (nyq:s-reverse-file filename time-offset fsrate dur
                                 channels format mode bits endian nil)))))


;; nyq:s-reverse-file -- do the work of reversing one channel of a file
;; 
;; if nchans > 1, chan is the channel number to read
;;
(defun nyq:s-reverse-file (filename time-offset srate dur nchans
                           format mode bits endian chan)
  (let ((blockdur (/ *reverse-blocksize* srate)))
    (if (> dur blockdur)
        (seq (nyq:reverse-some-samples 
              (nyq:s-read-chan filename
                    (+ time-offset dur (- blockdur))
                    srate (/ *reverse-blocksize* srate)
                    nchans format mode bits endian chan)
              0 *reverse-blocksize*)
             (nyq:s-reverse-file filename time-offset srate (- dur blockdur)
                      nchans format mode bits endian chan))
        (nyq:s-read-chan filename time-offset srate dur nchans format
                 mode bits endian chan))))


;; nyq:s-read-chan -- grab some samples from one channel of a file
;;
(defun nyq:s-read-chan (filename time-offset srate dur nchans format
                        mode bits endian chan)
  (let (rslt)
    (setf rslt 
          (if (= nchans 1)
              (s-read filename :time-offset time-offset :srate srate
                       :dur dur :nchans nchans :format format :mode mode
                        :bits bits :endian endian)
              (aref (s-read filename :time-offset time-offset :srate srate
                       :dur dur :nchans nchans :format format :mode mode
                        :bits bits :endian endian)
                    chan)))
    (if (not rslt) (error "nyq:s-read-chan could not read part of file" filename))
    rslt))


;(play (s-read-reverse "sample.wav"))
;(play (s-read-reverse "test.wav"))

            
         