;; examples.lsp -- these are from the Nyquist Manual examples

(defun ex1 ()
  (play (osc 60)))

(defun ex2 ()
  (play (scale 0.5 (osc 60))))

; build-harmonic is already defined in nyquist.lsp
;
;(defun build-harmonic (n) (snd-sine 0 n tablesize 1))

(defun mkwave ()
  (setf *table* (sim (scale 0.5  (build-harmonic 1.0 2048))
                    (scale 0.25  (build-harmonic 2.0 2048))
                    (scale 0.125 (build-harmonic 3.0 2048))
                    (scale 0.062 (build-harmonic 4.0 2048))))
  (setf *table* (list *table* (hz-to-step 1) T)))

(cond ((not (boundp '*mkwave*))
       (mkwave)
       (setf *mkwave* t)))

(defun note (pitch dur) 
  (osc pitch dur *table*))

(defun ex3 ()
  (play (seq (note c4 i)
             (note d4 i)
             (note f4 i)
             (note g4 i)
             (note d4 q))))

(defun env-note (p)
  (mult (note p 1.0)
        (env 0.05 0.1 0.5 1.0 0.5 0.4)))

(defun ex4 ()
  (play (env-note c4)))


(defun ex5 ()
  (play (seq (stretch 0.25 
                      (seq (env-note c4)
                           (env-note d4)))
             (stretch 0.5
                      (seq (env-note f4)
                           (env-note g4)))
             (env-note c4))))

(defun ex6 ()
  (play (seq (note c4 q) (note d4 i))))


(defun ex7 ()
  (play (scale 0.5 (sim (note c4 q) (note d4 i)))))

;; previous versions could not get current-path to locate demo-snd.aiff...
;(format t "~%examples.lsp tries to load demo-snd.aiff from the~%")
;(format t "default sound file directory, which is stored in~%")
;(format t "the variable *default-sf-dir*. The current value is:~%")
;(format t "\"~A\". If you get an error immediately, you should~%"
;        *default-sf-dir*)
;(format t "either set *default-sf-dir* or copy demo-snd.aiff~%")
;(format t "where Nyquist will find it.~%")

(setf a-snd (s-read (strcat (current-path) 
                            "demo-snd.aiff")))

(defun ex8 ()
  (play a-snd))

(defun ex9 ()
  (play (seq (cue a-snd) (cue a-snd))))

(defun ex10 ()
  (play (sim (at 0.0 (cue a-snd))
             (at 0.7 (cue a-snd))
             (at 1.0 (cue a-snd))
             (at 1.2 (cue a-snd)))))

(defun ex11 ()
  (play (sim (cue a-snd)
             (loud 6.0 (at 3.0 (cue a-snd))))))

(defun ex12 ()
  (play (loud 6.0 (sim (at 0.0 (cue a-snd))
                       (at 0.7 (cue a-snd))))))

(defun snds (dly)
  (sim (at 0.0 (cue a-snd))
       (at 0.7 (cue a-snd))
       (at 1.0 (cue a-snd))
       (at (+ 1.2 dly) (cue a-snd))))

(defun ex13 ()
  (play (snds 0.1)))

(defun ex14 ()
  (play (loud 0.25 (stretch 0.9 (snds 0.3)))))

(defun ex15 ()
  (play (sound-srate-abs 44100.0 (osc c4))))

(defun tone-seq ()
  (seqrep (i 16)
          (stretch 0.25 (osc-note c4))))	

(defun pitch-rise () (stretch 4.0 (scale 16 (ramp))))

(defun chromatic-scale ()
  (transpose (pitch-rise) (tone-seq)))

(defun ex16 ()
  (play (chromatic-scale)))

(defun ex17 ()
  (play (sustain (stretch 4 (sum 0.2 (ramp)))
                 (chromatic-scale))))

(defun warper ()
  (pwl .25 .4 .75 .6 1.0 1.0 2.0 2.0 2.0))

(defun warp4 () (stretch 4 (scale 4 (warper))))

(defun ex18 ()
  (play (warp (warp4) (tone-seq))))

; note: as explained in the manual, the following is NOT
; the solution to a fixed duration/variable tempo sequence:
;
(defun tone-seq-2 ()
  (seqrep (i 16)
          (stretch-abs 0.25 (osc-note c4))))

(defun ex19 ()
  (play (warp (warp4) (tone-seq-2))))

; here is the proper solution (vs. ex19):
;
(defun tone-seq-3 ()
  (seqrep (i 16)
          (set-logical-stop
             (stretch-abs 0.25 (osc-note c4))
             0.25)))

(defun ex20 ()
  (play (warp (warp4) (tone-seq-3))))

(defun ex21 ()
  (play (warp (warp4)
              (transpose (pitch-rise) (tone-seq)))))

(defun ex22 ()
  (play (warp (warp4)
              (transpose (control-warp (get-warp)
                                       (warp-abs nil (pitch-rise)))
                         (tone-seq)))))

(if (not (boundp 'a-snd)) 
    (setf a-snd 
       (s-read "demo-snd.aiff")))

(defun ex23 ()
  (play (force-srate *default-sound-srate* (stretch 3.0 (sound a-snd)))))

(defun down ()
  (force-srate *default-sound-srate*
    (seq (stretch 0.2 (sound a-snd))
         (stretch 0.3 (sound a-snd))
         (stretch 0.4 (sound a-snd))
         (stretch 0.5 (sound a-snd))
         (stretch 0.6 (sound a-snd)))))

(defun ex24 () (play (down)))

(defun up ()
  (force-srate *default-sound-srate*
    (seq (stretch 0.5 (sound a-snd))
         (stretch 0.4 (sound a-snd))
         (stretch 0.3 (sound a-snd))
         (stretch 0.2 (sound a-snd)))))

(defun ex25 ()
  (play (seq (down) (up) (down))))

(defun ex26 ()
  (s-save a-snd 1000000000 "./a-snd-file.snd")
  (play-file "./a-snd-file.snd")
  (system "rm ./a-snd-file.snd"))

(defun ex27 ()
  (setf my-sound-file "./a-snd-file.snd")
  (s-save a-snd 1000000000 my-sound-file)
  (play-file my-sound-file)
  (system (strcat "rm " my-sound-file)))

(defun ex28 ()
  ; normalize in memory.  First, assign the sound to a variable so 
  ; it will be retained:
  (setf mysound (sim (osc c4) (osc c5)))
  ; now compute the maximum value (ny:all is a 1 giga-samples, you may want a
  ; smaller constant if you have less than 4GB of memory :-):
  (setf mymax (snd-max mysound NY:ALL))
  (display "Computed max" mymax)
  ; now write out and play the sound from memory with a scale factor:
  (play (scale (/ 1.0 mymax) mysound)))

(defun ex29 ()
  ; if you don't have space in memory, here's how to do it:
  (defun myscore () (sim (osc c4) (osc c5)))
  ; compute the maximum, don't forget the quote!:
  (setf mymax (snd-max '(myscore) NY:ALL))
  (display "Computed max" mymax)
  ; now we know the max, but we don't have a the sound (it was garbage
  ; collected and never existed all at once in memory).  Compute the sound
  ; again, this time with a scale factor:]
  (play (scale (/ 1.0 mymax) (myscore))))

(defun ex30 ()
  (play (fmosc c4 (pwl 0.1))))

(defun ex31 ()
  (play (fmosc c4 (pwl 0.5))))

(defun ex32 ()
  (play (fmosc c4 (pwl 0.5 (step-to-hz c4) 0.501))))

(defun ex33 ()
  (setf *fm-voice* (list 
                  (extract 0.110204 0.13932 (cue a-snd))
                  24.848422
                  T))
  (play (fmosc cs2 (pwl 0.5 (step-to-hz cs2) 0.501) 
               *fm-voice* 0.0)))

(defun sweep (delay pitch-1 sweep-time pitch-2 hold-time)
  (let ((interval (- (step-to-hz pitch-2)
                     (step-to-hz pitch-1))))
    (pwl delay 0.0 
         ; sweep from pitch 1 to pitch 2
         (+ delay sweep-time) interval
         ; hold until about 1 sample from the end
         (+ delay sweep-time hold-time -0.0005) interval
         ; quickly ramp to zero (pwl always does this,
         ;    so make it short)
         (+ delay sweep-time hold-time))))


(defun ex34 ()
  (play (fmosc cs2 (sweep 0.1 cs2 0.6 gs2 0.5) 
               *fm-voice* 0.0)))

(defun ex35 () 
  (play (fmosc cs2 (scale 10.0 (lfo 6.0)) 
               *fm-voice* 0.0)))

(defun ex36 ()
  (let (modulator)
    (setf modulator (mult (pwl 1.0 1000.0 1.0005)
                          (osc c4)))
    (play (fmosc c4 modulator))))

;;; FINDING ZERO CROSSINGS, SND-SAMPLES

(setf max-samples-for-zeros 1000)

(defun zeros (snd)
  ; start by getting the samples, only take 1000 samples max
  (prog ((s (snd-samples snd max-samples-for-zeros))
         newsign sign n len result result2 starttime srate)
    ; go through the array looking for zero crossings
    (setf len (length s))
    ; stop if there are no samples
    (if (= len 0) (return nil))
    (setf sign (> 0.0 (aref s 0)))
    ; get the start time and sample rate of the sound for use below
    (setf starttime (car (snd-extent snd max-samples-for-zeros)))
    (setf srate (snd-srate snd))
    (setf n 1)
   loop
    (if (>= n len) (go done))
    (setf newsign (> 0.0 (aref s n)))
    (if (not (eq sign newsign)) (setf result (cons n result)))
    (setf sign newsign)
    (setf n (1+ n))
    (go loop)
   done ; now we have the zero crossings, convert them to times
    (dolist (num result)
      ; return the time of the zero crossing, which is the start time
            of the snd plus the sample number / srate

      (setf result2 (cons (+ starttime (/ num srate))
                          result2)))
                       
    (return result2)))

(defun ex37 ()
  ; extract a short piece of this sample
  (setf short (extract 0.1 0.14 (cue a-snd)))
  (setf z (zeros short))
  (format t "Zero crossings from a-snd: ~A~%" z))


; find the differences between zero crossings reported by zeros
; print the result in terms of samples for readability
;
(defun periods (lis short)
  (prog (result prev srate)
    (if (null lis) (return nil))
    (setf srate (snd-srate short))
   loop
    (setf prev (car lis))
    (setf lis (cdr lis))
    (if (null lis) (return (reverse result)))
    (setf result (cons (* srate (- (car lis) prev)) result))
    (go loop)))

(defun ex38 ()
  ; ex38 depends upon z, set by (ex37)
  (cond ((not (boundp 'z)) (ex37)))
  (setf p (periods z short))
  (format t "The intervals (in samples) between zero crossings are: ~%~A~%" p))


; build a wavetable using zero crossing information
;
; I interactively played with the data and decided to extract from the
; 5th period to the 21st period (these had 86 and 87 samples each and
; seem to indicate some kind of periodicity).  The 1st period measures
; from the zeroth zero crossing to the first, so the 5th period measures
; from the 4th zero crossing to the 5th.  I'll arbitrarily take
; the 4th and 20th zero crossing times (the 5th and 20th should work as
; well), and from the data, this looks like 2 waveform periods.
; This is very clear if you plot the data.
;
; arguments are:
;  snd - the sound to extract from
;  zeros - the result of (zeros snd)
;  start - the number of the starting zero crossing
;  stop - the number of the ending zero crossing
;  n - number of periods contained in the extracted sound
;
(defun extract-table (snd zeros start stop n)
  (let (starttime extent hz)
    ; Start by shifting snd to time zero:
    (setf starttime (car (snd-extent snd max-samples-for-zeros)))
    (setf snd (at (- starttime) (cue snd)))

    (format t "~A~%" snd)
    ; also get the start and stop times and shift them:
    (setf start (- (nth start zeros) starttime))
    (setf stop (- (nth stop zeros) starttime))

    (format t "table ~A start ~A stop ~A~%" snd start stop)

    ; now extract the samples of interest, note that we are
    ; overwriting our pointer to the snd argument
    (setf snd (extract start stop (cue snd)))
    (format t "table now ~A~%" snd)

    ; now we need to figure out the pitch this sound would represent
    ; when played at its samplerate.  The pitch in hz is 1 / duration,
    ; and duration is the extent of the sound / n.  Therefore, take
    ; n/extent
    (setf extent (snd-extent snd max-samples-for-zeros))
    (setf hz (/ n (- (cadr extent) (car extent))))
    ; an osc table is a list of the sound, pitch number, and T (periodic)
    (list snd (hz-to-step hz) T)))


(defun ex39 ()
  ; try it out
  (setf *a-voice* (extract-table short z 4 20 2))
  ; now use the table with an oscillator
  (play (osc c3 1.0 *a-voice* )))

(defun ex40 ()
  ; play it at its normal pitch
  (play (osc (cadr *a-voice*) 1.0 *a-voice*)))

(defun ex41 ()
  (play (noise)))

(defun ex42 ()
  (play (lp (noise) 1000.0)))

(defun ex43 ()
  (play (hp (noise) 1000.0)))

; low pass sweep from 100 hz to 2000 hz
(defun ex44 ()
  (play (lp (noise) (pwl 0.0 100.0 1.0 2000.0 1.0))))

; high pass sweep from 50 hz to 4000 hz
(defun ex45 ()
  (play (hp (noise) (pwl 0.0 50.0 1.0 4000.0 1.0))))

; band pass at 500 hz, 20 hz bandwidth
(defun ex46 ()
  (play (reson (scale 10.0 (noise)) 500.0 20.0 1)))

; band pass sweep from 100 to 1000 hz, 20 hz bandwidth
(defun ex47 ()
  (play (reson (scale 0.04 (noise))
               (pwl 0.0 200.0 1.0 1000.0 1.0) 20.0)))

(format t "\nType (ex1) through (ex47) to run these examples.\n")
(format t "See demos/stktest.lsp for more simple sound examples.\n")
(format t "\nI'm turning off Auto-normalization.  See AUTONORM-ON\n")
(format t "in the documentation for an explanation:\n\n")
(autonorm-off)

(defun ex-all ()
  (format t
   "Warning: turning automatic normalization feature off~%")
  (autonorm-off)
  (dotimes (i 47)
           (let ((j (+ i 1)) fn)
             (setf fn (intern (format nil "EX~A" j)))
             (setf fn (list fn))
             (format t "~A~%" fn)
             (eval fn))))

(format t "\n(ex-all) will compute and play all examples for testing purposes.\n")
