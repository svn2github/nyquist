;nyquist plug-in
;version 1
;type generate
;name "Shephard glissando"
;action"Creating Shephard glissando"
;info "Shephard-Risset glissando\nwritten by Erich Neuwirth"
;control pitch1 "Start pitch" real "MIDInote" 60 24 96 
;control pitch2 "End pitch" real "MIDInote" 72 24 96 
;control cpitch1 "Start filter pitch" real "MIDInote" 60 24 96 
;control cpitch2 "End filter pitch" real "MIDInote" 60 24 96 
;control overtonesemi "Overtone interval" real "MIDInote" 12 3 24
;control overtones "Overtones" int "" 4 1 8
;control duration "Duration" real "seconds" 1.5 0.3 30
;control skew "Tone skewness" real "" 0.0 0.0 0.99

(setf overtones (truncate overtones))

(setf *onepi* 3.141592654)
(setf *twopi* (* 2 pi))
(setf *halfpi* (/ pi 2))


(defun make-table (func-exp points)
       (let ((table (make-array points)))
            (dotimes (i points)
                     (setf (aref table i)
                           (funcall func-exp (/ (float i) (float points)))))
             (list (snd-from-array 0.0 points table) (hz-to-step 1) T)
             ))


(defun erich-wave (skew)
       (make-table
           (lambda (x) (if (< (abs skew) 0.000001) (sin (* *twopi* x))
                                  (*
                                  (/ (sin (* *twopi* x)) (- (/ 1.0 skew)
                                     (cos (* *twopi* x))))
                                  (/ (
                                  sqrt (- 1.0 (* skew skew))) skew))))
            2048))


(defun pitchenv (pitch centerpitch halfwidth)
               (let ((xarg (abs (/ (- (float pitch) centerpitch) halfwidth))))
                    (if (> xarg 1) 0
                        (/ (+ (cos (* *onepi* xarg)) 1.0) 2.0))
               ))




; envshaper is shifted from 0 to 2
; it transforms (0 2) into 0 1
; to use it as envelope distorter, it has to be used like
; (shape s (envshaper) 1)

(defun envshaper ()
       (mult (sum 1 (hzosc (const (/ 1.0 2.0) 2) *table* 270)) 0.5))


; some utility functions

(defun normalize (s &optional (maxvol 0.8) (maxlen  44100))
       (let* ((mysound s)
             (vol (peak mysound maxlen)))
             (scale (/ (float maxvol) vol) mysound)))


(defun buffer (s t1 duration t2)
     (let ((timebase (hzosc (pwl (+ duration t1 t2)))))
	      (sim timebase (at t1 (cue s)))))


(defun iseq-helper (a b)
       (let ((mylist '()))
            (dotimes (i (1+ (- b a)) (reverse mylist))
                     (setf mylist (cons (+ a i) mylist)))))


(defun iseq (a b)
       (if (> a b) (reverse (iseq-helper b a))
                   (iseq-helper a b)))


(defun floor (x)
       (if (< x 0)
           (1- (truncate x))
           (truncate x)))

(defun realrem (x mod)
       (- (float x) (* (floor (/ (float x) (float mod))) (float mod))))


(defun sheptone-sweep-helper (pitch-1 centerpitch-1
                             duration
                             pitch-2 centerpitch-2
                             overtonesemi overtones
       &optional (wavetable *sine-table*))
   (let ((mytone (const 0 duration))
         (maxovertones (+ (floor (/ (float (max (abs (- pitch-1 centerpitch-2))
                            (abs (- pitch-1 centerpitch-2))))
                            overtonesemi))
                            overtones 2))
         (ampshaper (envshaper))
         )
         (dolist (i (iseq (-  maxovertones) maxovertones) mytone)
                 (progn
                       (setf startpitch (+ pitch-1 (* i overtonesemi)))
                       (setf endpitch (+ pitch-2 (* i overtonesemi)))
                       (setf f (pwe 0 (step-to-hz startpitch)
                                      duration (step-to-hz endpitch)))
                       (setf p (pwl 0 startpitch duration endpitch))
                       (setf c (pwl 0 centerpitch-1 duration centerpitch-2))
                       (setf normwidthfactor (/ 1.0 (* overtones overtonesemi)))
                       (setf a (shape (mult (diff p c) normwidthfactor)
                                       ampshaper 1))
                       (setf voice  (mult a (hzosc f wavetable)))
                       (setf mytone (sum mytone voice))
                 )
                 )))



(defun sheptone-sweep (pitch-1 centerpitch-1 duration pitch-2 centerpitch-2
                     overtonesemi overtones
                     &optional (wavetable *sine-table*))
       (normalize
       (mult (sheptone-sweep-helper pitch-1  centerpitch-1
                                    duration
                                    pitch-2 centerpitch-2
                                    overtonesemi overtones wavetable)
             (env 0.05 0 0.05 1 1 1 duration)))
      )


(setf result (buffer (sheptone-sweep pitch1 cpitch1 
		duration pitch2 cpitch2 overtonesemi overtones) 0.1 duration 0.2))

result
