;Stephen Mangiat
;15-392 Final Project
;Moog Instrument


;Moog Instrument: Main Function
(defun moog (s &key
	(range-osc1 2)
	(range-osc2 1)
	(range-osc3 3)
	(detun2 -.035861)
	(detun3 .0768)
	(noiselevel .05)
	(filter-cutoff 768)
	(Q 2)
	(contour .65)
	(filter-attack .0001)
	(filter-decay .5)
	(filter-sustain .8)
	(shape-osc1 *saw-table*)
	(shape-osc2 *saw-table*)
	(shape-osc3 *saw-table*)
	(volume-osc1 1)
	(volume-osc2 1)
	(volume-osc3 1)
	(amp-attack .01)
	(amp-decay 1)
	(amp-sustain 1)
	(amp-release 0)
	(glide 0))

	(cond ((eq glide 0)	(setf cv (score-to-cv s)))
	(t (setf cv (lp (score-to-cv s) (+ .1 (recip glide))))))

	(cond ((< range-osc1 2) (setf freq1 cv))
	(t (setf freq1 (mult cv (mult (- range-osc1 1) 2)))))

	(cond ((< range-osc2 2) (setf freq2temp cv))
	(t (setf freq2temp (mult cv (mult (- range-osc2 1) 2)))))

	(cond ((< range-osc3 2) (setf freq3temp cv))
	(t (setf freq3temp (mult cv (mult (- range-osc3 1) 2)))))

	(setf freq2 (mult freq2temp (1+ (mult detun2 .0596))))
	(setf freq3 (mult freq3temp (1+ (mult detun3 .0596))))

	(setf osc1 (hzosc freq1 shape-osc1))
	(setf osc2 (hzosc freq2 shape-osc2))
	(setf osc3 (hzosc freq3 shape-osc3))

	(setf mix1 (sum (scale volume-osc1 osc1) 
		(scale volume-osc2 osc2) (scale volume-osc3 osc3)))

	(setf ampenv (score-to-env-trig s 0 0 0 amp-attack amp-decay 	amp-sustain amp-release))
	; noise should be infinite. I hope 10000s is close enough.
	(setf mix2 (sum mix1 (scale noiselevel (noise 10000))))

	(setf durSum 0)
	(setf cutoffenv (score-to-filter-trig s 0 0 0 
		filter-cutoff Q contour filter-attack filter-decay filter-sustain))
	(setf bandwidth (mult (recip Q) cutoffenv))

	(setf mix3 (reson mix2 cutoffenv bandwidth 2))
	(setf mix4 (mult mix3 ampenv))
)

; Convert input list into Control Voltages
(defun score-to-cv (s) 
	(cond ((cdr s)
	(seq (const (step-to-hz (caar s)) (cadar s)) (score-to-cv (cdr s))))
	(t (const (step-to-hz (caar s)) (cadar s))))
)

; Helper functions used to maintain continuity in envelopes
(defun last-value (env1 info)
	(sref env1 (- (cadr info) (recip 2205))))
(defun last-value-2 (env1 info)
	(sref env1 (- info (recip 2205))))


; Create filter cutoff frequencies for Control Voltages
(defun score-to-filter-trig (s start dur-prev art-prev filter-cutoff Q contour attack decay sust)
	(let (env1 finish)
	(cond ((cdr s) (setf env1
	(make-filter start (car s) dur-prev art-prev filter-cutoff Q contour attack decay sust))
	(setf finish (last-value env1 (car s)))
	(seq (mult env1 (const 1 (cadar s)))
	(score-to-filter-trig (cdr s) finish (cadar s) (caddr (car s)) filter-cutoff Q contour attack decay sust)))
	(t (make-filter start (car s) dur-prev art-prev filter-cutoff Q contour attack decay sust)))))

(defun make-filter (start info dur-prev art-prev filter-cutoff Q contour attack decay sust)
	(let ((dur (cadr info)) (art (caddr info)))
	(setf highF (sum (mult 10000 contour) filter-cutoff))
	(setf sust1 (mult sust filter-cutoff))
	(cond ((eq art-prev 1)
		(setf durSum (+ durSum dur-prev))
		(cond ((> attack durSum)
		(mult (const 1 dur) (pwl 0 start (- attack durSum) highF (+ (- attack dur-prev) decay) sust1 dur 		sust1)))

	((> (+ attack decay) durSum)
		(mult (const 1 dur) (pwl 0 start (- (+ attack decay) durSum) sust1 dur sust1)))
	
		(t (const sust1 dur))))
	(t (setf durSum 0) (mult (const 1 dur) (pwl 0 0 attack highF (+ decay attack) sust1 dur sust1))))))

; Create amplitude envelope for Control Voltages
(defun score-to-env-trig (s start dur-prev art-prev attack decay sust release)
	(let (env1 finish)
	(cond ((cdr s) (setf env1
		(make-env-trig start (car s) dur-prev art-prev attack decay sust release))
		(setf finish (last-value env1 (car s)))
		(seq (mult env1 (const 1 (cadar s)))
	(score-to-env-trig (cdr s) finish (cadar s) (caddr (car s)) attack decay sust release)))
	(t (make-env-trig start (car s) dur-prev art-prev attack decay sust release)))))

; Make individual amplitude envelopes. Case checking needed if attack/decay are longer than notes.
(defun make-env-trig (start info dur-prev art-prev attack decay sust release)
	(let ((dur (cadr info)) (art (caddr info)))

	(cond ((eq art-prev 1)
		(cond ((> (+ attack decay) dur-prev)
			(cond ((> (- (+ attack decay) dur-prev) (* dur art))
				(setf art-cutoff (seq (const 1 (* dur art)) (const 0 (- dur (* dur art)))))
				(setf env1 (mult (const 1 dur) (pwl 0 start (- (+ attack decay)
				 dur-prev) sust (* dur art) sust (+ (* dur art) release) 0 dur 0)))
				(setf env2 (mult art-cutoff env1))
				(mult (const 1 dur)
				(sum env2 (pwl 0 0 (* dur art) 0 (+ (* dur art) .00001) 
				(last-value-2 env2 (* dur art)) (+ (* dur art) release) 0 dur 0))))
	
			(t (mult (const 1 dur)
			 (pwl 0 start (- (+ attack decay) dur-prev)
	 		sust (* dur art) sust (+ (* dur art) release) 0 dur 0)))))
		(t  (mult (const 1 dur) (pwl 0 start (* dur art) sust (+(* dur art) release) 0 dur 0)))))

	(t  (cond ((> (+ attack decay) (* dur art))
		(setf art-cutoff (seq (const 1 (* dur art)) (const 0 (- dur (* dur art)))))
		(setf env1 (pwl 0 start attack 1 (+ attack decay) sust (* dur art) sust 
		(+(* dur art) release) 0 dur 0))
		(setf env2 (mult art-cutoff env1))
		(mult (const 1 dur)
		(sum env2 (pwl 0 0 (* dur art) 0 (+ (* dur art) .00001) 
		(last-value-2 env2 (* dur art)) (+ (* dur art) release) 0 dur 0))))
		(t (mult  (const 1 dur)
		(pwl 0 start attack 1 (+ attack decay) sust (* dur art) 
		sust (+(* dur art) release) 0 dur 0))))))))

(defun moog-default-demo ()
  (setf s '((24 .5 .99) (26 .5 .99) (28 .5 .99)
            (29 .5 .99) (31 2 1)))
  (play (moog s)))

(defun moog-articulation-demo ()
  (setf s '((24 .5 .5) (26 .5 1) (28 .5 .25) (29 .5 1) (31 1 .8)))
  (play (moog s :amp-release .2)))

(defun moog-glide-demo ()
  (setf s  '((24 .5 .5) (38 .5 1) (40 .5 .25)
             (53 .5 1) (55 2 1) (31 2 .8) (36 2 .8)))
  (play (moog s :amp-release .2 :glide .5)))

(defun moog-filter-demo ()
  (setf s  '((20 .5 1) (27 .5 1) (26 .5 1) (21 .5 1)
             (20 .5 1) (27 .5 1) (26 .5 1) (21 .5 1)))
  (play (moog s :shape-osc1 *tri-table* :shape-osc2 *tri-table*
              :filter-attack 2 :filter-decay 2
              :filter-cutoff 300 :contour .8 :glide .2 :Q 8)))

(defun moog-sine-demo ()
  (setf s '((80 .4 .75) (28 .2 1) (70 .5 1) (38 1 .5)))
  (play (moog s :range-osc1 3 :range-osc2 2 :range-osc3 4
              :detun2 -.043155 :detun3 .015016
              :noiselevel 0 :filter-cutoff 400 :Q .1
              :contour .0000001 :filter-attack 0
              :filter-decay .01 :filter-sustain 1
              :shape-osc1 *sine-table* :shape-osc2 *sine-table*
              :shape-osc3 *sine-table* :volume-osc1 1 :volume-osc2 1
              :volume-osc3 .1 :amp-attack .1 :amp-decay 0
              :amp-sustain 1 :amp-release .3 :glide 2)))


(defun moog-tri-demo ()
  (setf s '((24 .5 .99) (26 .5 .99) (28 .5 .99)
            (29 .5 .99) (31 2 1)))
  (play (moog s :shape-osc1 *tri-table* :shape-osc2 *tri-table*
              :filter-attack .5 :contour .5)))
