
(setf *maintable* (sim (scale 0.9 (build-harmonic 1 2048))
                     (scale 0.2 (build-harmonic 2 2048))
                     (scale 0.3 (build-harmonic 3 2048))
                     (scale 0.3 (build-harmonic 4 2048))
                     (scale 0.2 (build-harmonic 5 2048))
                     (scale 0.2 (build-harmonic 6 2048))))

(setf *maintable* (list *maintable* (hz-to-step 1.0) T))

(setf *overtable* (sim (scale 1 (build-harmonic 1 2048))))

(setf *overtable* (list *overtable* (hz-to-step 1.0) T))

(defun maintone (note vibe)
  (fmosc note (scale 0.3 (lfo vibe)) *maintable* 0.0)
)

(defun overtone (note vibe vol) 
  (scale vol (mult (fmosc note (scale 0.2 (lfo 16)) *overtable* 0.1) 
		    (sum (const 1) (scale 0.2 (lfo vibe))))))


(defun bandsweep (low hi)
  (hp (lp (noise) (pwev (+ low 5) 1 (+ hi 5))) (pwev low 1 hi)))

(play (timed-seq `(
		   (0 30 (overtone ef7 .75 0.4))
		   (1 30 (overtone ds7 1.3 .3))
		   (1 30 (overtone cs7 .36 .3))
		   (1 30 (overtone gs7 .8 .5))		   
		   (1.5 5 (scale 0.7 (bandsweep 200 2000)))
		   (1 7 (scale 0.7 (bandsweep 500 8000)))
		   (2 2 (pluck c4))
		   (5 5 (pluck e4))
		   (7 10 (pluck g4))
		   )
		 )
      )
       
