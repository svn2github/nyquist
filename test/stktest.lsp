;; stktest.lsp -- test the STK instruments, currently clarinet and saxophony

(autonorm-off)

;; simple clarinet sound
(defun clarinet-example-1 ()
	(clarinet bf3 (clarinet-breath-env 1 0.2 0.1)))

;; clarinet sound with frequency sweep (glissando)
(defun clarinet-example-2 ()
	(clarinet-freq bf3 (clarinet-breath-env 3 0.2 0.1) (pwl 1.5 80 3 80 3)))

;; clarinet sound with change in breath pressure
(defun clarinet-example-3 ()
	(clarinet bf3 (prod (pwl 0 1 1.5 0.9 3 1 3) (clarinet-breath-env 3 0.2 0.1))))

;; clarinet sound using initial frequency sweep and built-in vibrato effect
(defun clarinet-example-4 ()
	(clarinet-all bf3 (clarinet-breath-env 3 0.5 0.05) (pwl 0.3 80 3 80 3) 5.7 0.5 0 0))

;; clarinet sound with increasing then decreasing reed stiffness
(defun clarinet-example-5 ()
	(clarinet-all bf3 (clarinet-breath-env 3 0.5 0.05) 0 0 0 (pwl 1.5 0.75 3) 0))

;; clarinet sound with increasing noise, with vibrato
(defun clarinet-example-6 ()
	(clarinet-all bf3 (clarinet-breath-env 3 0.5 0.05) 0 5.7 0.5 0 (pwl 3 1 3)))

(print "clarinet-example-1")
(play (clarinet-example-1))
(print "clarinet-example-2")
(play (clarinet-example-2))
(print "clarinet-example-3")
(play (clarinet-example-3))
(print "clarinet-example-4")
(play (clarinet-example-4))
(print "clarinet-example-5")
(play (clarinet-example-5))
(print "clarinet-example-6")
(play (clarinet-example-6))


(defun sax-example-1 ()
  (scale 0.5
   (timed-seq '(
		(0.0 1 (sax g3 (sax-breath-env 2 0.2 0.2 0.6)))
		(2.0 1 (sax-freq c4  (sax-breath-env 4 0.6 0.6) 
                                 (scale 100 (mult (pwl 0 0.95 4 1.3 4)))))
               )))
)

(defun sax-example-2 ()
	(let (fade stacenv genenv)
		(defun fade (dur env) (prod (pwl 0 0 0.005 1 (- dur 0.005) 1 dur 0 dur) env))
		(defun stacenv (dur amp) (scale (* 0.8 amp) (fade (* 0.9 dur) (sax-breath-env (* 0.9 dur) 1.0 0.9))))
		(defun genenv (dur amp) (scale amp (sax-breath-env dur 1.0 1.0 0.75)))
		
		(scale 0.5
		(timed-seq
			'(
				(0.0 1 (sax-freq
					bf3
					(mult (envbreaks 1 (list 0.125 0.25 0.375 0.5 0.625 0.75 0.875))
						(genenv 1 1))
					(freqenv 1 bf3 (list 0 bf3 0.125 af4 0.25 g4 0.375 d4
							0.5 f4 0.625 ef4 0.75 d4 0.875 ef4))
					))
				
				(1.0 1 (sax-freq
					e4
					(mult (envbreaks 1 (list 0.125 0.25 0.375 0.5 0.625 0.75 0.875))
						(genenv 1 1))
					(freqenv 1 e4 (list 0 e4 0.125 c4 0.25 a3 0.375 e3 
							0.5 fs3 0.625 e3 0.75 fs3 0.875 e4))
					))

				(2.0 1 (sax-freq
					d4
					(mult (envbreaks 1 (list 0.125 0.25 0.375 0.5 0.625 0.75 0.875))
						(genenv 1 1))
					(freqenv 1 d4 (list 0 d4 0.125 c4 0.25 b3 0.375 a3 
							0.5 g3 0.625 a3 0.75 b3 0.875 d4))
					))

				(3.0 1 (sax-freq
					ef4
					(mult (envbreaks 1 (list 0.125 0.25 0.375 0.5 0.625 0.75 0.875))
						(genenv 1 1))
					(freqenv 1 ef4 (list 0 ef4 0.125 cs4 0.25 b3 0.375 bf3 
							0.625 gf3 0.75 af3 0.875 bf4))
					))
			)
		))
	)
)

(print "sax-example-1")
(play (sax-example-1))
(print "sax-example-2")
(play (sax-example-2))

