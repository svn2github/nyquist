;; stktest.lsp -- test the STK instruments, currently clarinet and saxophony

(autonorm-off)

;; simple clarinet sound
(defun clarinet-example-1 ()
	(clarinet bf3 (stk-breath-env 1 0.2 0.1)))

;; clarinet sound with frequency sweep (glissando)
(defun clarinet-example-2 ()
	(clarinet-freq bf3 (stk-breath-env 3 0.2 0.1) (pwl 1.5 80 3 80 3)))

;; clarinet sound with change in breath pressure
(defun clarinet-example-3 ()
	(clarinet bf3 (prod (pwl 0 1 1.5 0.9 3 1 3) (stk-breath-env 3 0.2 0.1))))

;; clarinet sound using initial frequency sweep and built-in vibrato effect
(defun clarinet-example-4 ()
	(clarinet-all bf3 (stk-breath-env 3 0.5 0.05) (pwl 0.3 80 3 80 3) 5.7 0.5 0 0))

;; clarinet sound with increasing then decreasing reed stiffness
(defun clarinet-example-5 ()
	(clarinet-all bf3 (stk-breath-env 3 0.5 0.05) 0 0 0 (pwl 1.5 0.75 3) 0))

;; clarinet sound with increasing noise, with vibrato
(defun clarinet-example-6 ()
	(clarinet-all bf3 (stk-breath-env 3 0.5 0.05) 0 5.7 0.5 0 (pwl 3 1 3)))

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
		(0.0 1 (sax g3 (stk-breath-env 2 0.2 0.2)))
		(2.0 1 (sax-freq c4  (stk-breath-env 4 0.6 0.6) 
                                 (scale 100 (mult (pwl 0 0.95 4 1.3 4)))))
               )))
)

(defun sax-example-2 ()
  (scale 0.5
    (timed-seq
     '(
       (0.0 1 (sax-freq
		bf3
                (eight-sixteenths-env)
                (freqenv 1 bf3 (list 0 bf3 0.125 af4 0.25 g4 0.375 d4
		                     0.5 f4 0.625 ef4 0.75 d4 0.875 ef4))))
       (1.0 1 (sax-freq
		e4
                (eight-sixteenths-env)
		(freqenv 1 e4 (list 0 e4 0.125 c4 0.25 a3 0.375 e3 
				     0.5 fs3 0.625 e3 0.75 fs3 0.875 e4))))
       (2.0 1 (sax-freq
		d4
                (eight-sixteenths-env)
		(freqenv 1 d4 (list 0 d4 0.125 c4 0.25 b3 0.375 a3 
				    0.5 g3 0.625 a3 0.75 b3 0.875 d4))))
       (3.0 1 (sax-freq
        	ef4
                (eight-sixteenths-env)
		(freqenv 1 ef4 (list 0 ef4 0.125 cs4 0.25 b3 0.375 bf3 
				     0.625 gf3 0.75 af3 0.875 bf4))))))))

(defun eight-sixteenths-env ()
    (mult (envbreaks 1 (list 0.125 0.25 0.375 0.5 0.625 0.75 0.875))
          (stk-breath-env 1 1 1)))

(defun hzdiff (step1 step2) 
  (- (step-to-hz step2) (step-to-hz step1)))

;; create a piecewise-constant (stairstep) function to control frequencies
;;  timesteplist is (time0 step0 time1 step1 etc.)
;;
(defun freqenv (dur step timesteplist)	
  (let ((finalenv nil) hzdiff (tslist timesteplist) currt currs)
    (do () ((null tslist))
      (setf currt (car tslist))
      (setf currs (cadr tslist))
      (setf tslist (cdr (cdr tslist)))
		
      (setf finalenv (append finalenv
			(list currt
			      (hzdiff step currs)
			      (if (null tslist) dur (car tslist))
			      (hzdiff step currs))
			)))
    (setf finalenv (append finalenv (list dur)))
    (pwl-list finalenv)))	

;; create a breath envelope where pressure goes to zero at designated points
;;   dur is overall duration
;;   breaklist is places where pressure dips to zero during a 20ms window
;;
(defun envbreaks (dur breaklist)
  (let ((finalenv nil))
    (dolist (breakpt breaklist)
      (setf finalenv (append finalenv (list (- breakpt 0.01) 1
					    (- breakpt 0.001) 0
					    breakpt 1))))
    (setf finalenv (append (list 0 1) finalenv (list dur)))
    (pwl-list finalenv)))

(print "sax-example-1")
(play (sax-example-1))
(print "sax-example-2")
(play (sax-example-2))

