;; SDL. ejemplo
;; BWV 1069
;; pmorales. Junio, 2007


(setf *autonorm-type* 'previous)

(load "sdl")

(defun analog-saw (pitch dur atk rel det reson-freq)
  (let ((hz (step-to-hz pitch)))
     (reson
      (mult 0.5
	    (sum -0.1 (pwev 0.1 atk 1 dur 0.8 (+ dur rel) 0.1))
	    (sum (stretch (+ dur rel) (osc-saw  hz))
		 (stretch (+ dur rel) (osc-saw  (+ hz det)))))
      (sum (* 3 hz) (mult (* 8 hz) (sum -0.1 (pwev 1.1 (+ dur rel) 0.1))))
      reson-freq 1)))

(defun sint1 (&key pitch (idur 1) (din 1) (reson 1000))
  (scale din (analog-saw pitch idur 0.002 .3 3 reson)))

(setf voz-1
 '((TF 1)
   (INIT-INSTR "i1" sint1) (INSTRUMENT "i1") (ATTR :mm 100)
   (ATTR :idur 0.1)(PWL-POINT :reson 1000)
   2 (:c4 1)  :d4 (:e4 2) :c4 :g4 :e4 :a4 (:g4 1) :f4 
   (ATTR :idur 0.5)(:g4 4)(ATTR :idur 0.15)
   (:a4 1) :c5 :f4 :a4 :g4 :f4 :e4 :g4 :f4 :a4 :d4 :f4 :e4 :d4 :c4 :d4 :e4 :f4 (:g4 2)
   :a3 :c4 :d4 :f4
   :g3 :b3 :c4 :e4 (:f3 1) :e4 :d4 :c4 :g3 :d4 :c4 :b3
   (ATTR :idur 0.5) (LABEL :t1)(:c4 4) 2 
   (PWL-POINT :reson 2000)
   (ATTR :idur 0.1) (:g3 1) :f3 :e3 :f3 :d3 :e3 (:c3 2) (:c4 3)
   (:b3 1) :a3 :b3 (:c4 2) :a4 :b3 :g4 :a3 :f#4
   (:g3 1) :c4 :b3 :a3 :g3 :f3 :e3 :d3 :c3 :b3 :a3 :g3 :f#3 :e3 :d3 :c3 :b2 :a3 :g3 :f#3
   :e3 :d3 :c3 :b2 :a2 :g3 :f#3 :e3 :d3 :c3 :b2 :a2 :g2 :g3 :b3 :d4 
   (ATTR :idur 0.5) (:e4 4) 1 
   (ATTR :idur 0.2)(:f3 1) :a3 :c4 (ATTR :idur 2)(:d4 4)
   (PWL-POINT :reson 4000)
))

(setf voz-2 
  '((TF 1) 
    (INIT-INSTR "i1" sint1) (INSTRUMENT "i1") (ATTR :mm 100) 
    (ATTR :idur 0.15) (PWL-POINT :reson 1000)
    (AT-LABEL :t1)2 (:g4 1) :a4 (:b4 2) :g4
    :c5 :b4 :e5 (:d5 1) :c5
    (ATTR :idur 2)(:d5 4)
    (PWL-POINT :reson 1000)
    (ATTR :idur 0.1) (:e5 1) :g5 :c5 :e5 :d5 :c5 :b4 :d5 :c5 :e5 :a4 :c5
    :b4 :a4 :g4 :a4 :b4 :c5 (:d5 2) :e4 :g4 :a4 :c5 :d4 :f#4 :g4 :b4 
    (:c4 1) :b4 :a4 :g4 :d4 :a4 :g4 :f#4 (:g4 4)
    1 (:c5 1) :e5 :g5 
    (ATTR :idur 1)(:a5 5) 
    (PWL-POINT :reson 4000)
    (ATTR :idur 0.8)(:b4 1) :d5 :f5
    (ATTR :idur 1)(:g5 5)))

(setf *my-time-labels* (sdl->timelabels voz-1))

(defun vox-1 ()  (timed-seq  (sdl:normalize-score-duration (sdl->score voz-1))))
(defun vox-2 ()  (timed-seq  (sdl:normalize-score-duration (sdl->score voz-2 *my-time-labels*))))

(play (sim (vox-1) (vox-2)))
I
