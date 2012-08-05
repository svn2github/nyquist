; *********************************************
; Inventio # 8 a 2 voci BWV 779. J. S. Bach.
; Score Description Library example
; Translated from a previous lambda Music score
; by Pedro J. Morales.
; Junio, 2005
; Revised: July 25 2007
; Nyquist 2.37
; *********************************************

(load "sdl")

(setf *autonorm-type* 'previous)


(defun analog-saw (pitch dur atk rel det reson-freq)
  (let ((hz (step-to-hz pitch)))
    (set-logical-stop
     (reson
      (mult 0.5
	    (sum -0.1 (pwev 0.1 atk 1 dur 0.8 (+ dur rel) 0.1))
	    (sum (stretch (+ dur rel) (osc-saw  hz))
		 ; (scale 0.5 (stretch (+ dur rel) (osc-saw (* 2 hz))))
		 (stretch (+ dur rel) (osc-saw  (+ hz det)))))
      (sum (* 3 hz) (mult (* 8 hz) (sum -0.1 (pwev 1.1 (+ dur rel) 0.1))))
      reson-freq 1)
     dur)))


(defun xan2 (&key pitch)
  (analog-saw pitch  0.001 0.02 2.0 2 4000))

(defun xan3 (&key pitch (rel 2.5) (atk 0.02)(reson 4000)(din 1))
  (scale din (analog-saw pitch 0.001 atk rel 3 reson)))


; SCORE --------------------------------------------------------------------

(setf voz-1
            '((ATTR :mm 120.0) ; 140?
	      (INIT-INSTR "i1" xan3) (INSTRUMENT "i1")
	      (PWL-POINT :rel 1.5)(ATTR :atk 0.005)
             (DELTA 2) (f4 2) a4 f4 c5  f4
(PWL-POINT :rel 2.5) 
f5 (e5 1) d5 c5 d5 c5 :bb4 a4 :bb4 a4 g4 (f4 2)
             a4 c5 a4 f5 c5
; linea 2
            (PWL-POINT :rel 1.0)
            (a5 1) c6 :bb5 c6 a5 c6 :bb5 c6 a5 c6 :bb5 c6
            :f5 :a5 :g5 :a5  :f5 :a5 :g5 :a5 :f5 :a5 :g5 :a5            
            :d5 :f5 :e5 :f5 :d5 :f5 :e5 :f5 :d5 :f5 :e5 :f5
; linea 3
	    (PWL-POINT :rel 3.0)
          (MRK n5)
            (:b4 2) :g4 :d5 :b4 :f5 :d5
            (MRK n6)
            (:g5 1) :a5 :g5 :f5 :e5 :f5 :e5 :d5 :c5 :d5 :c5 :bb4
            (:a4 2)
            (MRK n7)
            (:d5 1) :c5 :b4 :c5 :b4 :a4 :g4 :a4 :g4 :f4
; linea 4
            :e4 :f4 :e4 :d4 (MRK n8)(:c4 2)
            (:c5 1) :b4 (:c5 2) 
            (PWL-POINT :rel 4)
            :e4 (PWL-POINT :rel 2) :f4 :c5 :e4 :c5 :d4 :b4 (MRK n9)
            (PWL-POINT :rel 1.5)(:c5 4) 4 (MRK n10) 4
; linea 5
            2 (:c5 2) :e5 :c5 :g5 :c5 :c6 (:b5 1) :a5 :g5 :a5 :g5 :f5 :e5 :f5 :e5
            (MRK n11) :d5 :c5 :bb4 :c5 :a5
            :c5 :a5 :bb4 :a5 :c5 :a5 :a4 :a5
; linea 6
            (PWL-POINT :rel 2.5)
            (:bb4 2)(PWL-POINT :rel 2) :g4 :bb4 :g4 :d5 :g4 :g5 (:f5 1) :eb5 :d5 :eb5 :d5 :c5 :bb4 :c5 :bb4 :a4 (:g4 2) :bb4 :d5 :bb4 :g5 :d5
; linea 7
            (PWL-POINT :rel 3)
            :bb5 :c#5 :bb5 :c#5 :bb5 :c#5
            (MRK n12)
            :d5 :a4 :f5 :d5 :a5 :f5
            (MRK n12b)
            (:g5 1) :f5 :g5 :bb5 :c5 :bb5 :d5 :bb5 :e5 :bb5 :c5 :bb5
; linea 8
            (MRK n12c)
            :f5 :e5 :f5 :a5  :b4 :a5 :c#5 :a5 :d5 :a5 :b4 :a5
            :e5 :d5 :e5 :g5  :a4 :g5 :b4 :g5 :c#5 :g5 :a4 :g5
            (MRK n13)
            (:f5 2) :d5 :bb4 :d5 :g4 :f5
; linea 9
            (MRK n14)
            :e5 :c5 :a4 :c5 :f4 :eb5
            (MRK n15)
            (:d5 1) :f5 :eb5 :f5 :d5 :f5 :eb5 :f5 :d5 :f5 :eb5 :f5
            (MRK n16)
            :bb4 :d5 :c5 :d5 :bb4 :d5 :c5 :d5 :bb4 :d5 :c5 
            (PWL-POINT :rel 3) :d5
            (PWL-POINT :rel 1)
; linea 10
            (MRK n17)
            :g4 :bb4 :a4 :bb4 :g4 :bb4 :a4 :bb4 :g4 :bb4 :a4 (PWL-POINT :rel 1.5) :bb4
            (MRK n18)
            (PWL-POINT :rel 2)
            (:e4 2) :c4 :g4 :e4 :bb4 :g4
            (MRK n19)
            (:c5 1) :d5 :c5 :bb4 :a4 :bb4 :a4 :g4 :f4 :g4 :f4 :eb4
; linea 11
            (:d4 2)
            (PWL-POINT :rel 4)
            (MRK n20)
            (:g4 1) :f4 :e4 :f4 :e4 :d4 :c4 :d4 :c4 :bb3 :a3 :bb3 :a3 :g3
            (:f3 2) (PWL-POINT :rel 2)
            (MRK n21)
            (:f4 1) :e4 (:f4 2) (MRK rall) :a3  
; rallentando
            (:bb3 2.2) (:f4 2.4) (:a3 2.6) (PWL-POINT :rel 1) (:f4 2.8)
            (MRK n22) (PWL-POINT :rel 3.2)
            (:g3 3) (:e4 3.4) (MRK final)(DUR 4) (PWL-POINT :rel 5)(ATTR :din 0.5)(CH :a3 :c4 :f4)))
(setf voz-2

            '((ATTR :mm 120.0)
	      (INIT-INSTR "i1" xan3) (INSTRUMENT "i1")
              (PWL-POINT :reson 100)(PWL-POINT :rel 1)(PWL-POINT :din 2.5)
	      (CHN 6)(PATCH 47) (DELTA 12)(DELTA 2) (:f3 2) :a3 :f3 :c4 :f3 
	      (PWL-POINT :rel 1.5)(PWL-POINT :reson 500)(PWL-POINT :din 3)
	      :f4 (:e4 1) :d4 :c4 :d4 :c4 :bb3 :a3 :bb3 :a3 :g3
	      (PWL-POINT :din 1.5)
; linea 2
            (:f3 2) :a3 :c4 :a3 :f4 :c4 (:a4 1) :c5 :bb4 :c5 :a4 :c5 :bb4 :c5 :a4 :c5 :bb4 :c5
            :f4 :a4 :g4 :a4 :f4 :a4 :g4 :a4 :f4 :a4 :g4 :a4
; linea 3
	    (PWL-POINT :reson 2500)
            :d4 :f4 :e4 :f4 :d4 :f4 :e4 :f4 :d4 :f4 :e4 :f4
            (:b3 2) :g3 :c4 :g3 :e4 :c4 
	    (PWL-POINT :rel 2)
	    (:f4 1) :g4 :f4 :e4 :d4 :e4 :d4 :c4 :b3 :c4 :b3 :a3
; linea 4
	    (PWL-POINT :din 1)(PWL-POINT :rel 2)
            (:g3 2) (:c4 1) :b3 :a3 :b3 :a3 :g3 :f3 :g3 :f3 :e3 :d3 :e3 :d3 :c3 :g3 :f3 :e3 :f3 (:g3 2) :g2
	    (PWL-POINT :rel 1.5)(PWL-POINT :reson 300)(PWL-POINT :din 3)
            2 :c3 (PWL-POINT :rel 2) :e3 :c3 :g3 :c3
; linea 5
	    (PWL-POINT :rel 2.5)
	    (PWL-POINT :rel 1.5)(PWL-POINT :reson 500)
            :c4 (:b3 1) :a3 :g3 :a3 :g3 :f3 :e3 :f3 :e3 :d3
            (:c3 2) :e3 :g3 :e3 :c4 :g3 :eb4 :f#3 :eb4 :f#3 :eb4 :f#3
; linea 6
            :g3 (:f3 1) :eb3 :d3 :eb3 :d3 :c3 :bb2 :c3 :bb2 :a2
	    (PWL-POINT :rel 1.5)(PWL-POINT :din 1)
            (:g2 2) :g3 :bb3 :g3 :d4 :g3 :g4 (:f4 1) :eb4 :d4 :eb4 :d4 :c4 :bb3 :c4 :bb3 :a3
; linea 7
	    (PWL-POINT :rel 1.5)(PWL-POINT :reson 1500)(PWL-POINT :din 2)
            :g3 :f3 :g3 :e4 :g3 :e4 :f3 :e4 :g3 :e4 :e3 :e4
            :f3 :e3 :f3 :d4 :f3 :d4 :e3 :d4 :f3 :d4 :d3 :d4
            (:bb3 2) :g3 :e3 :g3 :c3 :e3
; linea 8
            :a3 :f3 :d3 :f3 :b2 :d3 :g3 :e3 :c#3 :e3 :a2 (PWL-POINT :rel 1) :c#3
            (PWL-POINT :rel 1.5)
	    (:d2 1) :d3 :c3 :d3 :g2 :d3 :a2 :d3 :bb2 :d3 :g2 (PWL-POINT :din 1.) :d3
; linea 9
	    (PWL-POINT :din 1.)(PWL-POINT :rel 2)(PWL-POINT :reson 2000)
            :c2 :c3 :bb2 :c3 :f2 :c3 :g2 :c3 :a2 :c3 :f2 :c3
            (:bb2 2) :d3 :f3 :d3 :bb3 :f3
	    (PWL-POINT :din 2)(PWL-POINT :reson 1000)
            (:d4 1) :f4 :eb4 :f4 :d4 :f4 :eb4 :f4 :d4 :f4 :eb4 :f4
; linea 10
            :bb3 :d4 :c4 :d4 :bb3 :d4 :c4 :d4 :bb3 :d4 :c4 :d4
            :g3 :bb3 :a3 :bb3 :g3 :bb3 :a3 :bb3 :g3 :bb3 :a3 :bb3
            (:e3 2) :c3 :f3 :c3 :a3 :f3
; linea 11
            (:bb3 1) :c4 :bb3 :a3 
	    :g3  :a3 :g3  :f3 :e3 :f3 :e3 :d3 (:c3 2)
            (:f3 1) :e3 :d3 :e3 :d3 :c3 :bb2 :c3 :bb2 :a2 
	    (:g2 1.1) :a2 (:g2 1.2) :f2
            (:c3 1.3) :bb2 (:a2 1.4) :bb2 (:c3 3.2)
	    (PWL-POINT :din 1)
	    (PWL-POINT :rel 2)(PWL-POINT :reson 3500) (:c2 3.4) (PWL-POINT :rel 4)(:f2 4)))

(autonorm-off)
(play
  (scale 0.5
    (sim
      (scale 1 (timed-seq (sdl->score voz-1)))
      (timed-seq (sdl->score voz-2)))))
 
