; SOUNDS

(defun mkwave ()
        (setf *s-table* (scale 0.5 (sim (scale 1 (build-harmonic 1.41 2048))
                        (scale 0.7 (build-harmonic 3.1 2048))
                        (scale 0.9 (build-harmonic 6.3 2048))
                        (scale 0.4 (build-harmonic 9.12 2048)))))
        (setf *s-table* (list *s-table* (hz-to-step 1) T)))

(if (not (boundp '*s-table*)) (mkwave))

(defun mk-voc1-table()
        (if (not (boundp 'voc-snd1)) (setf voc-snd1 (s-read "./test/voc1.snd")))
        (setf *voc-table1* (list voc-snd1 16 T)))

(defun mod(dur) 
  (mult 
        (stretch dur (pwl 0 1000 .2 200 .5 8000 1 100 1))
        (fmosc c4 (stretch dur (pwl 0 1 .5 3.25 1 .74 1)))))

(defun mod2(dur)
  (mult
        (stretch dur (pwl 0 5000 0.6 3000 1 50 1))
        (fmosc c4 (stretch dur (pwl 0 10 0.5 50 0.65 1060 0.75 200 0.8 8400 1 5 1)))))

(defun envl(dur) (stretch dur (env 0.15 0.15 0.2 1 .6 .5 1)))

(defun blurp(dur) (fmosc c3 (mult (osc 07 dur) (mod dur))))

(defun bleerp(&optional (dur 4)) (fmosc 02 (mult (fmosc 23 (stretch dur (pwl 0 15000 .3 5600 .8 1500 1 380 1))) (mod dur))))

(defun ther(freq amp &optional (vib 6))
        (mult (sum amp (lfo vib)) (fmosc 0 (sum freq (lfo (* vib 0.7)))) ))

(defun fizz(scl &optional (dur 2) (pch 40))
  (scale scl
        ( mult 
                (stretch dur (env 0.02 0.08 0.01 1 0.6 0.8 1)) (fmosc pch (mod2 dur))
                ( at 0.1 ( mult (envl dur) (fmosc (* pch 1.414) (mod2 dur))))
                ( at 0.2 (mult (envl dur) (fmosc (* pch 0.57) (mod2 dur)))) )))

(defun warble(&optional (dur 1) (pch 60))
          (sum	(mult
                (stretch dur (env 0.017 0.1 0.004 1 0.7 0.8 1))
                (amosc pch (fmosc (hz-to-step 8) (stretch dur (pwl 0 4 0.2 -4 0.56 9 0.7 0 1 -8 1))))
                
                (mult (stretch (* dur 0.96) (env 0.2 0.09 0.07 0.92 0.8 0.6 1))
                (amosc pch (fmosc (* pch 1.414) (stretch dur (pwl 0.2 80 0.5 4 0.9 1120  1 200 1)) ) ) )) ))

(defun bell(dur pch) (mult 
        (stretch dur (env 0.01 0.1 0.2 1 0.6 0.24 1)) 
        (fmosc pch (sum
                (mult (stretch dur (pwl 0.07 1800 0.15 1000 0.4 680 0.8 240 1 100 1)) 
                (osc (hz-to-step (* (step-to-hz pch) (sqrt 2.0))) dur))
                
                (scale 1.57 (mult (stretch dur (pwl 0.001 1000 0.007 450 0.01)) 
                (osc (hz-to-step (* (step-to-hz pch) (sqrt 11.0))))))

                (scale 1.3 (mult (stretch dur (pwl 0.002 921 0.009 600 0.012)) 
                (osc (hz-to-step (* (step-to-hz pch) (sqrt 71.0))))))) )))

(defun ring(dur pch scl) (mult 
        (stretch dur (env 0.05 0.1 0.2 1 0.6 0.24 1)) 
        (fmosc pch (mult 
                (stretch dur (pwl 0.07 1800 0.15 1000 0.4 680 0.8 240 1 100 1)) 
                (mult (osc (hz-to-step (* (step-to-hz pch) (sqrt 2.0))) dur) 
                        (scale scl (osc (hz-to-step (* (step-to-hz pch) (sqrt 11.0))) dur)) )))))

(defun wind(&optional (dur 3) (scal 3) (cps 590) (bw 40))
        (mult (stretch dur (env 0.07 0.08 0.1 1 0.6 0.8 1))
        (sum 
        (stretch dur (reson (scale scal (noise)) cps bw 2))
        (stretch dur (reson (scale (mult scal 1.13) (noise)) (mult cps (pwl 0 0.74 0.2 0.96 0.5 0.8 0.75 1.16 0.9 0.97 1 0.74 1)) (mult bw 1.042) 2)))) )

(defun suck(dur)
        (stretch dur (hp (noise) (pwl 0 15 0.2 6000 0.6 15000 0.75 7))))

(defun vocrap(&optional (pch 16) (dur 1))
        (if (not (boundp '*voc-table1*)) (mk-voc1-table))
        (fmosc pch (stretch dur(pwl 0 3 0.1 -20 0.2 20 0.3 30 0.4 -10 0.5 15 0.6 0 0.8 -30 1 60 1)) *voc-table1*))

(defun voc1(baspch varpch &optional (dur 1))
        (display "duration" dur)
        (if (not (boundp '*voc-table1*)) (mk-voc1-table))
        (fmosc baspch (stretch dur varpch) *voc-table1*))

(defun sparkle()
        
)	
        
;______________________________________________________________________________________
;UTILITIES 

(defun pl(exp) 
        (setf the_max (snd-max exp 10000000))
        (display "Max" the_max)
        (play (scale (/ 1.0 the_max) exp)))


(defun ster(sound pan)
        (vector (mult sound pan)
                (mult sound (sum 1 (mult -1 pan)))))


(defun echo (sound &optional (delay 0.2) (reps 12))
        (seqrep (i reps) (scale (- 1 (* (+ i 1) (/ 1.0 reps))) (seq sound (s-rest delay)) )))

;------------------------------------------------------------
(defun ster-echo (sound &optional (delay 0.2) (reps 12))
        (vector 
                (seqrep (i reps) (scale (- 0.95 (* (+ i 1) (/ 1.0 reps))) (seq (aref (cond ((oddp i) (ster sound 0.1)) (T (ster sound 0.9))) 0) (s-rest delay))))
                (seqrep (i reps) (scale (- 1.0 (* (+ i 1) (/ 1.0 reps))) (seq (aref (cond ((oddp i) (ster sound 0.1)) (T (ster sound 0.9))) 1) (s-rest delay))))
))


(defun ster-echo (sound &optional (delay 0.2) (reps 12))
        (vector 
                (seqrep (i reps) (scale (- 0.95 (* (+ i 1) (/ 1.0 reps))) (seq (cond ((oddp i) sound) (T (scale 0 sound))) (s-rest delay))))
                (seqrep (i reps) (scale (- 1.0 (* (+ i 1) (/ 1.0 reps))) (seq (cond ((not (oddp i)) sound) (T (scale 0 sound))) (s-rest delay))))
))

;------------------------------------------------------------

(defun loop(exp &optional (rep 5))
        (simrep (i rep) (at (- (* i (snd-stop-time exp)) 0.15) exp )))

;______________________________________________________________________________________
;RANDOM LOOKUP

(defun pch-table(oct)
        (setf pch-t (vector (+ 12 (* oct 12)) (+ 13 (* oct 12)) (+ 14 (* oct 12)) (+ 15 (* oct 12)) (+ 16 (* oct 12)) (+ 17 (* oct 12)) (+ 18 (* oct 12)) (+ 19 (* oct 12)) (+ 20 (* oct 12)) (+ 21 (* oct 12)) (+ 22 (* oct 12)) (+ 23 (* oct 12)) )))

(defun dur-table()
        (setf dur-t (vector 1 1.5 1.25 1.75 2 3)))

(defun time-table()
        (setf time-t (vector 0.1 0.2 0.4 0.8)))

(defun rand-sel(v) 
        ;;;(if (not (boundp v)) (pch-table oct))
        (setf v (symbol-value v))
        (aref v (random (- (length v) 1))))

(pch-table 5)
(dur-table)
(time-table)

(defun chimes (n)
        (simrep (i n) (at (* i (rand-sel time-t)) (bell (rand-sel dur-t) (step-to-hz (rand-sel 'pch-t))))))

;_______________________________________________________________________________________
; SCORE SECTIONS

(defun bellpat1() (scale 0.6 (sim
        (scale 0.7 (at 0.0 (bell 1 90)))
        (scale 0.6 (at 0.25 (bell 1.2 78)))
        (scale 0.7 (at 0.5 (bell 0.8 85)))
        (scale 0.55 (at 0.675 (bell 0.7 87)))
        (scale 0.6 (at 0.75 (bell 0.6 88)))
        (scale 0.7 (at 1 (bell 1 86)))
)))

(defun bellpat2() (scale 0.6 (sim
        (scale 0.7 (at 0.0 (bell 1.2 67)))
        (scale 0.55 (at 0.125 (bell 0.8 74)))
        (scale 0.65 (at 0.25 (bell 1.3 67)))
        (scale 0.5 (at 0.5 (bell 0.5 79)))
        (scale 0.7 (at 0.75 (bell 1.5 74)))
)))

(defun ringpat1() (scale 0.8 (sim
        (scale 0.6 (at 0.0 (ring 0.6 45 2)))
        (scale 0.5 (at 0.2 (ring 0.8 40 1.5)))
        (scale 0.8 (at 0.6 (ring 1 44 1)))
        (scale 0.7 (at 0.8 (ring 1.2 32 0.8)))
)))

(defun ringpat2() (scale 0.65 (sim
        (scale 0.8 (at 0.0 (ring 1 39 1.95)))
        (scale 0.7 (at 0.45 (ring 0.7 27 1.7)))
        (scale 0.9 (at 0.6 (ring 0.9 32 1.88)))
        (scale 0.75 (at 1.05 (ring 0.7 36 1.6)))
        (scale 0.8 (at 1.2 (ring 0.8 37 1.78)))
        (scale 0.7 (at 1.5 (ring 0.8 34 1.8)))
        (scale 0.75 (at 1.8 (ring 2 32 2.6)))
)))

(defun techno(rep) (seqrep (i rep) (scale 0.8 (sim
        (scale 0.8 (at 0.0 (ring 0.4 30 1.2)))
        (scale 0.6 (at 0.2 (ring 0.2 30 0.9)))
        (scale 0.7 (at 0.3 (ring 0.1 30 1.1)))
))))

(defun suckpat(&optional (rep 16)) 
        (seqrep (i rep) (stretch 0.2 (scale (* i 0.1) (seq (suck 1) (suck 1) (suck 2) (suck 2))))))

(defun tribal(rep) (scale 0.8 (simrep (i rep) (at (* i 0.9) (sim
                (at 0.0 (bell 0.2 72))
        (scale 0.7 (at 0.15 (bell 0.2 72)))
        (scale 0.8 (at 0.3 (bell 0.2 60)))
        (scale 0.6 (at 0.45 (bell 0.2 65)))
        (scale 0.9 (at 0.6 (bell 0.2 69)))
        (scale 0.7 (at 0.75 (bell 0.2 60)))
)))))


(defun bells(rep) (scale 0.4 (sim
        (bell 0.9 72)
        (at 0.3 (simrep (i rep) (at (* i 0.9 ) (sim
        (scale 0.7 (at 0.0 (bell 0.85 67)))
        (scale 0.8 (at 0.15 (bell 0.85 69)))
        (scale 0.9 (at 0.3 (bell 0.8 71)))
        (scale 0.8 (at 0.45 (bell 1.2 67)))
                (at 0.6 (bell 1.2 72 ))
)))))))

(defun rings (&optional (rep 12) (init_del 0.1) (sep 0.12) (lenfac 0.2))
        (scale 0.12 (simrep (i rep) (at (- 4 (+ init_del (* sep i))) (ring (* i lenfac) 24 (+ 1 (* i 0.4)) )))))

(defun rings2 (&optional (rep 8))
        (simrep (i rep) (at (* 0.02 i) (vocrap))))

(defun bleerps (&optional (rep 12) (init_del 0.1) (sep 0.12) (lenfac 0.2))
        (scale 0.12 (simrep (i rep) (at (- 4 (+ init_del (* sep i))) (bleerp (* i lenfac))))))


(defun accel_bleerps() (sim
        
        (scale 0.4 (at 0.0 (bleerp 4.5)))
        (scale 0.1 (at 0.6 (bleerp 1)))
        (scale 0.12 (at 1.3 (bleerp 0.8)))
        (scale 0.16 (at 1.9 (bleerp 0.7)))
        (scale 0.20 (at 2.3 (bleerp 0.6)))
        (scale 0.24 (at 2.7 (bleerp 0.5)))
        (scale 0.28 (at 3.0 (bleerp 0.4)))
        (scale 0.32 (at 3.2 (bleerp 0.3)))
        (scale 0.25 (at 3.4 (bleerp 0.2)))
        (scale 0.14 (at 3.55 (bleerp 0.2)))
        (scale 0.10 (at 3.7 (bleerp 0.2)))
        (scale 0.06 (at 3.8 (bleerp 0.2)))
        (scale 0.03 (at 3.9 (bleerp 0.2)))
))


(defun sect1() (sim
        (scale 3 (at 0.0 (simrep(i 4) (at (* i 2.9) (wind)))))
        (scale 0.5 (at 2 (warble 8 48)))
        (scale 0.3 (at 2.05 (warble 8.05 47.9)))
        (scale 0.15 (at 2.9 (ring 7.1 (hz-to-step 1) 1.2)))
        (scale 0.175 (at 4.9 (ring 5.1 (hz-to-step 2) 1.414)))
        (scale 0.2 (at 6.9 (ring 3.1 (hz-to-step 4) 1.8)))
        (scale 0.7 (at 9.9 (suck 3.5)))
        (scale 0.28 (at 9.9 (blurp 3.1)))
        (scale 0.7 (at 12.4 (stretch 0.5 (suckpat 17))))
        (scale 0.4 (at 13.8 (seqrep (i 2) (seq (techno 2) (transpose 5 (techno 2)) (transpose -2 (techno 1)) (transpose 3 (techno 1)) (techno 2)))))
        (scale 0.2 (at 13.9 (seqrep (i 2) (seq (transpose 2 (techno 2)) (transpose 7 (techno 2)) (transpose -4 (techno 1)) (transpose 5 (techno 1)) (transpose -2 (techno 2))))))
        (scale 0.5 (at 15.75 (seqrep (i 4) (vocrap)) ))
        (scale 0.35 (at 21.5 (ring 4 1 1)))
        (scale 0.325 (at 24 (ring 4 4 2)))
        (scale 0.3 (at 27.5 (ring 4 10 4)))
        (scale 0.85 (at 23 (seqrep (i 17) (lp (scale (+ (* i 0.05 ) 0.3) (seq (transpose -4 (ring 0.1 32 0.6)) (transpose -5 (ring 0.05 20 0.2)) (transpose (* 2 i) (ring 0.1 27 0.5)) (transpose -3 (ring 0.05 22 0.1)) (transpose (* i 3) (ring 0.1 28 0.4)) (ring 0.05 31 0.7))) (* 100 i)))))
        (scale 0.75 (at 23.025 (seqrep (i 17) (scale (+ (* i 0.05 ) 0.3) (seq (ring 0.1 32 1.2) (transpose -10 (ring 0.05 20 0.4)) (transpose (* 0.66 i) (ring 0.1 27 1)) (transpose -13 (ring 0.05 22 0.2)) (transpose (* i 1.5) (ring 0.1 28 0.7)) (transpose -2 (ring 0.05 31 0.9)))))))
        (scale 1.0 (at 20.0 (ringpat1)))
        (scale 0.7 (at 20.05 (stretch 1.5 (ringpat1))))
))

(defun segfault ()
  (seqrep (i 17) (scale (+ (* i 0.05 ) 0.3) (seq (ring 0.1 32 1.2) (transpose -10 (ring 0.05 20 0.4)) (transpose (* 0.66 i) (ring 0.1 27 1)) (transpose -13 (ring 0.05 22 0.2)) (transpose (* i 1.5) (ring 0.1 28 0.7)) (transpose -2 (ring 0.05 31 0.9))))))

; the following does clicks
(defun sect1() (sim
        (scale 3 (at 0.0 (simrep(i 4) (at (* i 2.9) (wind)))))
        (scale 0.5 (at 2 (warble 8 48)))
        (scale 0.3 (at 2.05 (warble 8.05 47.9)))
        (scale 0.15 (at 2.9 (ring 7.1 (hz-to-step 1) 1.2)))
        (scale 0.175 (at 4.9 (ring 5.1 (hz-to-step 2) 1.414)))
        (scale 0.2 (at 6.9 (ring 3.1 (hz-to-step 4) 1.8)))
        (scale 0.7 (at 9.9 (suck 3.5)))
        (scale 0.28 (at 9.9 (blurp 3.1)))
        (scale 0.7 (at 12.4 (stretch 0.5 (suckpat 17))))
        (scale 0.4 (at 13.8 (seqrep (i 2) (seq (techno 2) (transpose 5 (techno 2)) (transpose -2 (techno 1)) (transpose 3 (techno 1)) (techno 2)))))
        (scale 0.2 (at 13.9 (seqrep (i 2) (seq (transpose 2 (techno 2)) (transpose 7 (techno 2)) (transpose -4 (techno 1)) (transpose 5 (techno 1)) (transpose -2 (techno 2))))))
        (scale 0.5 (at 15.75 (seqrep (i 4) (vocrap)) ))
        (scale 0.35 (at 21.5 (ring 4 1 1)))
        (scale 0.325 (at 24 (ring 4 4 2)))
        (scale 0.3 (at 27.5 (ring 4 10 4)))
        (scale 0.85 (at 23 (seqrep (i 17) (lp (scale (+ (* i 0.05 ) 0.3) (seq (transpose -4 (ring 0.1 32 0.6)) (transpose -5 (ring 0.05 20 0.2)) (transpose (* 2 i) (ring 0.1 27 0.5)) (transpose -3 (ring 0.05 22 0.1)) (transpose (* i 3) (ring 0.1 28 0.4)) (ring 0.05 31 0.7))) (* 100 i)))))
        (scale 0.75 (at 23.025 (seqrep (i 17) (scale (+ (* i 0.05 ) 0.3) (seq (ring 0.1 32 1.2) (transpose -10 (ring 0.05 20 0.4)) (transpose (* 0.66 i) (ring 0.1 27 1)) (transpose -13 (ring 0.05 22 0.2)) (transpose (* i 1.5) (ring 0.1 28 0.7)) (transpose -2 (ring 0.05 31 0.9)))))))
        (scale 1.0 (at 20.0 (ringpat1)))
        (scale 0.7 (at 20.05 (stretch 1.5 (ringpat1))))
        ))

