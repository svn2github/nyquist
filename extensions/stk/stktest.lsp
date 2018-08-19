;; stktest.lsp -- test the STK instruments
;;
;; Nyquist Extension
;; Version: 1.0;
;;
;; Author Name: Roger B. Dannenberg;
;; Author Email: rbd@cs.cmu.edu;
;; 
;; End Metadata

;; Usage: 
;;    load "stk/stktest.sal" -- load and run this demo
;;    exec all-stk-demos()
;;
;; Description:
;;
;; This extension exercises a number of synthesis functions
;; imported from STK (Synthesis Tool Kit).
;;
;; While written in LISP, I have added comments to suggest
;; how to get all these sounds using SAL.


(autonorm-on)

(print "Type (makedemo number) with number ranging from 1 to 17")
(print "Type (all-stk-demos) to hear them all")

(defun makedemo (n)
  (case n
    (1 (progn (nrev-demo)(print "NRev Demo")))
    (2 (progn (jcrev-demo)(print "JCRev Demo")))
    (3 (progn (prcrev-demo)(print "PRCRev Demo")))
    (4 (progn (stkchorus-demo)(print "Chorus Demo")))
    (5 (progn (pitshift-demo) (print "Pitch Shift Demo")))
    (6 (progn (flute-demo)(print "Flute Demo")))
    (7 (progn (flute-freq-demo)(print "Flute Freq Demo")))
    (8 (progn (flute-all-demo)(print "Flute All Demo")))
    (9 (progn (bowed-demo 0.4)(print "Bowed Instrument Demo")))
    (10 (progn (bowed-freq-demo)(print "Bowed Freq Instrument Demo")))
    (11 (progn (mandolin-demo)(print "Mandolin Demo")))
    (12 (progn (wg-uniform-bar-demo) (print "Uniform Bar Wave Guide Demo")))
    (13 (progn (wg-tuned-bar-demo)(print "Tuned Bar Wave Guide Demo")))
    (14 (progn (wg-glass-harm-demo)(print "Glass Harmonica Wave Guide Demo")))
    (15 (progn (wg-tibetan-bowl-demo)(print "Tibetan Bowl Prayer Wave Guide Demo")))
    (16 (progn (modalbar-demo)(print "Modal Bar Demo")))
    (17 (progn (sitar-demo) (print "Sitar Demo")))
    (18 (progn (clarinet-example-1) (print "Clarinet Demo 1")))
    (19 (progn (clarinet-example-2) (print "Clarinet Demo 2")))
    (20 (progn (clarinet-example-3) (print "Clarinet Demo 3")))
    (21 (progn (clarinet-example-4) (print "Clarinet Demo 4")))
    (22 (progn (clarinet-example-5) (print "Clarinet Demo 5")))
    (23 (progn (clarinet-example-6) (print "Clarinet Example 6")))
    (24 (progn (sax-example-1) (print "Sax Demo 1")))
    (25 (progn (sax-example-2) (print "Sax Demo 2")))
    (t (error "number ranges from 1 to 25"))))
    

(defun all-stk-demos ()
  (dotimes (i 25) (makedemo (1+ i))))



;;; ********************************
;;;
;;;   EFFECTS DEMOS
;;;
;;; ********************************


; design of a tubular-bell-like sound

(setf *pi-over-2* (/ 3.141592 2))

(defun pan-fun-l (x)
  (* (/ (sqrt 2.0)) (- (cos (* x *pi-over-2*)) (sin (* x *pi-over-2*)))))

(defun pan-fun-r (x)
  (* (/ (sqrt 2.0)) (+ (cos (* x *pi-over-2*)) (sin (* x *pi-over-2*)))))

(defun pan-snd (snd pan)
  (vector (scale (pan-fun-l pan) snd)
	  (scale (pan-fun-r pan) snd)))


(defun tub-partial (scale-factor freq-factor time-factor pitch dur)
  (scale scale-factor (mult (sim -1.0 (pwev 10 (* dur time-factor) 1))
			    (osc (hz-to-step (* freq-factor (step-to-hz pitch)))
				 (* dur time-factor)))))

(defun tubular (pitch dur)
  (let ((hz (step-to-hz pitch)))
    (sim (tub-partial 0.1 1.0 1.0 pitch dur)
	 (tub-partial 0.06 2.76 0.8 pitch dur)
	 (tub-partial 0.04 5.4  0.7 pitch dur)
	 (tub-partial 0.03 8.93 0.4 pitch dur)
	 (tub-partial 0.02 13.34 0.2 pitch dur)
	 (tub-partial 0.01 18.64 0.1 pitch dur)
	 (tub-partial 0.005 31.87 0.05 pitch dur))))

(defun ktubular (&key pitch idur pan dyn)
  (pan-snd (scale dyn (tubular pitch idur)) pan))

;   (if rev (nrev snd 4 0.25)
;     snd)))

; algorithmic score by means of Xmusic

(setf pitches1 (make-cycle (list c5 g4 e4 c4) :for 8))
(setf pitches2 (make-cycle (list c5 a4 f4 d4) :for 8))
(setf *pitches* (make-cycle (list pitches1 pitches2) :for 6))

(setf *ioi* (make-cycle (list 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.6)))
(setf *pan* (make-cycle (list 0 -0.2 0.2 -0.4 0.4 -0.45 0.45 -0.1 0.1 -0.25 0.25 -0.3 0.3))) 
(setf *dyn* (make-cycle (list 1 1 1 1 1 1 1 1.2 2 2 1 2 1 2 1 2 )))

(setf my-score (score-gen :name 'ktubular :dur 1 :idur 2
			  :ioi 0.2
			  :pitch (next *pitches*)
			  :dyn (next *dyn*)
			  :pan (next *pan*)
			  :score-len 16))


;; nrev-demo - example of NREV reverb
;;
;; In SAL, call nrev(sound, 4.0, 0.25)
;;
(defun nrev-demo ()
  (autonorm-off)
  (play (scale 0.25
         (seq (timed-seq my-score)
	      (nrev (timed-seq my-score) 4.0 0.25))))
  (autonorm-on))

;(nrev-demo)


;; jcrev-demo - example of JCREV reverb
;;
;; In SAL, call jcrev(sound, 4.0, 0.25)
;;
(defun jcrev-demo ()
  (autonorm-off)
  (play (scale 0.2
         (seq (timed-seq my-score)
	      (jcrev (timed-seq my-score) 4.0 0.25))))
  (autonorm-on))

;(jcrev-demo)



;; prcrev-demo - example of PRCREV reverb
;;
;; In SAL, call prcrev(sound, 4.0, 0.25)
;;
(defun prcrev-demo ()
  (play (scale 0.25
         (seq (timed-seq my-score)
	      (prcrev (timed-seq my-score) 4.0 0.25)))))

;(prcrev-demo)


;; stkchorus-demo - example of chorus effect
;;
;; In SAL, call stkchorus(sound, 0.1, 0.2, 0.5)
;;
(defun stkchorus-demo ()
  (play (scale 0.25
         (seq (timed-seq my-score)
	      (stkchorus (timed-seq my-score) 0.1 0.2 0.5)))))

;(stkchorus-demo)


;; pitshift-demo - example of pitch shift effect
;;
;; In SAL, call pitshift(sound, ratio, mix), where
;; mix=0 means only the input sound, mix=1 means only shifted sound
;;
(defun pitshift-demo ()
  (play (scale 0.25
         (seq (timed-seq my-score)
              (pitshift (timed-seq my-score) 2.0 0.9)
              (pitshift (timed-seq my-score) 0.4 0.7)
	      (pitshift (timed-seq my-score) 6.0 0.7)))))

;(pitshift-demo)


; *******************************************
;
;  INSTRUMENTS DEMOS
;
; *******************************************


(defun env1 (d)
 (pwl 0.1 0.7 (- d 0.3) 0.5  d))

;; flute-demo - play flute sounds
;;
;; In SAL, call flute(pitch, env(0.1, 0, 0.3, 0.7, 0.7, 0.5, dur))
;;
(defun flute-demo ()
  (play
   (seq (flute e5 (env1 1))
        (flute c5 (env1 1))
	(flute g4 (env1 2)))))

;(flute-demo)


;; flute-freq-demo - play flute with modulation
;;
;; In SAL, call flute-freq(c5, env(0.1, 0, 0.3, 0.7, 0.7, 0.5, dur),
;;                          pwlv(0, 1, 0, dur, 1) * lfo(5, dur))
;; [in this example, dur = 6]
;;
(defun env2 (ampl d)
  (scale ampl (mult (pwlv 0.0 1.0 0.0 d 1.0) (lfo 5 d))))
(defun flute-freq-demo ()
  (play (flute-freq c5 (env1 6) (env2 8 6))))

;(flute-freq-demo)


;; flute-all-demo - play flute with more modulation
;;
;; In SAL, call flute-all(c5, env(0.1, 0, 0.3, 0.7, 0.7, 0.5, dur),
;;                         pwl(dur * 0.5, 20, dur),
;;                         0.2, 0.5, 0.06)
;; [in this example, dur = 6]
;;
(defun env3 (d)
  (pwl (* d 0.5) 20 d))
(defun flute-all-demo ()
  (play (flute-all c5 (env1 6) (env3 6) 6 0.2 0.5 0.06)))

;(flute-all-demo)
 

;; bowed-demo - play bowed string model
;;
;; In SAL, call bowed(g4, env(0.05, 0, 0.3, 0.5, 0.5, 0.3, 0.4))
;;
(defun bow-env (d)
  (pwl 0.05 0.5 (- d 0.1) 0.3 d))
  
(defun bowed-demo (d)
  (play (seq (bowed g4 (bow-env d))
	     (bowed d5 (bow-env d))
	     (bowed b5 (bow-env d))
	     (bowed a5 (bow-env d))
	     (bowed b5 (bow-env d))
	     (bowed d5 (bow-env d))
	     (bowed b5 (bow-env d))
	     (bowed d5 (bow-env d))
	     (bowed g4 (bow-env d)))))

;(bowed-demo 0.4)
   

;; bowed-freq-demo - play string model with frequency modulation
;;
;; In SAL, call bowed-freq(c3,  env(0.05, 0, 0.3, 0.5, 0.5, 0.3, dur), pwlv(0, 1, 0, dur, 1) * lfo(5, dur))
;; [try dur = 5]
;;
(defun bowed-freq-demo ()
  (autonorm-off)
  (play (bowed-freq c3 (bow-env 10) (env2 5 10)))
  (autonorm-on))

;(bowed-freq-demo)
 

;; mandolin-demo - play mandolin model
;;
;; In SAL, call mandolin(c4, 1.0)
;;
(defun mandolin-demo ()
  (autonorm-off)
  (play (scale 0.4 
         (seq 
	     (mandolin c4 1.0)
             (mandolin c4 1.0 2.0)
	     (mandolin c4 1.0 3.0)
             (mandolin c4 1.0 4.0)
             (mandolin c4 1.0 5.0)
	     (mandolin c4 1.0 6.0)
	     (mandolin c4 1.0 7.0)
	     (mandolin c4 1.0 8.0)
	     (mandolin c4 1.0 9.0)
	     (mandolin c4 1.0 10.0)
	     (mandolin c4 1.0 11.0)
	     (mandolin c4 1.0 12.0)
	     (mandolin c4 1.0 13.0)
	     (mandolin c4 1.0 14.0)
	     (mandolin c4 1.0 15.0)
	     (mandolin c4 1.0 16.0)
	     (mandolin c4 1.0 17.0)
	     (mandolin c4 1.0 18.0)
	     (mandolin c4 1.0 19.0)
	     (mandolin c4 1.0 20.0)
	     (mandolin c4 1.0 25.0)
	     (mandolin c4 1.0 30.0)
	     (mandolin c4 1.0 35.0)
	     (mandolin c4 1.0 40.0)
	     (mandolin c4 1.0 45.0)
	     (mandolin c4 1.0 50.0)
	     (mandolin c4 1.0 55.0)
	     (mandolin c4 1.0 60.0)
	     (mandolin c4 1.0 65.0))))
  (autonorm-on))

;(mandolin-demo)


;; wg-uniform-bar-demo - banded waveguide demo
;;
;; In SAL, call wg-uniform-bar(c4, const(1))
;;
(defun wg-env (d)
  (pwlv 1 d 1))

(defun wg-uniform-bar-demo ()
  (play (seq (wg-uniform-bar c4 (wg-env 0.2))
	     (wg-uniform-bar g3 (wg-env 0.2))
	     (wg-uniform-bar c4 (wg-env 0.2))
	     (wg-uniform-bar e4 (wg-env 0.2))
	     (wg-uniform-bar g4 (wg-env 2.2)))))

;(wg-uniform-bar-demo)

;; wg-tuned-bar-demo - tuned bar demo
;;
;; In SAL, call wg-tuned-bar(c4, const(1))
;;
(defun wg-tuned-bar-demo ()
  (autonorm-off)
  (play (seq (wg-tuned-bar c4 (wg-env 0.2))
	     (wg-tuned-bar g3 (wg-env 0.2))
	     (wg-tuned-bar c4 (wg-env 0.2))
	     (wg-tuned-bar e4 (wg-env 0.2))
	     (wg-tuned-bar g4 (wg-env 0.2))))
  (autonorm-on))

;(wg-tuned-bar-demo)


;; wg-glass-harm-demo - glass harmonica demo
;;
;; In SAL, call wg-glass-harm(c4, const(1))
;;
(defun wg-glass-harm-demo ()
  (play (seq (wg-glass-harm c4 (wg-env 0.2))
	     (wg-glass-harm g3 (wg-env 0.2))
	     (wg-glass-harm c4 (wg-env 0.2))
	     (wg-glass-harm e4 (wg-env 0.2))
	     (wg-glass-harm g4 (wg-env 1.2)))))

;(wg-glass-harm-demo)


;; wg-tibetan-bowl-demo - tibetan bowl instrument demo
;;
;; In SAL, call wg-tibetan-bowl(c4, const(2))
;;
(defun wg-tibetan-bowl-demo ()
  (autonorm-off)
  (play 
     (scale 0.5
        (seq (wg-tibetan-bowl c4 (wg-env 0.2))
	     (wg-tibetan-bowl ef4 (wg-env 0.2))
	     (wg-tibetan-bowl fs4 (wg-env 0.2))
	     (wg-tibetan-bowl a4 (wg-env 2.0)))))
  (autonorm-on))

;(wg-tibetan-bowl-demo)


;; modalbar-demo-1 -- modalbar instrument demo
;;
;; In SAL, call modalbar(quote(MARIMBA), c4, 1.0)
;; and see modalbar-demo below for other presets
;;
(defun modalbar-demo-1 (prst)
   (seq (modalbar prst c4 0.2)
	(modalbar prst g3 0.2)
	(modalbar prst c4 0.2)
	(modalbar prst e4 0.2)
	(modalbar prst g4 1.0)))

(defun modalbar-demo ()
  (autonorm-off)
  (play (scale 0.05
          (seq 
	     (modalbar-demo-1 'MARIMBA)
	     (modalbar-demo-1 'VIBRAPHONE)
	     (modalbar-demo-1 'AGOGO)
	     (modalbar-demo-1 'WOOD1)
	     (modalbar-demo-1 'RESO)
	     (modalbar-demo-1 'WOOD2)
	     (modalbar-demo-1 'BEATS)
	     (modalbar-demo-1 'TWO-FIXED)
	     (modalbar-demo-1 'CLUMP))))
  (autonorm-on))

;(modalbar-demo)


;; sitar-demo - sitar instrument demo
;;
;; In SAL, call sitar(c3, 2.0)
;;
(defun sitar-demo ()
  (play (seq (sitar c3 0.6)
	     (sitar g3 1.2)
	     (sitar fs3 0.4)
	     (sitar g3 0.4)
	     (sitar af3 0.6)
	     (sitar ef3 2.0))))

;(sitar-demo)


;; clarinet-example-1 - simple clarinet sound
;;
;; In SAL, call clarinet(bf3, stk-breath-env(1, 0.2, 0.1))
;;
(defun clarinet-example-1 ()
  (autonorm-off)
  (play (clarinet bf3 (stk-breath-env 1 0.2 0.1)))
  (autonorm-on))

;; clarinet-example-2 - clarinet sound with frequency sweep (glissando)
;;
;; In SAL, call clarinet-freq(bf3, stk-breath-env(3, 0.2, 0.1), pwl(1.5, 80, 3, 80, 3))
;;
(defun clarinet-example-2 ()
  (play (clarinet-freq bf3 (stk-breath-env 3 0.2 0.1) (pwl 1.5 80 3 80 3))))


;; clarinet-example-3 - clarinet sound with change in breath pressure
;;
;; In SAL, call clarinet(bf3, pwl(0, 1, 1.5, 0.9, 3, 1, 3) * stk-breath-env(3, 0.2, 0.1))
;;
(defun clarinet-example-3 ()
  (play (clarinet bf3 (prod (pwl 0 1 1.5 0.9 3 1 3) (stk-breath-env 3 0.2 0.1)))))

;; clarinet-example-4 - clarinet sound using initial frequency sweep and built-in vibrato effect
;;
;; In SAL, call clarinet-all(bf3, stk-breath-env(3, 0.5, 0.05),
;;                           pwl(0.3, 80, 3, 80, 3), 5.7, 0.5, 0, 0)
(defun clarinet-example-4 ()
  (play (clarinet-all bf3 (stk-breath-env 3 0.5 0.05) (pwl 0.3 80 3 80 3) 5.7 0.5 0 0)))

;; clarinet-example-5 - clarinet sound with increasing then decreasing reed stiffness
;;
;; In SAL, call clarinet-all(bf3, stk-breath-env(3, 0.5, 0.05),
;;                           0, 0, 0, pwl(1.5, 0.75, 3), 0)
;;
(defun clarinet-example-5 ()
  (play (clarinet-all bf3 (stk-breath-env 3 0.5 0.05) 0 0 0 (pwl 1.5 0.75 3) 0)))

;; clarinet-example-6 - clarinet sound with increasing noise, with vibrato
;;
;; In SAL, call clarinet-all(bf3, stk-breath-env(3, 0.5, 0.05),
;;                           0, 5.7, 0.5, 0, pwl(3, 1, 3))
(defun clarinet-example-6 ()
  (play (clarinet-all bf3 (stk-breath-env 3 0.5 0.05) 0 5.7 0.5 0 (pwl 3 1 3))))

;(print "clarinet-example-1")
;(clarinet-example-1)
;(print "clarinet-example-2")
;(clarinet-example-2)
;(print "clarinet-example-3")
;(clarinet-example-3)
;(print "clarinet-example-4")
;(clarinet-example-4)
;(print "clarinet-example-5")
;(clarinet-example-5)
;(print "clarinet-example-6")
;(clarinet-example-6)


;; sax-example-1 - STK sax example
;;
;; In SAL, call sax(g3, stk-breath-env(2, 0.2, 0.2))
;; and sax-freq(c4, stk-breath-env(4, 0.6, 0.6),
;;              100 * pwlv(0.95, 4, 1.3))
;;
(defun sax-example-1 ()
  (autonorm-off)
  (play (scale 0.5
   (timed-seq '(
		(0.0 1 (sax g3 (stk-breath-env 2 0.2 0.2)))
		(2.0 1 (sax-freq c4  (stk-breath-env 4 0.6 0.6) 
                                 (scale 100 (mult (pwl 0 0.95 4 1.3 4)))))
               ))))
  (autonorm-on)
)

;; sax-example-2 - showing frequency changes to make a melody
;;
;; In SAL, call sax-freq(bf3, eight-sixteenths-env(),
;;                       freqenv(1, bf3, list(0, bf3, 0.125, af4, 0.25, g4, 
;;                                            0.375, d4, 0.5, f4, 0.625, ef4,
;;                                            0.75, d4, 0.875, ef4)))
;; [this relies on some helper function eight-sixteenths-env() and freqenv()
;;  which are still in LISP, but described below]
;; 
(defun sax-example-2 ()
  (autonorm-off)
  (play (scale 0.5
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
  (autonorm-on)
)

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

;(print "sax-example-1")
;(play (sax-example-1))
;(print "sax-example-2")
;(play (sax-example-2))

