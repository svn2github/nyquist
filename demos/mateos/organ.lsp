;; Daniel Mateos - danielma@andrew.cmu.edu
;; Instrument by Hans Mikelson,
;; imported from CSOUND
;; Website: http://www.adp-gmbh.ch/csound/instruments/organ01.html
;; This instrument really sounds as a real organ!!
;; Modified by Roger Dannenberg, Nov. 2005


;; Sound overtones
(defun dmhm-overtones (freq)
 (sim 
  (scale 0.8 (hzosc freq))
  (scale 0.8 (hzosc (* freq 2)))
  (scale 0.8 (hzosc (* freq 2.9966)))
  (scale 0.8 (hzosc (* freq 4)))
  (scale 0.3 (hzosc (* freq 5.9932)))
  (scale 0.2 (hzosc (* freq 8)))
  (scale 0.1 (hzosc (* freq 10.0794)))
  (scale 0.1 (hzosc (* freq 11.9864)))
  (scale 0.4 (hzosc (* freq 16)))))


(defun dmhm-organ (pitch)
 (mult 0.1 ;; normalize to about 1
       (env 0.1 0 0.1 1 1 1)
       (dmhm-overtones (step-to-hz pitch))))


;; DMHM-ORGAN-TEST -- a small test program/demo
;;
;; The same score used by the CSOUND example.
;;
(defun dmhm-organ-test ()
  (autonorm-off)
  (play
   (sim
    (at 0 
     (stretch 1.98 
      (sim
        (dmhm-organ c3)
	(dmhm-organ e3)
	(dmhm-organ g3)
	(dmhm-organ as3))))

    (at 2 
     (stretch 1.98
      (sim
	(dmhm-organ c3)
	(dmhm-organ ds3)
	(dmhm-organ f3)
	(dmhm-organ a3))))

    (at 4 (stretch 1.98 (dmhm-organ c3)))
    (at 4 (stretch 0.1 (dmhm-organ ds3)))
    (at 4.1 (stretch 1.88 (dmhm-organ e3)))
    (at 4 (stretch 1.98 (dmhm-organ g3))))))


