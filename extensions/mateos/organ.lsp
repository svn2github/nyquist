;; organ.lsp -- organ sound synthesizer
;; Modified by Roger Dannenberg, Nov. 2005,
;; Created extension, Aug. 2018
;;
;; Nyquist Extension
;; Version: 1.0;
;;
;; Author Name: Danial Mateos and Roger B. Dannenberg;
;; Author Email: rbd@cs.cmu.edu;
;; 
;; Additional File: bell.lsp;
;; Additional File: gong.lsp;
;; Additional File: organ.lsp;
;; Additional File: tuba.lsp;
;; Additional File: autoload.lsp;
;; Additional File: nyquistwords.txt;
;;
;; End Metadata

;; Usage (synthesis functions autoload, but test functions do not):
;;
;;    dmhm-organ(pitch) ;; (use stretch ~ for duration)
;;    dmhm-bell(pitch)
;;    dmhm-gong(pitch)
;;    dmhm-tuba(pitch)
;;
;;    load "mateos/organ" ;; needed to run dmhm-organ-test
;;    play dmhm-organ-test() 
;;
;;    load "mateos/bell" ;; needed to run dmhm-bell-test
;;    play dmhm-bell-test()
;;
;;    load "mateos/gong" ;; needed to run dmhm-gong-test
;;    play dmhm-gong-test()
;;
;;    load "mateos/tuba" ;; needed to run dmhm-tuba-test
;;    dmhm-tuba-test()

;; Description for dmhm-organ (see other files for info on bell, gong, tuba):
;;
;; Instrument by Hans Mikelson,
;; ported from CSOUND
;; Website: http://www.adp-gmbh.ch/csound/instruments/organ01.html
;;
;; This instrument sounds like a real organ!!

;; Implementation:

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
    (at 4 (stretch 1.98 (dmhm-organ g3)))))


