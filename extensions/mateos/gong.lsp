;; GONG INSTRUMENT (TAM TAM)
;; Daniel Mateos - danielma@andrew.cmu.edu
;; Instrument by Hans Mikelson, imported from CSOUND
;; Website: http://www.adp-gmbh.ch/csound/instruments/
;; Modified by Roger Dannenberg, Nov. 2005
;; See also: demos/pmorales/b1.lsp


;; DMHM-GONG -- an additive synthesis gong sound
;;
;; so named to avoid naming conflicts with other gongs
;;
(defun dmhm-gong (pitch)
  (let* ((ifrq (step-to-hz pitch))
         ;; frequencies of partials
         (ifrq1 (* 1.0000 ifrq))
	 (ifrq2 (* 1.1541 ifrq))
	 (ifrq3 (* 1.6041 ifrq))
	 (ifrq4 (* 1.5208 ifrq))
	 (ifrq5 (* 1.4166 ifrq))
	 (ifrq6 (* 2.7916 ifrq))
	 (ifrq7 (* 3.3833 ifrq))

	 ;; amplitude of partials
	 (iamp1 1.0000)
	 (iamp2 0.8333)
	 (iamp3 0.6667)
	 (iamp4 1.0000)
	 (iamp5 0.3333)
	 (iamp6 0.3333)
	 (iamp7 0.3333)

	 ;; main envelope
	 (envelope (pwevr 1 1 0.001))

	 ;; partial envelopes
	 (aenv1 (mult iamp1 envelope))
	 (aenv2 (mult iamp2 envelope))
	 (aenv3 (mult iamp3 envelope))
	 (aenv4 (mult iamp4 envelope))
	 (aenv5 (mult iamp5 envelope))
	 (aenv6 (mult iamp6 envelope))
	 (aenv7 (mult iamp7 envelope)))

    ;; sum the partials
    (scale 0.25  ; normalize to about 1.0
      (sim
       (partial (hz-to-step ifrq1) aenv1)
       (partial (hz-to-step ifrq2) aenv2)
       (partial (hz-to-step ifrq3) aenv3)
       (partial (hz-to-step ifrq4) aenv4)
       (partial (hz-to-step ifrq5) aenv5)
       (partial (hz-to-step ifrq6) aenv6)
       (partial (hz-to-step ifrq7) aenv7)
       ))))

;; Let's play something!
;;
;; type (dmhm-gong-test) to play an example
;;
(defun dmhm-gong-test ()
  (autonorm-off)
  (play (stretch 10 (dmhm-gong a3))))

