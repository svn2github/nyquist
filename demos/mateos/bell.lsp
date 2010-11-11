;; BELL INSTRUMENT
;; Daniel Mateos - danielma@andrew.cmu.edu
;; Instrument by Hans Mikelson, imported from CSOUND
;; Website: http://www.adp-gmbh.ch/csound/instruments/
;; Modified by Roger Dannenberg, Nov. 2005
;; see also: demos/pmorales/b3.lsp,
;;           demos/pmorales/e2.lsp, and
;;           demos/pmoraels/partial.lsp
;; This bell is closely related to FM-BELL in e2.lsp 

;; DMHM-BELL -- an FM bell sound
;;
;; so named to avoid naming conflicts with other bells
;;
(defun dmhm-bell (pitch)
        (let ((imax 10) ; max amplitude
              (ifq1 (hz-to-step (* (step-to-hz pitch) 5))) ; partials
              (ifq2 (hz-to-step (* (step-to-hz pitch) 7)))
              (aenv (pwevr 1 1 0.0001)) ; amplitude envelope
              (adyn (mult (* imax ifq2) (pwevr 1 1 0.001)))) ; dynamics envelope
             (mult aenv ; create a carrier modulated signal
                   (fmosc ifq1 (mult adyn (osc ifq2))))))


;; Let's play this bell!!
;; 
;; type (dmhm-bell-test) to play an example
;;
(defun dmhm-bell-test ()
  (play (stretch 10 (dmhm-bell g1))))



