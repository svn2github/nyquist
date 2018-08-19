;; dtmf.lsp -- DTMF encoding functions
;;
;; Nyquist Extension
;; Version: 1.0;
;;
;; Author Name: Rob Rost and Roger B. Dannenberg;
;; Author Email: rbd@cs.cmu.edu;
;;
;; Additional File: autoload.lsp
;; Additional File: nyquistwords.txt
;;
;; End Metadata

;; Usage:
;;    dtmf-tone(key, len, space) - make DTMF tone for key where len is
;;        the length of the tone and space is the duration of silence
;;        after the tone. key is one of 0, 1, ...., 9, 'pound, or 'star
;;    speed-dial(thelist) - make a sequence of DTMF tones. thelist is
;;        a list of key values (0, 1, ..., 9, 'pound, or 'star).
;;        Example: play speed-dial({4 1 2 5 5 5 1 2 1 2})
;;
;; Description:
;;
;; This library takes a list of DTMF (touch-tone) digits and 
;; synthesizes the correct audio. Example:
;;   (speed-dial '(1 2 3 pound 5 6 star 7))
;; Note how pound and star keys are entered.
;; For an example, try:
;;     load "dtmf/dtmf"
;;     exec dtmf-example()


(setf dtmf-freqs
  '((star 941 1209) (0 941 1336) (pound 941 1477)
    (1 697 1209)    (2 697 1336) (3 697 1477)
    (4 770 1209)    (5 770 1336) (6 770 1477)
    (7 852 1209)    (8 852 1336) (9 852 1477)))

(defun dtmf-freq1 (key)
  (cadr (assoc key dtmf-freqs)))

(defun dtmf-freq2 (key)
  (caddr (assoc key dtmf-freqs)))

(defun dtmf-tone (key len space)
  (scale 0.5 
    (seq
      (stretch len
        (sim (hzosc (dtmf-freq1 key))
             (hzosc (dtmf-freq2 key))))
      (s-rest space))))


; send it a list of digits and it returns the
; Sound object to dial that number
(defun speed-dial (thelist)
  (cond ((null thelist) (s-rest 0))
	(t 
         (seq (dtmf-tone (car thelist) 0.2 0.1)
	      (speed-dial (cdr thelist))))))


(defun dtmf-example ()
  (play (speed-dial (list 4 1 2 5 5 5 1 2 1 2))))




