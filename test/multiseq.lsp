
(defun mst ()
  (setf xxx (seq (vector (osc c4) (osc g4)) (vector (osc c3) (osc e4))))
  (play xxx))

(defun mst1 ()
  (setf xxx (seq (vector (const 1) (const 2)) (vector (const 3) (const 4))))
  (play xxx))

(defun mst2 ()
  (setf xxx (seq (vector (ramp) (ramp)) (vector (ramp) (ramp))))
  (play xxx))

(defun mst3 ()
  (setf xxx (seq (vector (ramp) (at 0.5 (const 1)))
                 (vector (ramp) (at 0.8 (const 2))))))

(defun mst4 ()
  (setf xxx (seq (vector (ramp)             (ramp))
                 (vector (at 0.5 (const 1)) (at 0.8 (const 2)))
                 (vector (ramp)             (ramp)) )))

(defun sh () (dotimes (i 2) (print (snd-samples (aref xxx i) 100))))

(defun lsr ()
  (set-sound-srate 10)
  (set-control-srate 10))

(defun hsr ()
  (set-sound-srate 22050)
  (set-control-srate 2205))

;crash: (load "test/rbd") (lsr) (mst1)

(defun msr1 ()
  (seqrep (i 3)
          (vector (osc (+ c4 i)) (osc (+ e4 i)))))

(defun ster (sound pan)
        (vector (mult sound pan)
                (mult sound (sum 1 (mult -1 pan)))))

(defun ster (sound pan)
        (vector (snd-normalize (mult sound pan))
                (snd-normalize (mult sound (sum 1 (mult -1 pan))))))

(defun msr2 ()
  (seqrep (i 10) (ster (osc 46 0.2) (* i 0.1))))

; The next 4 lines crash nyquist if sound_types are allocated from the
; freelist.  They just don't work if sound_types are always allocated
; from the pool.  I guess something is getting on the freelist early.
; bad ref count?
(lsr)
(play (msr2))
(play (msr2))
;(play (msr2))
;(play (msr2))
;(play (msr2))


;(play (stretch 0.2 (ster (osc c4) 0.1)))
'(
(hsr)
(play (stretch 0.2 (seq (ster (osc c4) 0.9) (ster (osc c4) 0.1))))

(lsr)
(setf xxx (stretch 0.2 (ster (const 1) 0.9)))
(setf xxx (stretch 0.2 (seq (ster (const 1) 0.9) (ster (const 2) 0.1))))
(setf xxx (seq (ster (const 1) 0.9) (ster (const 2) 0.1)))
(sx)
)
(defun sx ()
  (list (snd-samples (aref xxx 0) 100) (snd-samples (aref xxx 1) 100)))

;(lsr)

;(msr1)

;(mst1)
;(sh)
;(mst4)
;(sh)
;(snd-print  (seq (vector (ramp)             (ramp))
;		 (vector (at 0.5 (const 1)) (at 0.8 (const 2)))
;		 (vector (ramp)             (ramp)) )
;            100)

