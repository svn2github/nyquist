;; stk.lsp -- STK-based instruments
;;
;; currently clarinet and saxophony are implemented

(defun instr-parameter (parm)
  ;; coerce parameter into a *sound-srate* signal
  (cond ((numberp parm)
         (stretch 30 (control-srate-abs *sound-srate* (const (float parm)))))
        (t
         (force-srate *sound-srate* parm))))


(defun clarinet (step breath-env)
  (snd-clarinet (step-to-hz step) (force-srate *sound-srate* breath-env) *sound-srate*))


(defun clarinet-freq (step breath-env freq-env)
  ;; note that the parameters are in a different order -- I defined 
  ;; clarinet-freq this way so that the first two parameters are always
  ;; step and breath. I didn't redo snd-clarinet-freq.
  (snd-clarinet_freq (step-to-hz step) 
                (instr-parameter breath-env)
                (instr-parameter freq-env)
                *sound-srate*))



(defun clarinet-all (step breath-env freq-env vibrato-freq vibrato-gain reed-stiffness noise)
  ;; note that the parameters are not in the same order as snd-clarinet-all
  (setf breath-env (instr-parameter breath-env))
  (setf freq-env (instr-parameter freq-env))
  (setf reed-stiffness (instr-parameter reed-stiffness))
  (setf noise (instr-parameter noise))
  (snd-clarinet_all (step-to-hz step)
                    breath-env freq-env 
                    ;; STK scales 1.0 to 12Hz. Scale here so vibrato-freq is in Hz
                    (/ vibrato-freq 12.0) vibrato-gain
                    reed-stiffness noise 
                    *sound-srate*))


(defun sax (step breath-env)
  (snd-sax (step-to-hz step) (force-srate *sound-srate* breath-env) *sound-srate*))

(defun sax-freq (step breath-env freq-env)
  (snd-sax_freq (step-to-hz step)
          (instr-parameter breath-env)
          (instr-parameter freq-env)
          *sound-srate*))

(defun sax-all (step breath-env freq-env vibrato-freq vibrato-gain reed-stiffness noise blow-pos reed-table-offset)
  (snd-sax_all (step-to-hz step)
	       (instr-parameter freq-env)
               (instr-parameter breath-env)
               (instr-parameter (/ vibrato-freq 12.0))
               (instr-parameter vibrato-gain)
               (instr-parameter reed-stiffness)
               (instr-parameter noise)
               (instr-parameter blow-pos)
               (instr-parameter reed-table-offset)
               *sound-srate*)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pass in rates of increase/decrease in begin/end... this is like noteOn and noteOff
;
; STK uses setRate but the actual ramp time is also a function of the sample rate.
; I will assume the clarinet was run at 44100Hz and fix things so that the envelope
; is sample-rate independent.
;
; STK seemed to always give a very fast release, so I changed the numbers so that
; note-off values from 0.01 to 1 give an interesting range of articulations.
;
; IMPORTANT: the returned envelope is 0.1s longer than dur. There is 0.1s of silence
; at the end so that the clarinet can "ring" after the driving force is removed.
;
(defun stk-breath-env (dur note-on note-off)
  (let* ((target (+ 0.55 (* 0.3 note-on)))
         (on-time (/ (* target 0.0045) note-on))
         (off-time (/ (* target 0.02) note-off)))
    (display "clarinet-breath-env" target on-time off-time)
    (pwl on-time target
         (- dur off-time) target
         dur 0 (+ dur 0.1))))


