;; soften.lsp -- this is code to "soften" harsh clipping
;
; it works by detecting peaks that exceed an absolute amplitude of 126/127
; using SND-ONESHOT. Then the envelope is smoothed using SND-CHASE
; to produce a smooth cross-fade envelope. The envelope picks out the loud
; stuff to be filtered (try 4K) and an inverted envelope grabs the soft 
; stuff which is unmodified except where the loud regions are clipped out.
; The sum of soft and filtered loud components is returned.
;
; Since clipping tends to generate harsh aliasing, the low-pass filter
; eliminates a lot of the problem, and the filter is usually on so
; briefly that you don't notice it.

(defun square (x) (* x x))

;; region for low-pass will be *soften-width* wide, with
;; *soften-crossfade* seconds of cross-fade
(setf *soften-width* 0.02)
(setf *soften-crossfade* 0.002)

(defun soften-clipping (snd cutoff)
  (let (clip-region snd2 loud-stuff soft-stuff filtered-stuff)
    (setf clip-region (snd-oneshot (prod snd snd) 
                                        (square (/ 126.0 127.0)) *soften-width*))
    (setf clip-region (snd-chase clip-region 
                                     *soften-crossfade* *soften-crossfade*))
    ; s-rest needs a sample rate:
    (sound-srate-abs (snd-srate snd)
      (setf snd2 (seq (s-rest (/ *soften-width* 2)) 
                          (cue (scale 0.99 snd))) ))
    (setf loud-stuff (prod snd2 clip-region))
    (setf soft-stuff (prod snd2 (sum 1 (scale -1 clip-region))))
    (setf filtered-stuff (lp loud-stuff cutoff))
    ; (vector filtered-stuff loud-stuff)
    (sum filtered-stuff soft-stuff)
  ))

    
;(defun tes ()
;  (sound-off)
;  (let (snd)
;    (setf snd (s-read "..\\..\\intro.aif"))
;    (s-save (soften-clipping snd 4000) ny:all "temp.wav" :bits 16)))

; (tes)
