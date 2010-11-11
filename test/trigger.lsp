'(print
  (control-srate-abs 20
    (snd-samples 
      (snd-trigger (lfo 2) '(lambda (t0) (at-abs t0 (const 0.5 0.2))))
      100)))

(print
  (control-srate-abs 20
    (snd-samples 
      (trigger (lfo 2) (const 0.5 0.2))
      100)))


