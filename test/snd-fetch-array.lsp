;; snd-fetch-array.lsp -- a test program to explore a new feature

(print "loading snd-fetch-array.lsp")

(load "/Users/rbd/nyquist/sys/unix/osx/system.lsp")

(autonorm-off)

;; make short sound
(setf s (osc c4 0.001)) ; about 44 samples

(dotimes (i 100) ; limited iterations in case of problems
  (setf samps (snd-fetch-array s 10 10))
  (display "after snd-fetch-array" i samps *rslt*)
  (if (null samps) (return 'done)))




