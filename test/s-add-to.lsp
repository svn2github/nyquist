;; s-add-to.lsp -- a test program to explore a bug report

(print "loading s-add-to.lsp")

(load "/Users/rbd/nyquist/sys/unix/osx/system.lsp")

; make a file to add to 
(s-save (mult 0.1 (vector (osc c4) (osc g4))) ny:all "deleteme.wav")

; play it to make sure
(play-file "deleteme.wav")


; add to it
(s-add-to (mult 0.1 (vector (osc e4) (osc b4))) ny:all "deleteme.wav")

; play the result
(play-file "deleteme.wav")

