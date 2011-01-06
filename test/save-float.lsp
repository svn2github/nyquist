;; save-float.lsp -- a test program to explore a bug report

(print "loading save-float.lsp")

(load "/Users/rbd/nyquist/sys/unix/osx/system.lsp")

(autonorm-off)

; write file
(s-save (mult (osc c4) 10) ny:all "/Users/rbd/tmp/deleteme.rawfloat"
        :mode snd-mode-float :bits 32 :format snd-head-raw :play nil)

; read file
(play (mult 0.1 (s-read "/Users/rbd/tmp/deleteme.rawfloat"
        :mode snd-mode-float :bits 32 :format snd-head-raw :nchans 1)))




