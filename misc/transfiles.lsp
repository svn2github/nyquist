;; these are files in /tran that implement nyquist primitives
;; they become arguments to intgen, which builds the stubs from lisp to C
;; 
(setf transfiles '("abs" "allpoles" "alpass" "alpasscv" "alpassvv" "amosc"
                   "areson" "aresonvc" "aresoncv" "aresonvv"
                   "atone" "atonev" 
                   "biquad" "buzz"
                   "chase" "clip" "congen" "const" "coterm" 
                   "delaycc" "delaycv" 
                   "eqbandvvv" "exp"
                   "follow" "fmosc" "fromobject" "fromarraystream"
                   "gate" "ifft" 
                   "instrclar" "instrclarall" "instrclarfreq"
                   "instrsax" "instrsaxall" "instrsaxfreq"
                   "integrate" "log" "lpreson"
                   "maxv" "offset" "oneshot" "osc"
                   "partial" "pluck" "prod" "pwl" "quantize" 
                   "recip" 
                   "reson" "resonvc" "resoncv"  "resonvv"
                   "sampler" "scale" "shape" "sine" "siosc" "slope" "sqrt"
                   "tapf" "tapv" "tone" "tonev" "upsample"  "white"                   
                   ))

; deleted comb

;; these are files in /nyqsrc that implement nyquist primitives
;; they become arguments to intgen, which builds the stubs from lisp to C
;; 
(setf nyqsrcfiles '("sound" "add" "avg" "compose" "convolve" "downsample" 
                    "fft" "inverse" "multiseq" "resamp" "resampv" 
                    "samples" "sndmax" "sndread" "sndseq" 
                    "sndwritepa" "yin"
                   ))
