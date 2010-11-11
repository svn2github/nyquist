;; these are files in /tran that implement nyquist primitives
;; they become arguments to intgen, which builds the stubs from lisp to C
;; 

;*******************************************************************
; CHANGED. PJM July 2007
; Added new ALG files related to new STK instruments
;*******************************************************************

(setf transfiles '("abs" "allpoles" "alpass" "alpasscv" "alpassvv" "amosc"
                   "areson" "aresonvc" "aresoncv" "aresonvv"
                   "atone" "atonev" 
                   "biquadfilt" "buzz"
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
		   "stkrev" "stkpitshift" "stkchorus"
		   "instrbow" "instrbowedfreq" 
                   "instrbanded" "instrmandolin"
                   "instrsitar" "instrmodalbar"                   
                   "instrflute" "instrflutefreq" "instrfluteall"
                   "fmfb" "fmfbv"
                   ))

; deleted comb

;******************************************************************
; CHANGES. PJM July 2007
; nyqsrcfiles: DELETED "sndsliders" -> January 2008: now ADDED
;                                      and fixed in makefile.lsp               
;              RENAMED "sndwrite" -> "sndwritepa"
;              ADDED   "lpanal"
;******************************************************************


;; these are files in /nyqsrc that implement nyquist primitives
;; they become arguments to intgen, which builds the stubs from lisp to C
;; 
(setf nyqsrcfiles '("sound" "add" "avg" "compose" "convolve" "downsample" 
                    "fft" "inverse" "multiseq" "resamp" "resampv" 
                    "samples" "sndmax" "sndread" "sndseq"  "sndsliders"
                    "sndwritepa" "yin" "nyq-osc-server" 
                    "trigger" "lpanal" "phasevocoder" "pvshell"
                   ))

;; these are additional files in /nyqsrc that need to be compiled
;; and linked to make nyquist


;********************************************************************
; CHANGES. PJM July 2007
; nyqfiles: DELETED "lpanal"
;           DELETED "trigger"
;********************************************************************

(setf nyqfiles '("debug" "falloc" "local" 
		 "handlers" "multiread" 
		 "seqext" "seqinterf"  "stats" 
		 "ffilterkit"
                 "sliders" 
))

