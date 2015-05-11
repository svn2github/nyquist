;; files.lsp
;; Roger Dannenberg
;; Jan 2013

;; This file (renamed from transfiles.lsp) contains lists of files
;; in various categories. This is the "declarative" part of makefile.lsp
;; In other words, makefile.lsp loads these file lists and turns them
;; into Makefiles and other build specifications.



;; transfiles - these are .c and .h files in /tran that implement nyquist
;; primitives they become arguments to intgen, which builds the stubs from
;; lisp to C
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
                   "fmfb" "fmfbv" "stoponzero"
                   ))

;; nyqsrcfiles - these are .c and .h files in /nyqsrc that implement nyquist
;; primitives. They become arguments to intgen, which builds the stubs from
;; lisp to C
(setf nyqsrcfiles '("sound" "add" "avg" "compose" "convolve" "downsample" 
                    "fft" "inverse" "multiseq" "resamp" "resampv" 
                    "samples" "sndmax" "sndread" "sndseq"  "sndsliders"
                    "sliderdata" "sndwritepa" "yin" "nyq-osc-server" 
                    "trigger" "lpanal" "phasevocoder" "pvshell"
                   ))

;; nyqfiles - these are additional files in /nyqsrc that need to be compiled
;; and linked to make nyquist
(setf nyqfiles '("debug" "falloc" "local" 
		 "handlers" "multiread" 
		 "seqext" "seqinterf"  "stats" 
		 "ffilterkit"
                 ))

;; nyqfiles-h - .h files (not sure these are used)
;(setf nyqfiles-h '("localdefs" "localptrs" "seqdecls" "cque" "switches"))


;; nyqfiles-lsp -- .lsp files (seem to be unused)
;(setf nyqfiles-lsp '("init" "nyquist" "seqmidi" "seq" "makefile"
;                     "update" "transfiles" "examples" "nyinit"))


;; xlfiles - .c sources from /xlisp
(setf xlfiles '("extern" "xldmem" 
  "xlbfun" "xlcont" "xldbug" "xleval"
  "xlfio" "xlftab" "xlglob" "xlimage" "xlinit" "xlio" "xlisp"
  "xljump" "xllist" "xlmath" "xlobj" "xlpp" "xlprin" "xlread"
  "xlstr" "xlsubr" "xlsym" "xlsys" "path" "security"))

;; xlfiles-h - .h files from /xlisp, note that xlisp does not
;; generally have a .h file for each .c file, hence the separate
;; list
(setf xlfiles-h '("osdefs" "osptrs" "xldmem" "xlisp" "extern"))

;; xlfiles-lsp - .lsp files from /runtime
(setf xlfiles-lsp '("xlinit" "misc" "evalenv" "printrec"))

;; stksrcfiles - .c and .h files from /nyqstk/src/
(setf stksrcfiles '("Generator" "SineWave" "Function" "FileRead" "FileWvIn" "Effect"
 "Clarinet" "Delay" "DelayL" "Envelope" "Filter" 
  "Instrmnt" "Noise" "OneZero" "ReedTable" "Saxofony" "Stk"
  "WaveLoop" "WvIn"
  "NRev" "JCRev" "PRCRev" "PitShift" "Chorus"
  "Bowed" "BowTable" "ADSR" "OnePole" "BiQuad"
  "BandedWG" "DelayA"
  "Mandolin" "PluckTwo"
  "Sitar" "ModalBar" "Modal"
  "Flute" "JetTable" "PoleZero"
))

;; stkfiles - .c and .h files from /nyqstk
(setf stkfiles '("stkinit" "instr" "stkint"))

;; fftfiles - .c and .h files from /ffts/src
(setf fftfiles '("fftext" "fftlib" "matlib"))

;; cmtfiles - .c and .h files from /cmt
(setf cmtfiles '("cext" "cleanup" "cmdline" "cmtcmd" 
  "moxc" "mem" "midifile" "midifns" "record"
  "seq" "seqmread" "seqmwrite" "seqread" "seqwrite" "tempomap"
  "timebase" "userio")) ; "midimgr" - removed by RBD

;; cmtfiles-h - extra .h files in /cmt (not used)
;(setf cmtfiles-h '("mfmidi" "midicode" "midierr" "musiprog"
;  "pitch"  "swlogic" "hash" "hashrout" "io" "midibuff"))

;; nylsffiles - .c and .h files from /nylsf (a local version of libsndfile)
(setf nylsffiles '("aiff" "alaw" "au" "audio_detect"
        "avr" "broadcast"
	"caf" "chanmap" "chunk" "command" "common" "dither"
	"double64" "dwd" "dwvw" "file_io"
	"flac" "float32" "gsm610" "htk" "id3"
	"ima_adpcm" "ima_oki_adpcm" "interleave" "ircam" 
	"macbinary3" "macos" "mat4" "mat5" "mpc2k"
        "ms_adpcm" "nist" "ogg" "ogg_vorbis" "paf"
	"pcm" "pvf" "raw" "rf64" "rx2" "sd2"
	"sds" "sndfile" "strings" "svx"
	"txw" "ulaw" "voc" "vox_adpcm"
	"w64" "wav" "wav_w64" "wve"
	"xi" "g72x"
        "GSM610/add" "GSM610/code" "GSM610/decode"
	"GSM610/gsm_create" "GSM610/gsm_decode"
	"GSM610/gsm_destroy" "GSM610/gsm_encode"
	"GSM610/gsm_option" "GSM610/long_term"
	"GSM610/lpc" "GSM610/preprocess"
	"GSM610/rpe" "GSM610/short_term"
	"GSM610/table"
	"G72x/g721" "G72x/g723_16" "G72x/g723_24"
	"G72x/g723_40" "G72x/g72x"))

;; sysunixfiles - .c and .h files from /sys/unix
(setf sysunixfiles '("osstuff" "term"))

;; nylsffiles-h - extra .h files needed for /nylsf (not used)
;(setf nylsffiles-h '("common" "config" "float_cast" "sfconfig"
;                     "endian" "sf_unistd" "sndfile" "wav_w64"
;                     "GSM610/gsm610_priv" "GSM610/gsm"
;                     "G72x/g72x" "G72x/g72x_priv"))


;; sndfiles-lsp - .lsp files generated by intgen (not used)
;(setf sndfiles-lsp '("sndfmt" "sndfile"))

;; javafiles - source files for Java
(setf javafiles '("browser" "NyquistThread"
          "Pair" "BareBonesBrowserLaunch"
          "EnvelopeFrame" "Piano_Roll"
          "FindDialog" "PlotFrame"
          "InstrumentCharacteristics"
          "PlotMouseAdapter"
          "Jslide" "PopupListener"
          "LispFileFilter" "PreferencesDialog"
          "MainFrame_AboutBox" "ReplaceDialog"
          "MainFrame" "SpringUtilities"
          "Main"
          "NotFoundDialog" "TextColor"
          "NyqPlot" "Trie"
          "NyquistFile" "WordList"))


;; Other variables that control Makefile generation:
;
;; system-type - a symbol naming the system to create a Makefile for
;;               current options are ALSA and NONALSA
;; target-file - a string for the target, default is "ny"
;;
