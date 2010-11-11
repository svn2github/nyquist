;; makefile.lsp -- builds makefiles for various machine types

(setf system-types '(rs6k next pmax sparc sgi linux))

(if (not (boundp 'system-type)) (setf system-type nil))
(if (not (boundp 'target-file)) (setf target-file "ny"))

(format t "System types: ~A~%" system-types)
(format t "(Only linux has been supported for a long time.)~%")
(format t "Current type: ~A~%" system-type)
(format t "Current target: ~A~%" target-file)
(format t "~%Instructions: (run from top nyquist directory)~%")
(format t "Choose a system from the list above by typing:~%")
(format t "\t(setf system-type '<a system type>)~%")
(format t "Override the executable name or location by:~%")
(format t "\t(setf target-file \"unix-path-name/ny\")~%")
(format t "To build the Makefile, type:~%")
(format t "\t(makefile)~%")
(format t "To make Makefiles for all system types, type:~%")
(format t "\t(makeall)~%")
(format t "To make sndfn.wcl and sndfn.cl, type:~%")
(format t "\t(commandline)~%")

;(format t "To build the Makesrc file, type:~%")
;(format t "\t(makesrc)~%")
;(format t
;"Note: Makesrc is used to update sources from other directories.
;    It isn't necessary if you got the sources from the normal
;    .tar file release of Nyquist
;")


(setf xlfiles '("extern" "xldmem" 
  "xlbfun" "xlcont" "xldbug" "xleval"
  "xlfio" "xlftab" "xlglob" "xlimage" "xlinit" "xlio" "xlisp"
  "xljump" "xllist" "xlmath" "xlobj" "xlpp" "xlprin" "xlread"
  "xlstr" "xlsubr" "xlsym" "xlsys" "path"))

(setf xlfiles-h '("osdefs" "osptrs" "xldmem" "xlisp" "extern"))

(setf xlfiles-lsp '("xlinit" "misc" "evalenv" "printrec"))

; ************************************
; CHANGED stksrcfiles. PJM July 2007
; ************************************

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

; ***************************************************
; CHANGED stkfiles. PJM July 2007
; Added stkint, An interface for new stk instruments
; ***************************************************

(setf stkfiles '("stkinit" "instr" "stkint"))

(setf liblofiles '(
"server" "timetag" "pattern_match"
  "message" "send" "blob" "address" "bundle" "method"))

(setf fftfiles '("fftext" "fftlib" "matlib"))

;(setf pafiles '("src/common/pa_front" "src/os/unix/pa_unix_hostapis" 
;                "src/hostapi/oss/pa_unix_oss" "src/os/unix/pa_unix_util"
;                "src/common/pa_cpuload" "src/common/pa_allocation"
;                "src/common/pa_stream" "src/common/pa_converters"
;                "src/common/pa_process" "src/common/pa_dither"
;                "src/common/pa_trace" "src/common/pa_debugprint"))

; note: audio<sys> and snd<sys> will be prepended to this list, e.g.
;   the strings "audiooss" and "sndlinux" will be added for linux systems
;
(defun init-sndfiles ()
  (setf sndfiles '("ieeecvt" "snd" "sndcvt" "sndio" "sndheader"))
  (setf sndfiles-lsp '("snd")))

(init-sndfiles)

(setf depends-exceptions '(
   ("nyqsrc/handlers" "") 
   ;("nyqsrc/sndfail" "")
   ("nyqsrc/local" "xlisp/xlisp.h nyqsrc/sound.h")
   ("nyqsrc/stats" "nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h")
   ("snd/sndcvt" "snd/snd.h")
   ("snd/sndio" "snd/snd.h")
   ("snd/audiors6k" "snd/snd.h")
   ("snd/audionext" "snd/snd.h")
   ("snd/audiosgi" "snd/snd.h")
   ("snd/audiopmax" "snd/snd.h")
   ("snd/audiosparc" "snd/snd.h")
   ("snd/audiolinux" "snd/snd.h")
   ("snd/audiooss" "snd/snd.h")
   ("nyqsrc/sndwritepa" "nyqsrc/sndwrite.h")
   ("nyqsrc/sndfnint" "")  ; sparc needs explicit rule for sndfnint.o
   ("nyqsrc/seqfnint" "")  ; ditto for seqfnint.o
))

(setf nyqfiles-lsp '("init" "nyquist" "seqmidi" "seq" "makefile" "update" "transfiles" "examples" "nyinit"))

(setf system-types-as-strings (mapcar #'string-downcase 
                (mapcar #'symbol-name system-types)))
(setf nyqfiles-lsp (append nyqfiles-lsp system-types-as-strings))

(setf nyqfiles-h '("localdefs" "localptrs" "seqdecls" "cque" "switches"))

(setf intfiles '("sndfnint" "seqfnint"))

(setf extrafiles nil)
;(dolist (m system-types)
;        (push (strcat "Makefile." 
;                      (string-downcase (symbol-name m)))
;              extrafiles))
(push "export" extrafiles)
(push "README" extrafiles)
(push "howtorelease.doc" extrafiles)

(setf cmtfiles '("cext" "cleanup" "cmdline" "cmtcmd" 
  "moxc" "mem" "midifile" "midifns" "record"
  "seq" "seqmread" "seqmwrite" "seqread" "seqwrite" "tempomap"
  "timebase" "userio")) ; "midimgr" - removed by RBD

(setf cmtfiles-h '("mfmidi" "midicode" "midierr" "musiprog"
  "pitch"  "swlogic" "hash" "hashrout" "io" "midibuff"))


(setf nylsffiles '("aiff" "alaw" "au" "avr" "broadcast"
	"caf" "command" "common" "dither"
	"double64" "dwd" "dwvw" "file_io"
	"flac" "float32" "gsm610" "htk"
	"ima_adpcm" "interleave" "ircam" "macbinary3"
        "macos" "mat4" "mat5" "ms_adpcm"
        "nist" "ogg" "paf"
	"pcm" "pvf" "raw" "rx2" "sd2"
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

(setf nylsffiles-h '("common" "config" "float_cast" "sfconfig"
                     "endian" "sf_unistd" "sndfile" "wav_w64"
                     "GSM610/gsm610_priv.h" "GSM610/gsm.h"
                     "G72x/g72x.h" "G72x/g72x_priv.h"))

(defun insert-separator (pre sep lis)
  (mapcar #'(lambda (pair) 
              (cond ((= (length pair) 2)
                     (strcat pre (car pair) sep (cadr pair) ".h"))
                    (t
                     (strcat (car pair) pre (cadr pair) sep (caddr pair) ".h"))))
          lis))

;; COMMAND-PREFIX -- insert prefix before each file
;;
(defun command-prefix (prefix lis)
  (mapcar #'(lambda (item) (list prefix item))
          lis))

(defun fix-sndwritepa (lis)
  ;; exception: sndwritepa.h -> sndwrite.h
  (mapcar #'(lambda (f)
	      (cond ((equal f "sndwritepa") "sndwrite")
		    (t f)))
	  lis))

;; COMMAND-FILELIST -- build files for command line
;;
(defun command-filelist (prefix separator)
  (let ()
    (setf filelist '(("snd" "snd")))
    (setf filelist (append filelist 
			   (command-prefix "nyqsrc" 
					   (fix-sndwritepa nyqsrcfiles))))
    (display "after nyqsrc" filelist nyqsrcfiles)
    (setf filelist (append filelist '(("~" "nyqsrc" "sndheader"))))
    (setf filelist (append filelist (command-prefix "tran" transfiles)))
    (cons (strcat prefix "nyqsrc" separator "sndfnint") 
          (insert-separator prefix separator filelist))))


;; COMMANDLINE -- build sndfn.cl and sndfn.wcl for mac and windows
;; versions of intgen; the files will be written to current directory
;;
(defun commandline ()
  (princ "Your current directory should be nyquist, and you should have\n")
  (princ "just evaluated (load \"misc/makefile\") and (commandline).\n")
  (load "misc/transfiles") ;; get current versions of transfiles and nyqsrcfiles
  (let (filelist outf)
    (setf filelist (command-filelist "" "\\"))
    (setf outf (open "sndfn.wcl" :direction :output))
    (write-file-list outf filelist #\ )
    (close outf)
    ; now do the mac
    (setf filelist (command-filelist "" "/"))
    (setf outf (open "sndfn.cl" :direction :output))
    (write-file-list outf filelist #\ )
    (close outf)
    (princ "On Mac OS-X, you should now (exit) nyquist, and at the commandline\n")
    (princ "run macosxproject/build/Development/intgen @sndfn.cl\n")
    (princ "updates to sndfn.cl and sndfn.wcl should be moved to nyqsrc\n")
    ))

;; MAKEALL - makes all makefiles and copies them to nyqsrc
;; 
;; run this in nyquist/src
;; 
(defun makeall ()
;  (makesrc)
;  (system "cp -p Makesrc nyqsrc")
  (dolist (m system-types)
      (setf system-type m)
      (setf m (string-downcase (symbol-name m)))
      (init-sndfiles)
      (makefile)))

;; MAKE-AUDIO-NAME -- (strcat "audio" system-name)
;; jlh1 is audiooss something I need to track down and consider changing?
(defun make-audio-name (system-name)
  (cond ((eq system-type 'linux)
         "audiooss")
        (t (strcat "audio" system-name))))

;; MAKE-SND-NAME -- (strcat "audio" system-name)
(defun make-snd-name (system-name)
   (strcat "snd" system-name))


;; MAKEFILE - creates a Makefile from a list of sources
;;
;; reads sources from transfiles.lsp

;; *********************************************************
;; CHANGED. PJM July 2007
;; JAVASRC separators must be double bar  \\
;; Added onlyny: to compile only Nyquist. javac not needed
;; *********************************************************

(defun makefile ()
  (let (system-name outf outf-name)
    (load "misc/transfiles.lsp") ; just to make sure we're current
    (while (null system-type)
       (format t "Write Makefile for what system?  One of:~A~%" system-types)
       (setf system-type (read))
       (cond ((not (member system-type system-types))
          (format t "Unknown system type.~%")
          (setf system-type nil))))
    (setf system-name (string-downcase
      (symbol-name system-type)))
    (setf outf-name (strcat "sys/unix/" system-name "/Makefile"))
    (format t "Opening for output: ~A\n" outf-name)
    (setf outf (open outf-name :direction :output))
    (cond ((null outf)
           (error "could not open output file" outf-name)))
    (setf sndfiles (cons (make-audio-name system-name)
             (cons (make-snd-name system-name) sndfiles)))
    (format outf 
     "#
# Makefile for Nyquist, SYSTEM-TYPE is ~A
# run make in the top-level Nyquist directory to compile Nyquist
#
# NOTE: this file is machine-generated.  DO NOT EDIT!
#   Instead, modify makefile.lsp and regenerate the makefile.
#   Ports and bug fixes are welcome - please mail them to 
#   dannenberg@cs.cmu.edu.  Thanks.
#

# This is the resulting executable (normally \"ny\"):
NY = ~A

OPT = -O2 -m32
# OPT = -g -m32

EVERYTHING = $(NY) runtime/system.lsp jnyqide/jNyqIDE.jar \\
           bin/ser-to-osc bin/test-client

CURRENT = $(EVERYTHING)

current: $(CURRENT)

onlyny: $(NY) runtime/system.lsp

JAVASRC = jnyqide/browser.java jnyqide/NyquistThread.java \\
          jnyqide/Pair.java jnyqide/BareBonesBrowserLaunch.java \\
          jnyqide/EnvelopeFrame.java jnyqide/Piano_Roll.java \\
          jnyqide/FindDialog.java jnyqide/PlotFrame.java \\
          jnyqide/InstrumentCharacteristics.java \\
          jnyqide/PlotMouseAdapter.java \\
          jnyqide/Jslide.java jnyqide/PopupListener.java \\
          jnyqide/LispFileFilter.java jnyqide/PreferencesDialog.java \\
          jnyqide/MainFrame_AboutBox.java jnyqide/ReplaceDialog.java \\
          jnyqide/MainFrame.java jnyqide/SpringUtilities.java \\
          jnyqide/Main.java \\
          jnyqide/NotFoundDialog.java jnyqide/TextColor.java \\
          jnyqide/NyqPlot.java jnyqide/Trie.java \\
          jnyqide/NyquistFile.java jnyqide/WordList.java


jnyqide/jNyqIDE.jar: $(JAVASRC)
	if [ -r jnyqide/SpecialMacHandler.java ] ; then \\
	mv jnyqide/SpecialMacHandler.java jnyqide/SpecialMacHandler.hidden ;\\
	fi 
	cd jnyqide; javac *.java 
	mv jnyqide/SpecialMacHandler.hidden jnyqide/SpecialMacHandler.java 
	rm -rf jnyqide/jNyqIDE.jar 
	jar -cfm jnyqide/jNyqIDE.jar jnyqide/manifest.txt jnyqide/*.class 

# Standard list of includes (common to all unix versions)
# Keeping portaudio and libsndfile sources local to nyquist
INCL = -Inyqsrc -Itran -Ixlisp -Isys/unix -Icmt -Iffts/src \\
   -Inyqstk/include -Inyqstk -Iportaudio/include -Iportaudio/src/common \\
   -Iportaudio/src/os/unix \\
   -Iliblo -Inylsf

# system dependent stuff for ~A:
~A

INTGEN = misc/intgen

# Object files for Nyquist:
" system-type target-file system-name (system-defs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (object-files outf)
    (format outf "# Sound functions to add to xlisp~%")
    (nyqheaders outf)
    (cmtheaders outf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format outf "

bin:
\tmkdir bin

liblo/Makefile:
\tcd liblo; ./configure --enable-static --disable-shared
\t# sometimes, residual files cause problems
\tcd liblo; make clean

$(LIBLO_PATH)/liblo.a: liblo/Makefile
\tcd liblo; make

bin/ser-to-osc: bin $(LIBLO_PATH)/liblo.a
\t$(CC) -c $(CFLAGS) liblo/ser-to-osc/ser-to-osc.cpp \\
\t      -o liblo/ser-to-osc/ser-to-osc.o
\t$(LN)  liblo/ser-to-osc/ser-to-osc.o -o bin/ser-to-osc $(LFLAGS)

bin/test-client: bin $(LIBLO_PATH)/liblo.a
\t$(CC) -c $(CFLAGS) liblo/test-client/test-client.c \\
\t      -o liblo/test-client/test-client.o
\t$(LN) liblo/test-client/test-client.o -o bin/test-client  $(LFLAGS)

portaudio/Makefile:
\tcd portaudio; ./configure
\t# sometimes, residual files cause problems
\tcd portaudio; make clean

$(LIBPA_PATH)/libportaudio.a: portaudio/Makefile
\tcd portaudio; make

$(NY): $(OBJECTS) $(LIBPA_PATH)/libportaudio.a $(LIBLO_PATH)/liblo.a
\t$(LN) $(OBJECTS) $(LFLAGS) -o $(NY)

# copy appropriate system.lsp and make it read-only;
# changes should be made to sys/unix/<system>/system.lsp
runtime/system.lsp: sys/unix/~A/system.lsp
\t# make sure it's there before you make it writeable
\ttouch runtime/system.lsp
\tchmod +w runtime/system.lsp
\tcp -p sys/unix/~A/system.lsp runtime/system.lsp
\tchmod -w runtime/system.lsp

" system-name system-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (dependencies outf system-name)
    (format outf (system-rules))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (format outf
        "misc/intgen: misc/intgen.c
\tcd misc; make intgen

misc/unpacker: misc/unpacker.c misc/convert.c
\tcd misc; make unpacker

misc/packer: misc/packer.c misc/convert.c
\tcd misc; make packer

nyqsrc/sndfnintptrs.h: $(NYQHDRS) misc/intgen
\t$(INTGEN) nyqsrc/sndfnint $(NYQHDRS)

nyqsrc/seqfnintptrs.h: $(CMTHDRS) misc/intgen
\t$(INTGEN) nyqsrc/seqfnint $(CMTHDRS)

clean:
\tcd misc; make clean
\tcd liblo; make clean
\tcd portaudio; make clean
\trm -f $(OBJECTS)
# These could be deleted, but they're part of the release, so we won't
# Note that these files are machine-generated:
# \trm -f nyqsrc/sndfnintptrs.h nyqsrc/sndfnint.c nyqsrc/sndfnintdefs.h
# \trm -f nyqsrc/seqfnintptrs.h nyqsrc/seqfnint.c nyqsrc/seqfnintdefs.h

cleaner: clean
\tcd misc; make cleaner
\trm -f *.backup */*.backup
\trm -f *~~ */*.*~~
\trm -f #*# */#*#
\trm -f *.save */*.save
\trm -f *.CKP */*.CKP
\trm -f *.BAK */*.BAK
\trm -f *.old */*.old
\trm -f *.gold */*.gold
\trm -f playparms
\trm -f points.dat
\trm -f core.* core
\trm -f $(NY)

release: cleaner
\tcd misc; make packer
\tmisc/packer files.txt release.pac
\trm -f *.wav
\tmv ny ..
\tmv -f *.pac ..
\trm -f unpacker
\trm -f packer
\tcd ..; zip -r nyquist.zip nyquist
\t../ny misc/cmu/cmu-linux-install.lsp
\tmv ../ny ./ny
")

    (cond ((eq system-type 'rs6k)
       (format outf "
tar: cleaner
\tsh -v sys/unix/cmu/tar.script

backup: cleaner
\tsh -v sys/unix/cmu/backup.script
")))
    (close outf)
    ))

;; system-defs looks for a string of system-dependent defs for the makefile
;;
(defun system-defs () (system-var "-DEFS"))


;; system-rules looks for a string of system-dependent rules for the makefile
;;
(defun system-rules () (system-var "-RULES"))


;; system-var returns a string stored in the variable (if any):
;;   <system-type>-<suffix>
;;
(defun system-var (suffix)
  (let ((v (intern (strcat (symbol-name system-type) suffix))))
    (cond ((boundp v) (symbol-value v))
      (t ""))))


(defun fix-sndsliders (lis)
  (remove "sndsliders" lis :test #'string=))

;; object-files - writes names of all object files for linking
;;
(defun object-files (outf)
  (let ((flist (append (add-prefix "xlisp/" xlfiles)
               (add-prefix "tran/" transfiles)
               (add-prefix "cmt/" cmtfiles)
               (add-prefix "nylsf/" nylsffiles)
               (add-prefix "nyqsrc/" nyqfiles)
               (add-prefix "nyqsrc/" (fix-sndsliders nyqsrcfiles)) ; pjm January 2008
               (add-prefix "nyqstk/src/" stksrcfiles)
               (add-prefix "nyqstk/" stkfiles)
               ;(add-prefix "liblo/src/" liblofiles)
               (add-prefix "ffts/src/" fftfiles)
               (add-prefix "nyqsrc/" intfiles)
               ;(add-prefix "snd/" sndfiles)
               ;(add-prefix "portaudio/" pafiles) ; jlh1
               '("sys/unix/osstuff" "sys/unix/term"))))
    (setf flist (add-suffix flist ".o"))
    (format outf "OBJECTS = ")
    (write-file-list outf flist #\\)))


;; add-prefix - place string at beginning of each string in list
;;
(defun add-prefix (prefix lis)
  (mapcar #'(lambda (str) (strcat prefix str)) lis))


;; add-suffix - place string at end of each string in list
;;
(defun add-suffix (lis suffix)
  (mapcar #'(lambda (str) (strcat str suffix)) lis))


;; write-file-list - write file names to Make macro
;;
(defun write-file-list (outf flist continuation-char)
  (while flist
     (dotimes (i 2)
          (format outf "~A " (car flist))
          (setf flist (cdr flist))
          (if (null flist) (return)))
     (if flist (format outf " ~A~%\t" continuation-char)))
  (format outf "~%~%"))


(defun nyqheaders (outf)
  (let ((flist (append
                (list "nyqsrc/sndfmt" "nylsf/sndfile")
                 (add-prefix "nyqsrc/" (fix-sndwritepa nyqsrcfiles))
                 (add-prefix "tran/" transfiles))))
    (setf flist (mapcar #'(lambda (f) (strcat f ".h"))
                        flist))
    (format outf "NYQHDRS = ")
    (write-file-list outf flist #\\)))


(defun cmtheaders (outf)
  (let ((flist 
     (append '("cmt/seqdecls" "nyqsrc/seqext" "cmt/seq"
           "nyqsrc/seqinterf")  ; order is important here!
         (add-prefix "cmt/"
          '("seqread" "seqmread" "seqwrite" "seqmwrite")))))
    (setf flist (add-suffix flist ".h"))
    (format outf "CMTHDRS = ")
    (write-file-list outf flist #\\)))


(defun dependencies (outf system-name)
  ;; this forces generation of sndfnintdefs.h, seqfnintdefs.h:
  (dolist (f (append (add-prefix "nyqsrc/" nyqsrcfiles)
	     (add-prefix "nyqsrc/" nyqfiles)
             ;(add-prefix "snd/" sndfiles)
             (add-prefix "ffts/src/" fftfiles)
             ; (add-prefix "liblo/" liblofiles)
             (add-prefix "tran/" transfiles)
             (add-prefix "nyqsrc/" intfiles)))
    (let ((ex (assoc f depends-exceptions :test #'equal)))
      (cond ((and ex (cdr ex))
         (format outf "~A.o: ~A.c ~A~%" f f (cadr ex))
         (format outf "\t$(CC) -c ~A.c -o ~A.o $(CFLAGS)~%~%" f f))
        (t
         (format outf "~A.o: ~A.c ~A.h nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h~%"
              f f f)
         (format outf "\t$(CC) -c ~A.c -o ~A.o $(CFLAGS)~%~%" f f)))))
  (dolist (f stkfiles)
     (format outf "nyqstk/~A.o: nyqstk/~A.cpp nyqstk/~A.h~%"
             f f f)
     (format outf "\tg++ -c nyqstk/~A.cpp -o nyqstk/~A.o $(CFLAGS)~%~%"
             f f))

  (dolist (f stksrcfiles)
     (format outf "nyqstk/src/~A.o: nyqstk/src/~A.cpp nyqstk/include/~A.h~%"
             f f f)
     (format outf "\tg++ -c nyqstk/src/~A.cpp -o nyqstk/src/~A.o $(CFLAGS)~%~%"
             f f))

  (format outf "xlisp/xlftab.o: nyqsrc/sndfnintptrs.h nyqsrc/sndfnintdefs.h")
  (format outf " nyqsrc/seqfnintptrs.h nyqsrc/seqfnintdefs.h~%")
  (format outf "\t$(CC) -c xlisp/xlftab.c -o xlisp/xlftab.o $(CFLAGS)~%~%")
  (dolist (f (append (add-prefix "xlisp/" xlfiles)
                     (add-prefix "cmt/" cmtfiles)
                     (add-prefix "nylsf/" nylsffiles)
                     '("sys/unix/osstuff")))
    (cond ((and (not (equal f "xlisp/xlftab"))   ; special case handled above
        (not (and (equal f "xlisp/xljump") ; case handled below
              (equal system-name "next"))))
       (format outf "~A.o: ~A.c~%\t$(CC) -c ~A.c -o ~A.o $(CFLAGS)~%~%"
           f f f f)))))


;;===================================================
;; SYSTEM DEPENDENCIES
;;===================================================

(setf rs6k-defs "
MIDI = /afs/cs/project/music/rs6k/midilib
CC = cc
# change -g to -O for optimization
# to enable command line editing, add -DREADLINE
CFLAGS = -DCMTSTUFF -g $(INCL) -I$(MIDI)
XFLAGS = $(CFLAGS) -qlanglvl=extended
LN = xlc -qattr -qlist
# to enable command line editing, add -lreadline -lcurses
LFLAGS = -lm -lpthread -L$(MIDI) -lmidi -lbsd -lg
")


(setf next-defs "
CC = cc
# to enable command line editing, insert -DREADLINE
CFLAGS = -DCMTSTUFF -O $(INCL)
LN = cc
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = -lm -lpthread
")

(setf next-rules "
# this doesn't compile with the -O switch (a NeXT compiler bug?)
xlisp/xljump.o : xlisp/xljump.c xlisp/xlisp.h
\t$(CC) -DCMTSTUFF -c xlisp/xljump.c -o xlisp/xljump.o
")

(setf pmax-defs "
CC = cc
# to enable command line editing, insert -DREADLINE
CFLAGS = -DCMTSTUFF -g $(INCL)
LN = cc
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = -lm
")


(setf sgi-defs "
CC = cc
# to enable command line editing, insert -DREADLINE
CFLAGS = -DCMTSTUFF -g $(INCL)
LN = cc
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = -lm -lpthread
# you would need -lmd if UNIX_IRIX_MIDIFNS were defined in midifns.c
")


(setf sparc-defs "
CC = gcc
# to enable command line editing, insert -DREADLINE
CFLAGS = -DCMTSTUFF -g $(INCL)
LN = g++
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = -lm -lpthread
")

(setf linux-defs "
CC = gcc

LIBPA_PATH = portaudio/lib/.libs

LIBLO_PATH = liblo/src/.libs

# to enable command line editing, use -DREADLINE. WARNING: THIS WILL 
# DISABLE THE ABILITY TO INTERRUPT LISP AND USE SOME OTHER HANDY 
# CONTROL CHARACTERS (You will also need the readline and curses libraries)
CFLAGS = -DOSC -DCMTSTUFF -DPA_LITTLE_ENDIAN $(OPT) $(INCL) \\
    -DHAVE_LIBPTHREAD=1 -D_FILE_OFFSET_BITS=64 \\
    -DSTK_NYQUIST -DUSE_VSPRINTF \\
    -DHAVE_CONFIG_H
LN = g++ -m32
AR = ar
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = $(LIBPA_PATH)/libportaudio.a -lm -lpthread -lasound -llo -L$(LIBLO_PATH)

TAGS:
	find . \( -name "*.c" -o -name "*.h" \) -print | etags -

tags: TAGS
")
