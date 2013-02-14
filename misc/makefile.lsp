;; makefile.lsp -- builds makefiles for various machine types

(load "misc/files.lsp") ; just to make sure we're current

(setf system-types '(alsa nonalsa))

(if (not (boundp 'system-type)) (setf system-type nil))
(if (not (boundp 'target-file)) (setf target-file "ny"))

(format t "System types: ~A~%" system-types)
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


;; add-prefix - place string at beginning of each string in list
;;
(defun add-prefix (prefix lis)
  (mapcar #'(lambda (str) (strcat prefix str)) lis))


;; add-suffix - place string at end of each string in list
;;
(defun add-suffix (lis suffix)
  (mapcar #'(lambda (str) (strcat str suffix)) lis))


(setf depends-exceptions '(
   ("nyqsrc/handlers" "") 
   ;("nyqsrc/sndfail" "")
   ("nyqsrc/local" "xlisp/xlisp.h nyqsrc/sound.h")
   ("nyqsrc/stats" "nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h")
   ("nyqsrc/sndwritepa" "nyqsrc/sndwrite.h")
   ("nyqsrc/sndfnint" "")  ; sparc needs explicit rule for sndfnint.o
   ("nyqsrc/seqfnint" "")  ; ditto for seqfnint.o
))


(setf system-types-as-strings (mapcar #'string-downcase 
                (mapcar #'symbol-name system-types)))

(setf intfiles '("sndfnint" "seqfnint"))

(setf object-files 
  (append
   (add-prefix "xlisp/" xlfiles)
   (add-prefix "tran/" transfiles)
   (add-prefix "cmt/" cmtfiles)
   (add-prefix "nylsf/" nylsffiles)
   (add-prefix "nyqsrc/" nyqfiles)
   (add-prefix "nyqsrc/" nyqsrcfiles)
   (add-prefix "nyqstk/src/" stksrcfiles)
   (add-prefix "nyqstk/" stkfiles)
   (add-prefix "ffts/src/" fftfiles)
   (add-prefix "nyqsrc/" intfiles)
   (add-prefix "sys/unix/" sysunixfiles)))


;; insert-separator -- converts each list (dir name) to pre/dir/name.h
;;     pre is the prefix string
;;     sep is the separator character, e.g. "/"
;;     lis is the list of (dir name) pairs
;; result is ("pre/dir1/name1.h" "pre/dir2/name2.h" ...)
;;
;; A special case is that an element of lis may be a triple, e.g.
;;   ("~" "dir" "file") which is converted to ~pre/dir/file.h
;;
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


;; COMMAND-FILELIST -- build files for command line
;;
(defun command-filelist (separator)
  (let ()
    (setf filelist (append filelist 
			   (command-prefix "nyqsrc" nyqsrcfiles)))
    (display "after nyqsrc" filelist nyqsrcfiles)
    ;; sndheader no longer needed with libsndfile
    ;;(setf filelist (append filelist '(("~" "nyqsrc" "sndheader"))))
    (setf filelist (append filelist (command-prefix "tran" transfiles)))
    (cons (strcat "nyqsrc" separator "sndfnint") 
          (insert-separator separator filelist))))


;; COMMANDLINE -- build sndfn.cl and sndfn.wcl for mac and windows
;; versions of intgen; the files will be written to current directory
;;
(defun commandline ()
  (princ "Your current directory should be nyquist, and you should have\n")
  (princ "just evaluated (load \"misc/makefile\") and (commandline).\n")
  (let (filelist outf)
    (setf filelist (command-filelist "\\"))
    (setf outf (open "sndfn.wcl" :direction :output))
    (write-file-list outf "" filelist #\ )
    (close outf)
    ; now do the mac
    (setf filelist (command-filelist "/"))
    (setf outf (open "sndfn.cl" :direction :output))
    (write-file-list outf "" filelist #\ )
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
      (makefile)))


;; MAKEFILE - creates a Makefile from a list of sources
;;
;; reads sources from transfiles.lsp

(defun makefile ()
  (let (system-name outf outf-name portaudio-host-flag)
    (while (null system-type)
       (format t "Write Makefile for what system?  One of:~A~%" system-types)
       (setf system-type (read))
       (cond ((not (member system-type system-types))
          (format t "Unknown system type.~%")
          (setf system-type nil))))
    (setf system-name (string-downcase
      (symbol-name system-type)))
    (setf portaudio-host-flag 
	  (if (eq system-type 'ALSA) "--with-alsa" "--with-oss"))
    (setf outf-name (strcat "sys/unix/" system-name "/Makefile"))
    (format t "Opening for output: ~A\n" outf-name)
    (setf outf (open outf-name :direction :output))
    (cond ((null outf)
           (error "could not open output file" outf-name)))
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

" system-type target-file)

   (write-file-list outf "JAVASRC = "
    (add-prefix "jnyqide/" (add-suffix javafiles ".java")) #\\)
   (format outf
"jnyqide/jNyqIDE.jar: $(JAVASRC)
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
   -Iliblo -Inylsf -IFLAC/include -Ilibogg/include

# system dependent stuff for ~A:
~A
# end of system dependencies

INTGEN = misc/intgen

# Object files for Nyquist:
" 
     system-name (system-defs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (write-object-files outf)
    (format outf "# Sound functions to add to xlisp~%")
    (sndfnint-headers outf)
    (seqfnint-headers outf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format outf "

liblo/Makefile:
\tcd liblo; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared
\t# sometimes, residual files cause problems
\tcd liblo; make clean

$(LIBLO_PATH)/liblo.a: liblo/Makefile
\tcd liblo; make

bin/ser-to-osc: $(LIBLO_PATH)/liblo.a
\tmkdir -p bin
\t$(CC) -c $(CFLAGS) liblo/ser-to-osc/ser-to-osc.cpp \\
\t      -o liblo/ser-to-osc/ser-to-osc.o
\t$(LN)  liblo/ser-to-osc/ser-to-osc.o -o bin/ser-to-osc $(LFLAGS)

bin/test-client: $(LIBLO_PATH)/liblo.a
\tmkdir -p bin
\t$(CC) -c $(CFLAGS) liblo/test-client/test-client.c \\
\t      -o liblo/test-client/test-client.o
\t$(LN) liblo/test-client/test-client.o -o bin/test-client  $(LFLAGS)

portaudio/Makefile:
\t# note: without-jack avoids 32/64-bit link error on Debian
\tcd portaudio; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared --without-jack ~A
\t# sometimes, residual files cause problems
\tcd portaudio; make clean

$(LIBPA_PATH)/libportaudio.a: portaudio/Makefile
\tcd portaudio; make

libogg/Makefile:
	cd libogg; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared

$(LIBOGG_PATH)/libogg.a: libogg/Makefile
	cd libogg; make

# NOTE: libvorbis/configure on a 64-bit machine will expect to find
#  libogg installed (even though we are not going to use the installed
#  libogg library, and even though the installed libogg library will
#  be for 64-bit architecture. This represents a bug in configure
#  because it checks for a 64-bit library when it is building for
#  a 32-bit architecture (we pass in CFLAGS=-m32). In spite of the 
#  bug, configure will build a Makefile that will build a 32-bit
#  libvorbis library that we need. We will link it with other 
#  32-bit code including 32-bit libogg.a

libvorbis/Makefile:
	cd libvorbis; ./configure CFLAGS=-m32 LDFLAGS=-m32 CXXFLAGS=-m32 --enable-static --disable-shared

$(LIBVORBIS_PATH)/libvorbis.a: libvorbis/Makefile
	cd libvorbis; make

$(LIBVORBIS_PATH)/libvorbisfile.a: libvorbis/Makefile
	cd libvorbis; make

$(LIBVORBIS_PATH)/libvorbisenc.a: libvorbis/Makefile
	cd libvorbis; make


$(LIBFLAC_PATH)/libFLAC.a: FLAC/src/libFLAC/Makefile.lite
	cd FLAC/src/libFLAC; make -f Makefile.lite

$(NY): $(OBJECTS) $(LIBPA_PATH)/libportaudio.a $(LIBLO_PATH)/liblo.a \\
       FLAC/obj/release/lib/libFLAC.a \\
       $(LIBVORBIS_PATH)/libvorbis.a $(LIBVORBIS_PATH)/libvorbisfile.a \\
       $(LIBVORBIS_PATH)/libvorbisenc.a $(LIBOGG_PATH)/libogg.a
	$(LN) $(OBJECTS) $(LFLAGS) -o $(NY)

# copy appropriate system.lsp and make it read-only;
# changes should be made to sys/unix/<system>/system.lsp
runtime/system.lsp: sys/unix/~A/system.lsp
#\tmake sure it's there before you make it writeable
\ttouch runtime/system.lsp
\tchmod +w runtime/system.lsp
\tcp -p sys/unix/~A/system.lsp runtime/system.lsp
\tchmod -w runtime/system.lsp

" portaudio-host-flag system-name system-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (format outf "NYQDEP = nyqsrc/sound.h nyqsrc/falloc.h nyqsrc/cque.h~%~%")

    (dependencies outf system-name)
    (format outf (system-rules))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (format outf
        "misc/intgen: misc/intgen.c
\tcd misc; make intgen

nyqsrc/sndfnintptrs.h: $(SNDFNINT_HDRS) misc/intgen Makefile
\t$(INTGEN) nyqsrc/sndfnint $(SNDFNINT_HDRS)

nyqsrc/seqfnintptrs.h: $(SEQFNINT_HDRS) misc/intgen Makefile
\t$(INTGEN) nyqsrc/seqfnint $(SEQFNINT_HDRS)

clean:
\tcd misc; make clean
\tcd liblo; test -f Makefile && make clean || true
\tcd portaudio; test -f Makefile && make clean || true
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
")

    (close outf)
    ))

;;;;;; SYSTEM DEPENDENT STUFF ;;;;;;;;;;;;;;;;;;;;
;;
;; The way this works is you define variables, e.g. ALSA-DEFS or NONALSA-RULES
;; in order to insert system-dependent rules at different places in the
;; Makefile. As of this writing, these are undefined and so nothing (empty
;; string) is generated.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;; END OF SYSTEM DEPENDENT STUFF ;;;;;;;;;;;;;;;


;; write-object-files - writes names of all object files for linking
;;
(defun write-object-files (outf)
  (let ((flist object-files))
    (setf flist (add-suffix flist ".o"))
    (write-file-list outf "OBJECTS = " flist #\\)))


;; add-prefix - place string at beginning of each string in list
;;
(defun add-prefix (prefix lis)
  (mapcar #'(lambda (str) (strcat prefix str)) lis))


;; add-suffix - place string at end of each string in list
;;
(defun add-suffix (lis suffix)
  (mapcar #'(lambda (str) (strcat str suffix)) lis))


;; write-file-list - write file names as list with line continuation char
;;    outf - output file
;;    first - an initial string to write, e.g. "MYMACRO = "
;;    flist - list of files
;;    continuation-char - typically #\\, used to wrap lines in Makefile macros
;;
(defun write-file-list (outf first flist continuation-char)
  (let ((str first) str2 len)
    (while flist
      ;; see if we can add file to str
      (setf str2 (strcat str " " (car flist)))
      (setf len (+ (length str2)
                   (if (eql (char str2 0) #\t) 7 0)))
      (cond ((> len 72) ;; too long: print str and put filename on next line
             (format outf "~A ~A~%" str continuation-char)
             (setf str (strcat "\t" (car flist))))
            (t ;; not too long, filename goes at end of current line
             (setf str str2)))
      (setf flist (cdr flist)))
    ;; at loop exit, str has the left-overs
    (format outf "~A~%~%" str)))


(defun sndfnint-headers (outf)
  (let ((flist (append
                (list "nyqsrc/sndfmt" "nylsf/sndfile")
                (add-prefix "nyqsrc/" nyqsrcfiles)
                (add-prefix "tran/" transfiles)
		'("nyqsrc/sndwrite"))))
    (setf flist (add-suffix flist ".h"))
    (write-file-list outf "SNDFNINT_HDRS = " flist #\\)))


(defun seqfnint-headers (outf)
  (let ((flist 
     (append '("cmt/seqdecls" "nyqsrc/seqext" "cmt/seq"
           "nyqsrc/seqinterf")  ; order is important here!
         (add-prefix "cmt/"
          '("seqread" "seqmread" "seqwrite" "seqmwrite")))))
    (setf flist (add-suffix flist ".h"))
    (write-file-list outf "SEQFNINT_HDRS = " flist #\\)))


(defun dependencies (outf system-name)
  ;; this forces generation of sndfnintdefs.h, seqfnintdefs.h:
  (dolist (f (append (add-prefix "nyqsrc/" nyqsrcfiles)
	     (add-prefix "nyqsrc/" nyqfiles)
             ;(add-prefix "snd/" sndfiles)
             (add-prefix "ffts/src/" fftfiles)
             (add-prefix "tran/" transfiles)
             (add-prefix "nyqsrc/" intfiles)))
    (let ((ex (assoc f depends-exceptions :test #'equal)))
      (cond ((and ex (cdr ex))
         (format outf "~A.o: ~A.c ~A~%" f f (cadr ex))
         (format outf "\t$(CC) -c ~A.c -o ~A.o $(CFLAGS)~%~%" f f))
        (t
         (format outf "~A.o: ~A.c ~A.h $(NYQDEP)~%" f f f)
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

;; linux-defs - definitions for Makefile that are common to ALSA and NONALSA.
;;    we have NONALSA because Debian cannot link with -lasound. The NONALSA
;;    makefile omits -lasound.
(setf linux-defs "
CC = gcc

LIBPA_PATH = portaudio/lib/.libs
LIBOGG_PATH = libogg/src/.libs
LIBVORBIS_PATH = libvorbis/lib/.libs
LIBLO_PATH = liblo/src/.libs
LIBFLAC_PATH = FLAC/obj/release/lib

# to enable command line editing, use -DREADLINE. WARNING: THIS WILL 
# DISABLE THE ABILITY TO INTERRUPT LISP AND USE SOME OTHER HANDY 
# CONTROL CHARACTERS (You will also need the readline and curses libraries)
CFLAGS = -DOSC -DCMTSTUFF $(OPT) $(INCL) \\
    -DHAVE_LIBPTHREAD=1 -D_FILE_OFFSET_BITS=64 \\
    -DSTK_NYQUIST -DUSE_VSPRINTF \\
    -DHAVE_CONFIG_H
LN = g++ -m32
AR = ar
# to enable command line editing, insert -lreadline -lcurses
LFLAGS = -L/usr/lib32 $(LIBPA_PATH)/libportaudio.a \\
         $(LIBLO_PATH)/liblo.a $(AUDIOLIBS) $(LIBFLAC_PATH)/libFLAC.a \\
         $(LIBVORBIS_PATH)/libvorbis.a \\
         $(LIBVORBIS_PATH)/libvorbisfile.a \\
         $(LIBVORBIS_PATH)/libvorbisenc.a $(LIBOGG_PATH)/libogg.a \\
         -lm -lpthread -lrt

TAGS:
	find . \( -name \"*.c\" -o -name \"*.h\" \) -print | etags -

tags: TAGS
")

(setf alsa-defs (strcat "
AUDIOLIBS = -lasound
" linux-defs))

(setf nonalsa-defs (strcat "
AUDIOLIBS =
" linux-defs))

(format t "loaded makefile.lsp")
 
