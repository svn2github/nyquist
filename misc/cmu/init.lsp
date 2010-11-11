;; edit music.software.html and make install.bat

(load "nyinit.lsp")

;; see if we are running on Windows
(setf *windowsp* (not (null (listdir "WinRel"))))

(setf *remote* (if *windowsp* "q:\\web" "rbd@linux.gp.cs.cmu.edu:music/web"))

(defun edit-music-software-html ()
  (let (inf outf input i prefix postfix postfix1 postfix2)
    (setf inf (open "music.software.html"))
    (if (null inf) (error "could not open music.software.html"))
    (setf outf (open "new.html" :direction :output))
    (if (null outf) (error "could not open new.html for output"))
    (format t "Major version number (e.g. 2): ")
    (setf *maj* (read))
    (format t "Minor version number (e.g. 27): ")
    (setf *min* (read))
    
    ;; find version in heading
    (print "find Executables")
    (setf input (read-line inf))
    (while (not (setf i (string-search "Executables (v" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 14)))
    (setf postfix (subseq input (+ i 14)))
    (setf i (string-search ")" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A.~A~A~%" prefix *maj* *min* postfix)
    
    ;; find nyquist/setupnyqrun
    (print "find nyquist/setupnyqrun")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/setupnyqrun" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf))
      ;(display "finding nyquist/setupnyqrun" input)
      )
    (setf prefix (subseq input 0 (+ i 19)))
    (setf postfix (subseq input (+ i 19)))
    (setf i (string-search "\">" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A~A.exe~A~%" prefix *maj* *min* postfix)

    ;; find nyquist/setupnyqwinrun
;    (print "find nyquist/setupnyqwinrun")
;    (setf input (read-line inf))
;    (while (not (setf i (string-search "nyquist/setupnyqwinrun" input)))
;      (format outf "~A~%" input)
;      (setf input (read-line inf)))
;    (setf prefix (subseq input 0 (+ i 22)))
;    (setf postfix (subseq input (+ i 22)))
;    (setf i (string-search "\">" postfix))
;    (setf postfix (subseq postfix i))
;    (format outf "~A~A~A.exe~A~%" prefix *maj* *min* postfix)

    ;; find nyquist/setupnyqiderun
    (print "find nyquist/setupnyqiderun")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/setupnyqiderun" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 22)))
    (setf postfix (subseq input (+ i 22)))
    (setf i (string-search "\">" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A~A.exe~A~%" prefix *maj* *min* postfix)

    ;; find nyquist/nyqosx
    (print "find nyquist/nyqosx")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/nyqosx" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 14)))
    (setf postfix (subseq input (+ i 14)))
    (setf i (string-search ".tgz" postfix))
    (setf postfix (subseq postfix i))
    (setf i (string-search "(v" postfix))
    (setf postfix1 (subseq postfix 0 (+ i 2)))
    (setf postfix2 (subseq postfix (+ i 2)))
    (setf i (string-search ")" postfix2))
    (setf postfix2 (subseq postfix2 i))
    (format outf "~A~A~A~A~A.~A~A~%" prefix *maj* *min*
            postfix1 *maj* *min* postfix2)
    
    ;; find <tt>nyqosx
    (print "find <tt>nyqosx")
    (setf input (read-line inf))
    (while (not (setf i (string-search "<tt>nyqosx" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 10)))
    (setf postfix (subseq input (+ i 10)))
    (setf i (string-search "</tt>" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A~A~A~%" prefix *maj* *min* postfix)
    
    ;; find nyquist/nyqsrc
    (print "find nyquist/nyqsrc")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/nyqsrc" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 14)))
    (setf postfix (subseq input (+ i 14)))
    (setf i (string-search ".zip" postfix))
    (setf postfix (subseq postfix i))
    (setf i (string-search "(v" postfix))
    (setf postfix1 (subseq postfix 0 (+ i 2)))
    (setf postfix2 (subseq postfix (+ i 2)))
    (setf i (string-search ")" postfix2))
    (setf postfix2 (subseq postfix2 i))
    (format outf "~A~A~A~A~A.~A~A~%" prefix *maj* *min*
            postfix1 *maj* *min* postfix2)

    (setf input (read-line inf))
    (while input
      (format outf "~A~%" input)
      (setf input (read-line inf)))
      
    (close inf)
    (close outf)
   ))

; "

(defun make-install-bat ()
  (let (outf)
    (setf outf (open "cmuinstall2.bat" :direction :output))
    (format outf "copy ..\\..\\setup\\setupnyqrun.exe ~A\\nyquist\\setupnyqrun~A~A.exe~%"
            *remote* *maj* *min*)
;    (format outf "copy ..\\..\\setup\\setupnyqwinrun.exe ~A\\nyquist\\setupnyqwinrun~A~A.exe~%"
;            *remote* *maj* *min*)
    (format outf "copy ..\\..\\setup\\setupnyqiderun.exe ~A\\nyquist\\setupnyqiderun~A~A.exe~%"
            *remote* *maj* *min*)
    (format outf "copy new.html ~A\\music.software.html~%" *remote*)
    (format outf "call cleanup.bat~%")
    (format outf "echo \"In d:\\rbd, make nyquist.zip from nyquist now...then type return to the pause...\"~%")
    (format outf "pause~%")
    (format outf "move ..\\..\\..\\nyquist.zip ..\\..\\..\\nyquist~A~A.zip~%" *maj* *min*)
    (format outf "copy ..\\..\\..\\nyquist~A~A.zip ~A\\nyquist\\nyqsrc~A~A.zip~%"
            *maj* *min* *remote* *maj* *min*)
    (format outf "call restore.bat~%")
    (close outf)))

(defun make-install-sh ()
  (let (outf)
    (setf outf (open "cmuinstall2.sh" :direction :output))
    (format outf "echo \"Make sure /Volumes/rbd is mounted...then type return\"~%")
    (format outf "read~%");
    (format outf "scp /Volumes/rbd/nyquist/setup/setupnyqrun.exe ~A/nyquist/setupnyqrun~A~A.exe~%"
            *remote* *maj* *min*)
;    (format outf "scp /Volumes/rbd/nyquist/setup/setupnyqwinrun.exe ~A/nyquist/setupnyqwinrun~A~A.exe~%"
;            *remote* *maj* *min*)
    (format outf "scp /Volumes/rbd/nyquist/setup/setupnyqiderun.exe ~A/nyquist/setupnyqiderun~A~A.exe~%"
            *remote* *maj* *min*)
    (format outf "scp new.html ~A/music.software.html~%" *remote*)
#|
 ;; this is the old way to make a source zip file
    (format outf "echo \"In e:\\rbd\\nyquist\\misc\\cmu, run cleanup.bat now...then type return to the pause...\"~%")
    (format outf "read~%")
    (format outf "echo \"In e:\\rbd, make nyquist.zip from nyquist now...then type return to the pause...\"~%")
    (format outf "read~%")
    (format outf "mv /Volumes/rbd/nyquist.zip /Volumes/rbd/nyquist~A~A.zip~%" *maj* *min*)
    (format outf "scp /Volumes/rbd/nyquist~A~A.zip ~A/nyquist/nyqsrc~A~A.zip~%"
            *maj* *min* *remote* *maj* *min*)
    (format outf "echo \"In e:\\rbd\\nyquist\\misc\\cmu, run restore.bat now...then type return to the pause...\"~%")
|#
  ;; this is the new way to make a source zip file
    (format outf "echo making source zip file...\n")
    (format outf "cd ../..\n")
    (format outf "cvs export -DNOW nyquist\n")
    (format outf "rm -rf nyquist/demos/plight\n")
    (format outf "zip -r nyquist.zip nyquist\n")
    (format outf "scp nyquist.zip ~A/nyquist/nyqsrc~A~A.zip~%"
                 *remote* *maj* *min*)
    (format outf "rm -rf nyquist.zip nyquist\n")
    (format outf "cd misc/cmu\n")
    (format outf "read~%")
    (close outf)))

(edit-music-software-html)
(if *windowsp* (make-install-bat) (make-install-sh))


(exit)

