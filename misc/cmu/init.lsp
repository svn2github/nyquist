;; edit music.software.html and make install.bat

(load "nyinit.lsp")

(defun edit-music-software-html ()
  (let (inf outf input i prefix postfix postfix1 postfix2)
    (setf inf (open "music.software.html"))
    (setf outf (open "new.html" :direction :output))
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
    
    ;; find nyquist/setupnyrun
    (print "find nyquist/setupnyrun")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/setupnyrun" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 18)))
    (setf postfix (subseq input (+ i 18)))
    (setf i (string-search "\">" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A~A.exe~A~%" prefix *maj* *min* postfix)

    ;; find nyquist/setupnywinrun
    (print "find nyquist/setupnywinrun")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/setupnywinrun" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 21)))
    (setf postfix (subseq input (+ i 21)))
    (setf i (string-search "\">" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A~A.exe~A~%" prefix *maj* *min* postfix)

    ;; find nyquist/setupnyrun
    (print "find nyquist/setupnyiderun")
    (setf input (read-line inf))
    (while (not (setf i (string-search "nyquist/setupnyiderun" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    (setf prefix (subseq input 0 (+ i 21)))
    (setf postfix (subseq input (+ i 21)))
    (setf i (string-search "\">" postfix))
    (setf postfix (subseq postfix i))
    (format outf "~A~A~A.exe~A~%" prefix *maj* *min* postfix)
    
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
    (format outf "copy ..\\..\\setup\\setupnyrun.exe g:\\user\\rbd\\music\\web\\nyquist\\setupnyrun~A~A.exe~%"
            *maj* *min*)
    (format outf "copy ..\\..\\winsetup\\setupnywinrun.exe g:\\user\\rbd\\music\\web\\nyquist\\setupnywinrun~A~A.exe~%"
            *maj* *min*)
    (format outf "copy ..\\..\\idesetup\\setupnyiderun.exe g:\\user\\rbd\\music\\web\\nyquist\\setupnyiderun~A~A.exe~%"
            *maj* *min*)
    (format outf "copy new.html g:\\user\\rbd\\music\\web\\music.software.html~%")
    (format outf "call cleanup.bat~%")
    (format outf "echo \"In d:\\rbd, make nyquist.zip from nyquist now...\"~%")
    (format outf "pause~%")
    (format outf "move ..\\..\\..\\nyquist.zip ..\\..\\..\\nyquist~A~A.zip~%" *maj* *min*)
    (format outf "copy ..\\..\\..\\nyquist~A~A.zip g:\\user\\rbd\\music\\web\\nyquist\\nyqsrc~A~A.zip~%"
            *maj* *min* *maj* *min*)
    (format outf "call restore.bat~%")
    (close outf)))

(edit-music-software-html)
(make-install-bat)


(exit)

