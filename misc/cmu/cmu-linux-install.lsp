;; edit music.software.html and copy zip file to web

;; where is the html page for nyquist downloads?:
(setf ny-web-path "/afs/cs/project/music/web/")
;; where are the actual nyquist download files:
(setf ny-web-bin-path "/afs/cs/project/music/web/nyquist/")

(defun ny-web (file) (strcat ny-web-path file))

;; verbose version of SYSTEM
;;
(defun vsystem (cmd)
  (format t "system command: ~A~%" cmd)
  (system cmd))

(defun linux-edit-music-software-html ()
  (let (inf outf input i prefix postfix postfix1 postfix2)
    (setf inf (open (ny-web "music.software.html")))
    (setf outf (open (ny-web "new.html") :direction :output))
    (format t "Major version number (e.g. 2): ")
    (setf *maj* (read))
    (format t "Minor version number (e.g. 27): ")
    (setf *min* (read))
    
    ;; find "source code for Linux"
    (print "source code for Linux")
    (setf input (read-line inf))
    (while (not (setf i (string-search "source code for Linux" input)))
      (format outf "~A~%" input)
      (setf input (read-line inf)))
    ;; now we have the line with the reference to the zip file and
    ;; also the text description, but we have to find the text to change
    ;; (ignore the variable i, it's the wrong location to change)
    (setf i (string-search "nyquist/nyquist" input))
    (setf prefix (subseq input 0 (+ i 15)))
    (setf postfix (subseq input (+ i 15)))
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

    (vsystem (strcat "rm " (ny-web "music.software.html")))
    (vsystem (strcat "mv " (ny-web "new.html") " " 
                    (ny-web "music.software.html")))
    (vsystem (format nil "cp ../nyquist.zip ~Anyquist~A~A.zip" 
                    ny-web-bin-path *maj* *min*))
    (vsystem (format nil "mv ../release.pac nyquist~A~A.pac" *maj* *min*))

   ))

(linux-edit-music-software-html)


(exit)

