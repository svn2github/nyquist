;; mac-to-win -- convert text files in nyquist project

;; NOTE: this might work to convert any source tree to the local newline
;; convention, but it was written to run under windows and convert mac
;; sources to windows so that I could run windiff on the files. -RBD

;; files.txt is a listing of all the component files

(defun mac-to-win (output-path)
  (let (files filename)
    (setf files (open "files.txt" :direction :input))
    (while (setf filename (read-line files))
      (process-file filename output-path))
    (close files)))
    

(defun process-file (filename output-path)
  (let ((filetype (char filename 0)))
    (cond ((eq filetype #\a)
           (setf filename (subseq filename 2))
           (convert-file filename output-path)))))
           
(defun convert-file (filename output-path)
  (let (infile outfile outfilename line)
    (setf outfilename (strcat output-path filename))
    (setf infile (open filename :direction :input))
    (setf outfile (open outfilename :direction :output))
    (cond ((null infile)
           (format t "Could not open ~A~%" filename))
          ((null outfile)
           (format t "Could not open ~A~%" outfilename))
          (t (format t "~A~%" filename)
             (while (setf line (read-line infile))
                    (format outfile "~A~%" line))
             (close infile)
             (close outfile)))))

(defun convert-mac-to-win ()
  (setdir "d:/rbd/icm_nyquist")
  (mac-to-win "d:/rbd/icm_nyquist_win/"))

