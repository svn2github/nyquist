;; extensions.lsp -- load extensions' autoload.lsp files from
;;    all sub-directories

(defun load-extensions ()
  (let* ((extpath (strcat (current-path)))
         (content (listdir extpath))
         (cwd (setdir "."))
         isdir inf)
    ;; make the current directory be the extpath
    (setdir extpath nil)

    ;; find and process each directory in extpath
    (dolist (dirname content)
      ;; test each dirname to see if it is a directory by using setdir
      ;; ignore ., .., and other hidden files
      (setf isdir (and (not (string-equal (subseq dirname 0 1) "."))
                       (setdir dirname nil))) ;; do not print error if any
      (cond (isdir ;; we found a directory
             (setf inf (open "autoload.lsp"))
             (setdir "..") ;; go back to lib directory
             (cond (inf
                    (close inf) ;; it was only open to check for it
                    (format t "Loading extension ~A ..." dirname)
                    (setdir dirname nil)
                    (load "autoload.lsp" :verbose nil)
                    (setdir "..")
                    (format t " done.\n"))))))

    ;; restore the original current working directory
    (setdir cwd nil)))


;; warning: you MUST invoke the function from here so that (current-path)
;; will return the directory of extensions.lsp (this file) -- this is the
;; path where we look for extensions. If you move this call to another file
;; in another directory, it will look there for extensions!
(load-extensions)
