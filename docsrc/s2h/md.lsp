;; md.lsp -- metadoc extensions to s2h
;;
;; load this to add metadoc output

;; first arg to format: if *omit* is false, we return the output file, *mdoutf*
;;    if *omit* is true, we return nil which "tricks" format into not outputting
;;
(defun mdoutf () (and (not *omit*) *mdoutf*))

(defun md-start ()
  (format (mdoutf) "% \\Huge{\\textbf{~A}}~%" *doc-title*)
  (format (mdoutf) "% \\textbf{\\normalsize{~A}}~%" *doc-version*)
  (format (mdoutf) "% \\textbf{\\large{~A}}~%~%" *doc-author*))

(defun md-majorheading (heading)
  nil) ;; ignore majorheadings -- they are titles handled differently

(defun md-begin-code ()
  (format (mdoutf) "`"))

(defun md-end-code ()
  (format (mdoutf) "`"))

(defun md-begin-quotation ()
  (format (mdoutf) "<begin quotation>"))

(defun md-end-quotation ()
  (format (mdoutf) "<end quotation>"))

(defun md-begin-normal ()
  (format (mdoutf) "<begin normal>"))

(defun md-end-normal ()
  (format (mdoutf) "<end normal>"))

(defun md-begin-tt ()
  (format (mdoutf) "`"))

(defun md-end-tt ()
  (format (mdoutf) "`"))

(defun md-write-index (str)
  (format (mdoutf) "\\index{~A}" str))

(defun md-generate-index-entry (entry target)
  (format (mdoutf) "<index-entry ~A ~A>" entry target))

(defun md-generate-index ()
  (format (mdoutf) "<index goes here?>"))

(defun md-generate-toc ()
  (format (mdoutf) "<table of content>"))

(defun md-generate-toc-2 ()
  (format (mdoutf) "<table of content 2>"))

(defun md-write-indexsecondary (str n)
  (format (mdoutf) "<indexsecondary ~A ~A>" str n))

(defun md-begin-style (style)
  (cond ((equal style "i")
         ; (format t "md-begin-style-* ")
         (format (mdoutf) "*"))
        ((equal style "b")
         (cond ((and (not *need-version*) (not *need-author*))
                ; (format t "md-begin-style-** ")
                (format (mdoutf) "**"))))
        (t
         (format (mdoutf) "<style-begin ~A>" style))))

(defun md-end-style (style)
  (cond ((equal style "i")
         (format (mdoutf) "*"))
        ((equal style "b")
         (cond ((and (not *need-version*) (not *need-author*))
                (format (mdoutf) "**"))))
        (t
         (format (mdoutf) "<style-end ~A>" style))))


(defun md-begin-bold-italic ()
  (format (mdoutf) "***"))

(defun md-end-bold-italic ()
  (format (mdoutf) "***"))

(defun md-write-dd ()
  (format (mdoutf) "<dd>"))

(defun md-begin-write-description ()
  (format (mdoutf) "<write-description-begin>"))

(defun md-end-write-description ()
  (format (mdoutf) "<write-description-end>"))

(defun md-begin-write-fdescription ()
  (format (mdoutf) "<write-fdescription-begin>"))

(defun md-end-write-fdescription ()
  (format (mdoutf) "<write-fdescription-end>"))

(defun md-begin-write-pdescription ()
  (format (mdoutf) "<write-pdescription-begin>"))

(defun md-end-write-pdescription ()
  (format (mdoutf) "<write-pdescription-end>"))

(defun md-begin-majorheading ()
  (format (mdoutf) "% "))

(defun md-end-majorheading ()
  (format (mdoutf) "~%"))

(defun md-begin-h2 ()
  (format (mdoutf) "<h2-begin>"))

(defun md-end-h2 ()
  (format (mdoutf) "<h2-end>"))

(defun md-begin-h3 ()
  (format (mdoutf) "<h3-begin>"))

(defun md-end-h3 ()
  (format (mdoutf) "<h3-end>"))

(defun md-begin-h4 ()
  (format (mdoutf) "<h4-begin>"))

(defun md-end-h4 ()
  (format (mdoutf) "<h4-end>"))

(defun md-write-paragraph (n body)
  (format (mdoutf) "<paragraph-begin ~A>~A<paragraph-end>~%" n body))

(defun md-write-subsection (n body)
  (format (mdoutf) "~%### ~A~%" body))

(defun md-write-section (n body)
  (format (mdoutf) "~%## ~A~%" body))

(defun md-finish-chapter ()
  nil
  ; (format (mdoutf) "<chapter-end>~%")
  )


(defun md-write-chapter (body)
  (format (mdoutf) "~%# ~A~%" body))

(defun md-begin-detail ()
  (format (mdoutf) "<begin detail>"))

(defun md-end-detail ()
  (format (mdoutf) "<end detail>"))

(defun md-appendix (n body)
  (format (mdoutf) "<appendix-begin ~A>~A<appendix-end>~%" n body))

(defun md-break-dt ()
  (format (mdoutf) "<break-dt>~%"))

(defun md-list-paragraph ()
  (format (mdoutf) "~%* "))

(defun md-description-paragraph ()
  (format (mdoutf) "<description-dt-begin>"))

(defun md-paragraph ()
  (format (mdoutf) "~%~%"))

(defun md-linebreak ()
  (format (mdoutf) "<linebreak>"))

(defun md-write-cite (link)
  (format (mdoutf) "<cite ~A>" link))

(defun md-write-ref (ref)
  (format (mdoutf) "<ref ~A>" ref))

(defun md-begin-example ()
  (format (mdoutf) "~%~~~~~~~~~~~~"))

(defun md-end-example ()
  (format (mdoutf) "~~~~~~~~~~~~"))

(defun md-begin-enumerate ()
  (format (mdoutf) "<enumerate-begin>"))

(defun md-end-enumerate ()
  (format (mdoutf) "<enumerate-end>"))

(defun md-begin-itemize ()
  (format (mdoutf) "~%* "))

(defun md-end-itemize ()
  (format (mdoutf) "~%"))

(defun md-end-write-format ()
  (format (mdoutf) "<write-format-end>"))

(defun md-begin-blockquote ()
  (format (mdoutf) "<blockquote-begin>"))

(defun md-end-blockquote ()
  (format (mdoutf) "<blockquote-end>"))

(defun md-begin-figure ()
  (format (mdoutf) "<figure-begin>"))

(defun md-end-figure ()
  (format (mdoutf) "<figure-end>"))

(defun md-write-footnote (name num)
  (format (mdoutf) "<footnote-begin ~A ~A>" name num))

(defun md-footnote-end ()
  (format (mdoutf) "<footnote-end>"))

(defun md-write-fillcaption (n)
  (format (mdoutf) "<fillcaption-begin ~A>" n))

(defun md-begin-underline ()
  (format (mdoutf) "<underline-begin>"))

(defun md-end-underline ()
  (format (mdoutf) "<underline-end>"))
