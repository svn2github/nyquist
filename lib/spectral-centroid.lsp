;; spectral-centroid.sal -- functions to simplify computing
;;   spectral centroid
;;
;; Roger B. Dannenberg and Gus Xia
;; Feb 2013

;; API:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set sc-number = sc-next(sc-obj)
;;
;; sc-next() fetches the next spectrum and compute the specturm centroid.
;;
;; sc-obj is a spectral-centroid object returned by sc-init-internal().
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sc-obj =  sc-init-internal (sa-obj)
; This function takes in a spectral analysis object and  creates a spectral centroid object.
; sa-obj is the spectral analysis object
; sc-obj is the spectral centroid object



;;define the class of spectral centroid objects
(setf sc-class (send class :new '(sa-obj)))

; define the next function of a sc-obj
(send sc-class :answer :next '() '(
  (let (frame mag)
    (setf frame (send sa-obj :next))
    (cond (frame
           (setf mag (sa-magnitude frame))
           (sc-centroid (sa-get-bin-width sa-obj) mag))
          (t nil)))))


(send sc-class :answer :isnew '(obj)
      '((setf sa-obj obj)))


(defun sc-init-internal (sa-obj)
  (send sc-class :new sa-obj))


(defun sc-next (sc-obj)
  (send sc-obj :next))

 
