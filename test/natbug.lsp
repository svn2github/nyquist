(defun n2b2 ()
  (scale 2 (sim
            (at .3 (stretch .2    (osc (hz-to-step 1616))))
            (at .5 (stretch .3    (osc (hz-to-step 1611))))
            (at .6 (stretch .2   (osc (hz-to-step 1605))))
            (at .8 (stretch .5  (osc (hz-to-step 1600)))))))

(defun n2b2R ()
  (scale 2 (sim
            (at .3 (stretch .2    (osc (hz-to-step 1600))))
            (at .5 (stretch .3    (osc (hz-to-step 1605))))
            (at .6 (stretch .2   (osc (hz-to-step 1611))))
            (at .8 (stretch .5  (osc (hz-to-step 1616)))))))


(defun hph1b ()
  (seq
   (n2b2)
   (s-rest .02)
   (stretch .2
            (at .5
                (sim
                 (osc (hz-to-step 200))
                 (osc (hz-to-step 206))
                 (s-rest .1))))))
           
(defun ply ()
  (scale .1
         (sim
          (hph1b)
          (at .9
              (n2b2R)))))

(defun Plystrm ()
  (scale .1
         (sim
          (seqrep  (i 10) (ply))
          (seq
           (stretch 2
                    (hph1b))
           (s-rest .01)
           (stretch 2
                    (n2b2))
           (stretch 3 (n2b2))))))

(defun drum ()
  (scale .2
         (stretch .5
                  (sim
                   (pan (Plystrm)1)
                   (at .4 (pan(Plystrm)0))
                   (at 1.5 (pan (Plystrm)1))
                   (at 3.5 (pan (Plystrm)0))))))

(play (scale 10.5  (drum)))

(defun n2b2 ()
  (scale 2 (sim
            (at .3 (stretch .2    (osc (hz-to-step 1616))))
            (at .5 (stretch .3    (osc (hz-to-step 1611))))
            (at .6 (stretch .2   (osc (hz-to-step 1605))))
            (at .8 (stretch .5  (osc (hz-to-step 1600)))))))

(defun n2b2R ()
  (scale 2 (sim
            (at .3 (stretch .2    (osc (hz-to-step 1600))))
            (at .5 (stretch .3    (osc (hz-to-step 1605))))
            (at .6 (stretch .2   (osc (hz-to-step 1611))))
            (at .8 (stretch .5  (osc (hz-to-step 1616)))))))


(defun hph1b ()
  (seq
   (n2b2)
   (s-rest .02)
   (stretch .2
           (at .5
               (sim
                (osc (hz-to-step 200))
                (osc (hz-to-step 206))
                (s-rest .1))))))
           
(defun ply ()
  (scale .1
         (sim
          (hph1b)
          (at .9
              (n2b2R)))))

(defun Plystrm ()
 (scale .1
        (sim
         (seqrep  (i 10) (ply))
         (seq
          (stretch 2
                   (hph1b))
          (s-rest .01)
          (stretch 2
                   (n2b2))
          (stretch 3 (n2b2))))))

(defun drum ()
 (scale .2
        (stretch .5
                 (sim
                  (pan (Plystrm)1)
                  (at .4 (pan(Plystrm)0))
                  (at 1.5 (pan (Plystrm)1))
                  (at 3.5 (pan (Plystrm)0))))))

(play (scale 10.5  (drum)))

