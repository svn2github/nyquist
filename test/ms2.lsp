;This causes a memory allocate/free bug, possibly related to multiseq:

(load "rbd")
(load "alex")
(play (seqrep (i 10) (scale 0.5 (ster (osc 46 0.5) (* i 0.1)))))
(play (seqrep (i 10) (scale 0.5 (ster (osc 46 0.5) (* i 0.1)))))
(play (seqrep (i 10) (scale 0.5 (ster (osc 46 0.5) (* i 0.1)))))
