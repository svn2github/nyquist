(defun reverb (x time) 
  (multichan-expand #'reverb-mono x time))

(defun reverb-mono (ga irevfactor)
  (let (sr ilowpass idel ihz icsc acomball allp1 allp2 allp3 alow allp4 allp5
        arevout)
    (setf sr (snd-srate ga))

    (setf ilowpass 9000.000)       ; frequency of lowpass filter

    (setf idel (list ; list of frequency/delay values
                    (/ 1237.000 sr) (/  1381.000 sr) (/ 1607.000 sr)
                    (/ 1777.000 sr) (/ 1949.000 sr) (/  2063.000 sr)
                    (/ 307.000 sr) (/ 97.000 sr) (/ 71.000 sr)
                    (/ 53.000 sr) (/ 47.000 sr) (/ 37.000 sr)
                    (/ 31.000 sr)))
    ; Nyquist's comb filter uses Hz rather than delay as parameter,
    ; so take reciprocals to get Hz:
    (setf ihz (mapcar #'/ idel))

    (setf icsc (list ; list of delay times
                    (* irevfactor 0.822) (* irevfactor 0.802)
                    (* irevfactor 0.773) (* irevfactor 0.753)
                    (* irevfactor 0.753) (* irevfactor 0.753)
                    (* irevfactor 0.7)))

    (setf acomball (sum
                        (comb ga (nth 0 icsc) (nth 0 ihz))
                        (comb ga (nth 1 icsc) (nth 1 ihz))
                        (comb ga (nth 2 icsc) (nth 2 ihz))
                        (comb ga (nth 3 icsc) (nth 3 ihz))
                        (comb ga (nth 4 icsc) (nth 4 ihz))
                        (comb ga (nth 5 icsc) (nth 5 ihz))))

    (setf allp1 (alpass acomball (nth 6 icsc) (nth 6 ihz)))
    (setf allp2 (alpass allp1 (nth 6 icsc) (nth 7 ihz)))
    (setf allp3 (alpass allp2 (nth 6 icsc) (nth 8 ihz)))
    (setf alow  (lp allp3 ilowpass))
    (setf allp4 (alpass alow (nth 6 icsc) (nth 9 ihz)))
    (setf allp5 (alpass allp4 (nth 6 icsc) (nth 11 ihz)))

    allp5
    ; acomball
    ))

