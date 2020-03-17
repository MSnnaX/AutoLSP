;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The Main Key                                                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:CC (/ cir) 
  (setvar "cmdecho" 1)
  (command "circle" pause pause)
  (setvar "cmdecho" 0)
  (setq cir (ssget "l"))
  (cm cir)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:CM (/ d i c1 sel nam tab cen ra lt cel lay cur len1 len2 lanam j latab p1 p2) 
  (setierr)
  (if 
    (not 
      (setq sel (ssget "_i" 
                       '((-4 . "<OR")
                         (0 . "CIRCLE")
                         (0 . "ARC")
                         (-4 . "OR>")
                        )
                ) ;_ end of ssget
      ) ;_ end of setq
    ) ;_ end of not
    (progn 
      (setq Sel (ssget 
                  '((-4 . "<OR")
                    (0 . "CIRCLE")
                    (0 . "ARC")
                    (-4 . "OR>")
                   )
                ) ;_ end of ssget
      ) ;_ end of setq
    ) ;end progn
  ) ;end if
  (command "ucs" "w")
  (setq d   (getvar "dimscale")
        cur (getvar "clayer")
  ) ;end setq
  (if (= d 0) 
    (setvar "dimscale" 1)
  ) ;end if
  (setq i 0)
  (if sel 
    (repeat (sslength sel) 
      (setq nam (ssname sel i)
            tab (entget nam)
            cen (cdr (assoc 10 tab)) ;Center
            px  (car cen)
            py  (cadr cen)
            c1  (list (+ 0.005 px) (+ 0.005 py))
            i   (1+ i)
            ra  (cdr (assoc 40 tab)) ;radius
            lay (cdr (assoc 8 tab)) ;layer
            lt  (getvar "ltscale")
            cel (/ (* ra 0.5) lt)
            dsc (/ 1 (getvar "dimscale"))
            nam (list nam cen)
            j   0
      ) ;_ end of setq
      (setvar "celtscale" cel)
      (setvar "clayer" lay)
      (setvar "dimcen" (* ra 1.2 dsc))
      (setq fl (cdr (assoc 70 (tblsearch "LAYER" lay))))
      (if (= fl 4)  ;layer Locked
        (command "layer" "u" lay)
      )
      (COMMAND "DIMCENTER" nam)
      (setq la   (ssget "c" cen c1 '((0 . "LINE")))
            len1 (* ra 2.4)
      ) ;end setq
      (repeat (sslength la) 
        (setq lanam (ssname la j)
              latab (entget lanam)
              p1    (cdr (assoc 10 latab))
              p2    (cdr (assoc 11 latab))
              len2  (distance p1 p2)
              j     (+ j 1)
        ) ;end setq
        (if (> (abs (- len1 len2)) 0.001) 
          (progn 
            (setq la (ssdel lanam la)
                  j  (- j 1)
            ) ;end setq
          ) ;end progn
        ) ;_ end of if
      ) ;end repeat
      (ce la)
      (if (= fl 4)  ;layer Locked
        (command "layer" "lo" lay)
      )
    ) ;end repeat
    (alert "\nNo Selection!")
  ) ;end if
  (setvar "clayer" cur)
  (command "ucs" "p")
  (Reerr)
  (PRINC)
) ;_ end of DEFUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:D0 (/ di) 
  (ch_precision 0)
) ;_ end of defun

(defun c:D1 (/) 
  (ch_precision 1)
) ;_ end of defun

(defun c:D2 (/) 
  (ch_precision 2)
) ;_ end of defun

(defun c:D3 (/) 
  (ch_precision 3)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DA () 
  (layer-41)
  (setvar "dimtad" 1)
  (command "DIMANGULAR")
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DAL () 
  (layer-41)
  (scale)
  (command "DIMALIGNED")
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DB () 
  (layer-41)
  (setvar "dimtad" 0)
  (command "dimbaseline")
  (princ)
)

(defun c:DBS (/ dim nam tab pt1 vla-obj sc sc_dl hei ss nam1 lss) 
  (setierr)
  (command "ucs" "w")
  (layer-41)
  (while 
    (if nam 
      (if (= (get-type nam) "DIMENSION") 
        nil
        (progn 
          (alert "\nType error!")
          t
        ) ;_ end of progn
      ) ;_ end of if
      t
    ) ;_ end of if
    (while 
      (not 
        (setq dim (entsel "\nSelect a Dimension:")
              nam (car dim)
        ) ;_ end of setq
      ) ;_ end of not
      (alert "\nNo selection!")
    ) ;_ end of while
  ) ;_ end of while
  (setq tab (entget nam)
        pt1 (cdr (assoc 10 tab))
  ) ;_ end of setq
  (command "ucs" "n" pt1)
  (vl-load-com)
  (setq vla-obj (vlax-ename->vla-object nam)
        sc_dl   (vla-get-LinearScaleFactor vla-obj)
        sc      (vla-get-ScaleFactor vla-obj)
        hei     (* sc (vla-get-TextHeight vla-obj))
  ) ;_ end of setq
  (setvar "dimscale" (/ hei (getvar "dimtxt")))
  (command "zoom" "e")
  (command "dimlinear" "0,0" "0,1.2345" "1,1.2345")
  (setq lss (ssget "l" '((0 . "DIMENSION"))))
  (if (not (= (sslength lss) 1)) 
    (progn 
      (alert "\nFail!")
      (exit)
    ) ;_ end of progn
  ) ;_ end of if
  (setq lnam (ssname lss 0)
        ltab (entget lnam)
        lv   (cdr (assoc 42 ltab))
        dlf  (getvar "dimlfac")
        lvc  (* 1.2345 dlf)
  ) ;_ end of setq
  (if (equal lv lvc 0.001) 
    (command "erase" lss "")
    (alert "\nFail!")
  ) ;_ end of if
  (command "zoom" "p")
  (if (= sc_dl 1) 
    (setvar "dimlfac" 1)
    (progn 
      (command "change" dim "" "p" "c" "cyan" "")
      (setvar "dimlfac" sc_dl)
      (alert (strcat "\nCurrent dimlfac is " (rtos sc_dl 2 2)))
    ) ;_ end of progn
  ) ;_ end of if
  (setvar "osmode" 679)
  (command "_dimbaseline" dim)
  (reerr)
  (princ)
) ;_ end of defun

(defun c:DOS (/ nam namtyp tab pt1 hei vla-obj sc sc_dl) 
  (setierr)
  (command "ucs" "w")
  (setvar "osmode" 679)
  (layer-41)
  (scale)
  (setq nam nil)
  (while (= nam nil) 
         (while (= nam nil) 
                (setq nam (car (entsel "\nSelect a Dimension:")))
         ) ;_ end of while
         (setq namtyp (cdr (assoc 0 (entget nam)))) ;_ end of setq
         (if (not (= namtyp "DIMENSION")) 
           (setq nam nil)
         ) ;_ end of if
  ) ;_ end of while
  (setq tab (entget nam)
        pt1 (cdr (assoc 10 tab))
  ) ;_ end of setq
  (command "ucs" "n" pt1)
  (vl-load-com)
  (setq vla-obj (vlax-ename->vla-object nam)
        sc_dl   (vla-get-LinearScaleFactor vla-obj)
        sc      (vla-get-ScaleFactor vla-obj)
        hei     (* sc (vla-get-TextHeight vla-obj))
  ) ;_ end of setq
  (if (= sc_dl 1) 
    (setvar "dimlfac" 1)
    (progn 
      (setvar "dimlfac" sc_dl)
      (alert (strcat "\nCurrent dimlfac is " (rtos sc_dl 2 2)))
    ) ;_ end of progn
  ) ;_ end of if
  (setvar "dimscale" (/ hei (getvar "dimtxt")))
  (setvar "cmdecho" 1)
  (setvar "dimtad" 0)
  (command "_dimordinate" pause pause)
  (setvar "cmdecho" 0)
  (setvar "cecolor" "bylayer")
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DC (/ i ss_li name p0 p1 p2 p1x p2x p1y p2y tab typ ss i count na ang dis 
             angle_count tp1x tp2x tp1y tp2y ta ltyp) 
  (setierr)
  (command "ucs" "w")
  (layer-41)
  (scale)
  (setvar "dimtad" 1)
  (setq lfac (getvar "DIMLFAC"))
  (prompt "\nSelect chamfer line:")
  (setq ss_li       (ssget '((0 . "LINE")))
        i           0
        angle_count 0
  ) ;_ end of setq
  (if ss_li 
    (progn 
      (repeat (sslength ss_li) 
        (setq name0 (ssname ss_li i)
              tab   (entget name0)
              p1    (cdr (assoc 10 tab))
              p1x   (car p1)
              p1y   (cadr p1)
              p2    (cdr (assoc 11 tab))
              p2x   (car p2)
              p2y   (cadr p2)
              a     (abs (- p1x p2x))
              b     (abs (- p1y p2y))
              i     (1+ i)
        ) ;end setq
        (if (< (abs (- a b)) 0.005) 
          (setq angle_count (1+ angle_count)
                tp1x        p1x
                tp1y        p1y
                tp2x        p2x
                tp2y        p2y
                ta          a
                name        name0
          ) ;_ end of setq
        ) ;_ end of if
      ) ;_ end of repeat
      (if (= angle_count 0) 
        (progn 
          (alert "Error:\nMust chamfer line!")
          (exit)
        ) ;end progn
        (if (> angle_count 1) 
          (progn 
            (alert 
              "Error:\nNum of chamfer line > 1!"
            ) ;_ end of alert
            (exit)
          ) ;end progn
        ) ;_ end of if
      ) ;end if
      (setq p0x (/ (+ tp1x tp2x) 2)
            p0y (/ (+ tp1y tp2y) 2)
            p0  (list p0x p0y)
      ) ;end setq
      (SETQ S1 (GETVAR "DIMSCALE")
            tx (getvar "dimtxt")
            O  (GETVAR "ORTHOMODE")
      ) ;end setq
      (if (= s1 0) 
        (progn 
          (alert "Dimsclae=0\nNow set to 2.5!")
          (setvar "Dimscale" 2.5)
          (setq s1 2.5)
        ) ;end progn
      ) ;end if
      (SETQ S    (* S1 tx)
            a    (* ta lfac)
            a    (RtoS a 2 1)
            ltyp (cdr (assoc 6 (entget name)))
      ) ;_ end of SETQ
      (if (not ltyp) 
        (setq ltyp "BYLAYER")
      ) ;_ end of if
      (command "zoom" "e")
      (bound-c-line name)
      (setq p1    pu-p1
            p2    pu-p2
            pu-p1 nil
            pu-p2 nil
      ) ;_ end of setq
      (if 
        (or (= ltyp "BYLAYER") 
            (= (strcase ltyp) "BYBLOCK")
        ) ;_ end of or
        (setq ss (ssget "c" 
                        p1
                        p2
                        (list 
                          (cons 0 "LINE")
                          (cons -4 "<OR")
                          (CONS 6 "BYLAYER")
                          (CONS 6 "BYBLOCK")
                          (CONS -4 "OR>")
                        ) ;_ end of list
                 ) ;_ end of ssget
        ) ;_ end of setq
        (setq ss (ssget "c" 
                        p1
                        p2
                        (list 
                          (cons 0 "LINE")
                          (cons 6 ltyp)
                        ) ;_ end of list
                 ) ;_ end of ssget
        ) ;_ end of setq
      ) ;_ end of if
      (setq i     0
            count 0
      ) ;_ end of setq
      (repeat (sslength ss) 
        (setq na  (ssname ss i)
              tab (entget na)
              p1  (cdr (assoc 10 tab))
              p2  (cdr (assoc 11 tab))
              ang (atoi (rtos (* 180 (/ (angle p1 p2) pi)) 2 0))
              dis (/ (distance p1 p2) (sqrt 2))
              i   (1+ i)
        ) ;_ end of setq
        (if 
          (and (/= 0 (rem (/ ang 45) 2)) 
               (< (abs (- dis (atof a))) 0.01)
          ) ;_ end of and
          (setq count (1+ count))
        ) ;_ end of if
      ) ;_ end of repeat
      (command "zoom" "p")
      (if (= count 1) 
        (command "leader" P0 PAUSE "" (strcat "C" a) "")
        (command "leader" 
                 P0
                 PAUSE
                 ""
                 (strcat (itoa count) "-C" a)
                 ""
        ) ;_ end of command
      ) ;_ end of if
    ) ;_ end of progn
    (alert "\nNo selection!")
  ) ;_ end of if
  (setvar "cecolor" "bylayer")
  (command "ucs" "p")
  (Reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DL (/ os) 
  (scale)
  (layer-41)
  (setvar "osmode" 679)
  (setvar "dimtad" 1)
  (command "dimlinear" pause pause pause)
  (setvar "cecolor" "bylayer")
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DO () 
  (setierr)
  (scale)
  (layer-41)
  (setvar "osmode" 679)
  (setvar "dimtad" 0)
  (command "_dimordinate" pause pause)
  (setvar "cecolor" "bylayer")
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DS () 
  (command "dimSCALE")
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DT (/ ss) 
  (setierr)
  (if (setq ss (ssget '((0 . "DIMENSION")))) 
    (if (is-ss-unlocked ss) 
      (progn 
        (setq up  (rtos (getREAL "\nEnter Upper Value:") 2 3)
              low (rtos (- (getreal "\nEnter Lower Value:")) 2 3)
        ) ;_ end of setq
        (command "dimoverride" 
                 "dimdec"
                 "3"
                 "dimzin"
                 "8"
                 "dimtdec"
                 "3"
                 "dimtzin"
                 "8"
                 "dimtol"
                 "on"
                 "dimtfac"
                 "0.7"
                 "dimtp"
                 up
                 "dimtm"
                 low
                 ""
                 ss
                 ""
        ) ;_ end of command
      ) ;_ end of progn
      (alert "All locked!")
    ) ;_ end of if
    (alert "No Selected!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:DTS () 
  (setq val (getstring "Enter Tolerance Value {symmetrical}:"))
  (tole7 val val)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:FA (/ ca s en1 en2 p1 p2 p3 p4 tab1 tab2 p-int ca) 
  (setierr)
  (command "ucs" "w")
  (angset)
  (setq en1  (car (select-object "the fist line" "LINE"))
        en2  (car (select-object "the second line" "LINE"))
        la   (get-layer en1)
        o_la (getvar "clayer")
  ) ;_ end of setq
  (setvar "clayer" la)
  (setq tab1  (entget en1)
        p1    (cdr (assoc 10 tab1))
        p2    (cdr (assoc 11 tab1))
        tab2  (entget en2)
        p3    (cdr (assoc 10 tab2))
        p4    (cdr (assoc 11 tab2))
        p-int (inters p1 p2 p3 p4 nil)
        ca    (/ 
                (min 
                  (min (distance p-int p1) (distance p-int p2))
                  (min (distance p-int p3) (distance p-int p4))
                ) ;_ end of min
                2
              ) ;_ end of /
  ) ;_ end of setq
  (command "color" "red")
  (command "line" p-int (polar p-int (angle p-int p1) ca) "")
  (command "line" p-int (polar p-int (angle p-int p3) ca) "")
  (redraw en1 4)
  (redraw en2 4)
  (setvar "clayer" o_la)
  (setvar "cecolor" "bylayer")
  (command "ucs" "p")
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:FC () 
  (SCALE)
  (layer-41)
  (setvar "cmdecho" 1)
  (setvar "dimtad" 1)
  (COMMAND "_dimlinear" PAUSE PAUSE "T" "%%C<>" PAUSE)
  (setvar "cmdecho" 0)
  (setvar "cecolor" "bylayer")
  (PRINC)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:HD (/ str ob tb va b ename) 
  (setierr)
  (if (not (setq b (vl-bb-ref 'hz_d))) 
    (setq b "5")
  ) ;_ end of if
  (setq str (getreal (strcat "\nInput suffix 'DEP <" b ">':")))
  (if str 
    (setq str (rtos str 2 1))
    (setq str b)
  ) ;_ end of if
  (vl-bb-set 'hz_d str)
  (setq str (strcat "DEP" str))
  (while (not (setq ename (entsel))) 
         (alert "\nNo selection,go on...")
  ) ;_ end of while
  (if (not (is-entity-locked (car ename))) 
    (progn 
      (setq ob (car ename)
            tb (entget ob)
            va (cdr (assoc 1 tb))
      ) ;_ end of setq
      (if (= va "") 
        (setq va "<>")
      ) ;_ end of if
      (setq va (strcat va " " str))
      (change-value ob 1 va)
    ) ;_ end of progn
    (alert "Locked!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:HT (/ str ob tb va b ename) 
  (setierr)
  (setq str "THR")
  (while (not (setq ename (entsel))) 
         (alert "\nNo selection,go on...")
  ) ;_ end of while
  (if (not (is-entity-locked (car ename))) 
    (progn 
      (setq ob (car ename)
            tb (entget ob)
            va (cdr (assoc 1 tb))
      ) ;_ end of setq
      (if (= va "") 
        (setq va "<>")
      ) ;_ end of if
      (setq va (strcat va " " str))
      (change-value ob 1 va)
    ) ;_ end of progn
    (alert "Locked!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:HZ (/ str ob tb va b ename) 
  (setierr)
  (if (not (setq b (vl-bb-ref 'hz))) 
    (setq b "ONE WALL")
  ) ;_ end of if
  (setq str (STRCASE 
              (getstring 
                (strcat 
                  "\nBACKSIDE(B)/BOTHSIDE(S)/EVENLY SPACED(E)/ONE WALL(O)/THR ONE WALL(T)/<"
                  b
                  ">:"
                ) ;_ end of strcat
              ) ;_ end of getstring
            ) ;_ end of STRCASE
  ) ;_ end of setq
  (cond 
    ((= str "") (setq str b))
    ((= str "B") (setq str "\nBACK"))
    ((= str "S") (setq str "\nBOTHSIDE"))
    ((= str "E") (setq str "\nEVENLY SPACED"))
    ((= str "O") (setq str "\nONE WALL"))
    ((= str "T") (setq str "THR\nONE WALL"))
    (T STR)
  ) ;_ end of cond
  (vl-bb-set 'hz str)
  (while (not (setq ename (entsel))) 
         (alert "\nNo selection,go on...")
  ) ;_ end of while
  (if (not (is-entity-locked (car ename))) 
    (progn 
      (setq ob (car ename)
            tb (entget ob)
            va (cdr (assoc 1 tb))
      ) ;_ end of setq
      (if (= va "") 
        (setq va "<>")
      ) ;_ end of if
      (setq va (strcat va " " str))
      (setq tb (subst (cons 1 va) (assoc 1 tb) tb))
      (entmod tb)
      (entupd ob)
    ) ;_ end of progn
    (alert "Locked!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:LS (/ part) 
  (setq part (strcase (getstring "Part<A>:")))
  (if (= part "") 
    (setq part "A")
  ) ;_ end of if
  (command "leader" 
           pause
           pause
           ""
           (strcat "SEE DETAIL " part)
           ""
  ) ;_ end of command
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:M0 () 
  (lt-change "Lessen" 0.5)
  (princ)
) ;_ end of defun

(defun c:M2 () 
  (lt-change "Augment" 2)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:MC (/) 
  (setierr)
  (command "ucs" "w")
  (angset)
  (setq line1 (car (select-object "the fist line" "LINE"))
        tab1  (entget line1)
        p1    (cdr (assoc 10 tab1))
        p2    (cdr (assoc 11 tab1))
        ang1  (angle p1 p2)
        line2 (car (select-object "the second line" "LINE"))
        tab2  (entget line2)
        p3    (cdr (assoc 10 tab2))
        p4    (cdr (assoc 11 tab2))
        ang2  (angle p3 p4)
  ) ;_ end of setq
  (if 
    (not 
      (or (equal (angtos ang1) (angtos ang2) 0.0001) 
          (equal (angtos (angle p2 p1)) (angtos ang2) 0.0001)
      ) ;_ end of or
    ) ;_ end of not
    (progn 
      (alert "No parallel!")
      (redraw line1 4)
      (redraw line2 4)
      (exit)
    ) ;_ end of progn
  ) ;_ end of if
  (setq pt-int (inters p1 (polar p1 (+ ang1 (/ pi 2)) 1) p3 p4 nil)
        dis    (distance p1 pt-int)
  ) ;_ end of setq
  (command "offset" (/ dis 2) line1 p3 "")
  (cc (ssget "l"))
  (redraw line1 4)
  (redraw line2 4)
  (command "ucs" "p")
  (Reerr)
  (princ)
) ;_ end of defun mc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:PCD (/ NAM CIR STR TOH TAD OFL) 
  (setierr)
  (layer-41)
  (command "ucs" "w")
  (SETQ CIR (select-object-two "circle" "CIRCLE" "arc" "ARC")
        nam (car cir)
        str (strcat "P.C.D. %%c" "<>")
        toh (getvar "dimtoh")
        tad (getvar "dimtad")
        ofl (getvar "dimtofl")
  )
  (command "_dimtoh" 
           "on"
           "_dimtad"
           "1"
           "_dimtofl"
           "on"
           "dimpost"
           STR
  )
  (scale)
  (COMMAND "DIMDIAMETER" CIR PAUSE)
  (setvar "cecolor" "bylayer")
  (command "_dimtoh" 
           toh
           "_dimtad"
           tad
           "_dimtofl"
           ofl
           "dimpost"
           "."
  )
  (redraw nam 4)
  (command "ucs" "p")
  (Reerr)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Q1 () 
  (sub-qz "1-")
) ;_ end of defun

(defun c:Q2 () 
  (sub-qz "2-")
) ;_ end of defun

(defun c:Q3 () 
  (sub-qz "3-")
) ;_ end of defun

(defun c:Q4 () 
  (sub-qz "4-")
) ;_ end of defun

(defun c:Q5 () 
  (sub-qz "5-")
) ;_ end of defun 

(defun c:Q6 () 
  (sub-qz "6-")
) ;_ end of defun

(defun c:Q7 () 
  (sub-qz "7-")
) ;_ end of defun 

(defun c:Q8 () 
  (sub-qz "8-")
) ;_ end of defun

(defun c:Q9 () 
  (sub-qz "9-")
) ;_ end of defun 

(defun c:Q10 () 
  (sub-qz "10-")
) ;_ end of defun

(defun c:Q11 () 
  (sub-qz "11-")
) ;_ end of defun 

(defun c:Q12 () 
  (sub-qz "12-")
) ;_ end of defun 

(defun c:Q13 () 
  (sub-qz "13-")
) ;_ end of defun

(defun c:Q14 () 
  (sub-qz "14-")
) ;_ end of defun

(defun c:QZ (/ str ob tb va b ename ss i) 
  (setierr)
  (if 
    (NOT 
      (setq ss (ssget "i" 
                      '((-4 . "<OR")
                        (0 . "TEXT")
                        (0 . "MTEXT")
                        (0 . "DIMENSION")
                        (-4 . "OR>")
                       )
               ) ;_ end of ssget
      ) ;_ end of setq
    ) ;_ end of NOT
    (setq ss (ssget 
               '((-4 . "<OR")
                 (0 . "TEXT")
                 (0 . "MTEXT")
                 (0 . "DIMENSION")
                 (-4 . "OR>")
                )
             ) ;_ end of ssget
    ) ;_ end of setq
  ) ;_ end of if
  (if ss 
    (if (is-ss-unlocked ss) 
      (progn 
        (if (setq b (vl-bb-ref 'qz)) 
          (setq str (getstring (strcat "\nInput prefix<" b ">:")))
          (setq str (getstring "\nInput prefix<2->:")
                b   "2-"
          ) ;_ end of setq
        ) ;_ end of if
        (if (= (type (read str)) 'int) 
          (setq str (strcat str "-"))
        ) ;_ end of if
        (if (= str "") 
          (setq str b)
        ) ;_ end of if
        (vl-bb-set 'qz str)
        (setq i 0)
        (repeat (sslength ss) 
          (setq ob (ssname ss i)
                tb (entget ob)
                va (cdr (assoc 1 tb))
                i  (1+ i)
          ) ;_ end of setq
          (if (= va "") 
            (setq va "<>")
          ) ;_ end of if
          (setq va (strcat str va))
          (setq tb (subst (cons 1 va) (assoc 1 tb) tb))
          (entmod tb)
          (entupd ob)
        ) ;_ end of repeat
      ) ;_ end of progn
      (alert "All locked!")
    ) ;_ end of if
    (alert "No selection!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:REF (/ ob tb va) 
  (setierr)
  (setq ob (car (entsel))
        tb (entget ob)
        va (cdr (assoc 1 tb))
  ) ;_ end of setq
  (if (not (is-entity-locked ob)) 
    (progn 
      (if (= va "") 
        (setq va "<>")
      ) ;_ end of if
      (setq va (strcat "(" va ")"))
      (setq tb (subst (cons 1 va) (assoc 1 tb) tb))
      (entmod tb)
      (entupd ob)
    ) ;_ end of progn
    (alert "Locked!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:SL (/ DS S1 txt a sc te lfac pt1 pt2 pt3 pt4) 
  (setierr)
  (SETQ S1   (GETVAR "DIMSCALE")
        txt  (getvar "dimtxt")
        DS   (* S1 txt)
        lfac (rtos (/ 1 (getvar "dimlfac")) 2 0)
        a    (strcase (getstring "\nPart<A>:"))
        sc   (getint (strcat "\nScale<" lfac ">:"))
        pt1  (getpoint "\nInput insert point:")
        pt2  (list (+ (car pt1) (* ds 13)) 
                   (- (cadr pt1) (* ds 3))
             ) ;_ end of list
        pt3  (list (+ (car pt1) (* ds 2.6)) 
                   (/ (+ (cadr pt1) (cadr pt2)) 2)
             ) ;_ end of list
        pt4  (list (- (car pt2) (* ds 2.6)) 
                   (/ (+ (cadr pt1) (cadr pt2)) 2)
             ) ;_ end of list
  ) ;_ end of SETQ
  (if (or (= a "A") (= a "")) 
    (setq a "A")
  ) ;_ end of if
  (if (or (= sc (atoi lfac)) (= sc nil)) 
    (setq sc lfac)
    (setq sc (itoa sc))
  ) ;_ end of if
  (COMMAND "_mtext" 
           pt1
           "h"
           ds
           "j"
           "mc"
           pt2
           (strcat "DETAIL " a)
           (STRCAT "SCALE " sc ":1")
           ""
  ) ;_ end of COMMAND
  (COMMAND "LINE" PT3 PT4 "")
  (command "move" "c" pt1 pt2 "" "@" pause)
  (reerr)
  (PRINC)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ST (/ ss i e_name x_13 y_13 x_14 y_14 pt_14) 
  (setierr)
  (if 
    (NOT 
      (setq ss (ssget "i" 
                      '((-4 . "<OR")
                        (70 . 38)
                        (70 . 102)
                        (70 . 230)
                        (70 . 166)
                        (-4 . "OR>")
                       )
               ) ;_ end of ssget
      ) ;_ end of setq
    ) ;_ end of NOT
    (setq ss (ssget 
               '((-4 . "<OR")
                 (70 . 38)
                 (70 . 102)
                 (70 . 230)
                 (70 . 166)
                 (-4 . "OR>")
                )
             ) ;_ end of ssget
    ) ;_ end of setq
  ) ;_ end of if
  (command "ucs" "w")
  (setq i 0)
  (if ss 
    (if (is-ss-unlocked ss) 
      (repeat (sslength ss) 
        (setq e_name (ssname ss i)
              x_13   (car (get-value e_name 13))
              y_13   (cadr (get-value e_name 13))
              x_14   (car (get-value e_name 14))
              y_14   (cadr (get-value e_name 14))
              i      (1+ i)
        ) ;_ end of setq
        (if 
          (or (= (get-value e_name 70) 166) 
              (= (get-value e_name 70) 38)
          ) ;_ end of or
          (progn 
            (if (< (abs (- y_13 y_14)) 1) 
              (progn 
                (setq pto_14 (list x_14 (+ 3 y_14) 0)
                      pt_14  (list x_14 y_13 0)
                ) ;_ end of setq
              ) ;_ end of progn
              (setq pt_14 (list x_14 y_13 0.0))
            ) ;_ end of if
          ) ;_ end of progn
          (progn 
            (if (< (abs (- x_13 x_14)) 1) 
              (progn 
                (setq pto_14 (list (+ x_14 3) y_14 0)
                      pt_14  (list x_13 y_14 0)
                ) ;_ end of setq
              ) ;_ end of progn
              (setq pt_14 (list x_13 y_14 0.0))
            ) ;_ end of if
          ) ;_ end of progn
        ) ;_ end of if
        (change-value e_name 14 pt_14)
      ) ;_ end of repeat
      (alert "All locked!")
    ) ;_ end of if
    (alert "\nNo selection!")
  ) ;_ end of if
  (command "ucs" "p")
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:T0 (/ sel i) 
  (setierr)
  (if 
    (not 
      (or (setq sel (ssget "_i" '((0 . "DIMENSION")))) 
          (setq sel (ssget "l" '((0 . "DIMENSION"))))
      ) ;_ end of or
    ) ;_ end of not
    (progn 
      (princ "\nSelect a Dimension:")
      (setq sel (ssget '((0 . "DIMENSION"))))
    ) ;_ end of progn
  ) ;_ end of if
  (setq i 0)
  (if sel 
    (if (is-ss-unlocked sel) 
      (repeat (sslength sel) 
        (setq di (ssname sel i)
              i  (+ i 1)
        ) ;_ end of setq
        (command "DIMOVERRIDE" "dimtoh" 0 "dimfit" 3 "" di "")
      ) ;_ end of repeat
      (alert "All locked!")
    ) ;_ end of if
    (alert "No selection!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun

(defun c:T1 (/ sel) 
  (setierr)
  (setq i 0)
  (if (setq sel (ssget '((0 . "DIMENSION")))) 
    (if (is-ss-unlocked sel) 
      (repeat (sslength sel) 
        (setq di (ssname sel i)
              i  (+ i 1)
        ) ;_ end of setq
        (command "DIMOVERRIDE" "dimtoh" 1 "dimfit" 3 "" di "")
      ) ;_ end of repeat
      (alert "All locked!")
    ) ;_ end of if
    (alert "No selection!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:TE (/ sca) 
  (if (tblsearch "style" "SIMPLEX") 
    (command "style" "SIMPLEX" "" "" "" "" "" "" "")
    (command "style" "SIMPLEX" "SIMPLEX" "" "" "" "" "" "")
  ) ;_ end of if
  (setierr)
  (if (= (getvar "dimscale") 0) 
    (setvar "dimscale" 1)
  ) ;_ end of if
  (setq sca (getvar "dimscale")
        sca (* sca (getvar "dimtxt"))
  ) ;_ end of setq
  (COMMAND "_text" PAUSE sca "0" "Detail 1:1")
  (COMMAND "_ddedit" "L" "")
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The sub Key                                                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angset () 
  (setvar "angbase" 0)
  (setvar "angdir" 0)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bound-c (pt1 pt2 /) 
  (setq ss1 (ssget "c" 
                   pt1
                   pt2
                   '((-4 . "<AND")
                     (-4 . "<OR")
                     (0 . "LINE")
                     (0 . "CIRCLE")
                     (0 . "ARC")
                     (-4 . "OR>")
                     (-4 . "<NOT")
                     (8 . "41")
                     (-4 . "NOT>")
                     (-4 . "AND>")
                    )
            ) ;end ssget
  ) ;end setq
  (bound-sub ss1)
) ;_ end of defun bound-c

(defun bound-c-line (line-name / p1 p2 p3 p4 do) 
  (setq p1 (line-start-point line-name)
        p2 (line-end-point line-name)
        p3 p1
        p4 p2
        do t
  ) ;_ end of setq
  (while do 
         (bound-c p1 p2)
         (setq p1    pu-p1
               p2    pu-p2
               pu-p1 nil
               pu-p2 nil
         ) ;_ end of setq
         (if 
           (and 
             (< (distance p1 p3) 0.001)
             (< (distance p2 p4) 0.001)
           ) ;_ end of and
           (setq do nil)
           (setq p3 p1
                 p4 p2
           ) ;_ end of setq
         ) ;_ end of if
  ) ;_ end of while
  (setq pu-p1 p1
        pu-p2 p2
  ) ;_ end of setq
) ;_ end of defun bound-c-line

(defun bound-sub (ss1 / i typ name tab pt3 pt4 x3 y3 x4 y4 ra minx miny maxx maxy) 
  (setq i    0
        minx 99999999
        miny 99999999
        maxx -99999999
        maxy -99999999
  ) ;_ end of setq
  (if ss1 
    (progn 
      (repeat (sslength ss1) 
        (setq name (ssname ss1 i)
              tab  (entget name)
              typ  (cdr (assoc 0 tab))
              i    (+ i 1)
        ) ;end setq
        (if (= typ "LINE") 
          (progn 
            (setq pt3 (cdr (assoc 10 tab))
                  pt4 (cdr (assoc 11 tab))
                  x3  (car pt3)
                  y3  (cadr pt3)
                  x4  (car pt4)
                  y4  (cadr pt4)
            ) ;end setq
            (setq minx (min minx x3 x4)
                  miny (min miny y3 y4)
                  maxx (max maxx x3 x4)
                  maxy (max maxy y3 y4)
            ) ;end setq
          ) ;end progn then
          (progn  ;;arc or circle
                 (setq pt3 (cdr (assoc 10 tab))
                       x3  (car pt3)
                       y3  (cadr pt3)
                       ra  (cdr (assoc 40 tab))
                 ) ;end setq
                 (setq minx (min minx (- x3 ra))
                       miny (min miny (- y3 ra))
                       maxx (max maxx (+ x3 ra))
                       maxy (max maxy (+ y3 ra))
                 ) ;end setq
          ) ;end progn else
        ) ;end if
      ) ;end repeat
      (setq pu-p1 (list minx miny)
            pu-p2 (list maxx maxy)
      ) ;_ end of setq
    ) ;_ end of progn
    (progn 
      (alert "\nNo selection or Only entity on '41' layer!")
      (command "zoom" "p")
      (exit)
    ) ;_ end of progn
  ) ;end if ss1
) ;end bound-sub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc (ss / nam i tb p1 len inc) 
  (angset)
  (command "ucs" "w")
  (setq i   0
        obj (ssadd)
  ) ;_ end of setq
  (if (= (type ss) 'ename) 
    (ssadd ss obj)
    (setq obj ss)
  ) ;_ end of if
  (repeat (sslength obj) 
    (setq nam (ssname obj i)
          i   (+ i 1)
          tb  (entget nam)
    ) ;_ end of setq
    (setq p1  (cdr (assoc 10 tb)) ;Coordinate of the start point
          p2  (cdr (assoc 11 tb)) ;Coordinate of the end point
          len (distance p1 p2) ;Length of the Line
          inc (* len 0.08) ;Increment
    ) ;_ end of setq
    (command "change" nam "" (polar p1 (angle p2 p1) inc))
    (command "change" nam "" (polar p2 (angle p1 p2) inc))
  ) ;_ end of repeat
  (ce obj)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ce (ss /) 
  (sub-ltchange ss "center" 4 "red")
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun change-value (e-name no value / tab val) 
  (setq tab (entget e-name)
        val (cdr (assoc no tab))
        tab (subst (cons no value) (assoc no tab) tab)
  ) ;_ end of setq
  (entmod tab)
  (entupd e-name)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ch_Precision (num / sel di) 
  (setierr)
  (setq i 0)
  (if (setq sel (ssget '((0 . "DIMENSION")))) 
    (if (is-ss-unlocked sel) 
      (repeat (sslength sel) 
        (setq di (ssname sel i)
              i  (1+ i)
        ) ;_ end of setq
        (command "dimoverride" 
                 "dimzin"
                 "0"
                 "dimazin"
                 "0"
                 "dimdec"
                 num
                 "dimadec"
                 num
                 ""
                 di
                 ""
        ) ;_ end of command
      ) ;_ end of repeat
      (alert "All locked!")
    ) ;_ end of if
    (alert "No selection!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cm (ss3 / d i c1 nam tab cen ra lt cel lay cur len1 len2 lanam j latab p1 p2) 
  (command "ucs" "w")
  (setq d   (getvar "dimscale")
        cur (getvar "clayer")
  ) ;_ end of setq
  (if (= d 0) 
    (setvar "dimscale" 1)
  ) ;_ end of if
  (setq i   0
        sel (ssadd)
  ) ;_ end of setq
  (if (= (type ss3) 'ename) 
    (ssadd ss3 sel)
    (setq sel ss3)
  ) ;_ end of if
  (repeat (sslength sel) 
    (setq nam (ssname sel i)
          tab (entget nam)
          cen (cdr (assoc 10 tab)) ;Center
          px  (car cen)
          py  (cadr cen)
          c1  (list (+ 0.005 px) (+ 0.005 py))
          i   (+ i 1)
          ra  (cdr (assoc 40 tab)) ;radius
          lay (cdr (assoc 8 tab)) ;layer
          lt  (getvar "ltscale")
          cel (/ (* ra 0.5) lt)
          dsc (/ 1 (getvar "dimscale"))
          nam (list nam cen)
          j   0
    ) ;_ end of setq
    (setvar "celtscale" cel)
    (setvar "clayer" lay)
    (setvar "dimcen" (* ra 1.2 dsc))
    (COMMAND "DIMCENTER" nam)
    (setq la   (ssget "c" cen c1 '((0 . "LINE")))
          len1 (* ra 2.4)
    ) ;_ end of setq
    (repeat (sslength la) 
      (setq lanam (ssname la j)
            latab (entget lanam)
            p1    (cdr (assoc 10 latab))
            p2    (cdr (assoc 11 latab))
            len2  (distance p1 p2)
            j     (+ j 1)
      ) ;_ end of setq
      (if (> (abs (- len1 len2)) 0.001) 
        (progn 
          (setq la (ssdel lanam la)
                j  (- j 1)
          ) ;_ end of setq
        ) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of repeat
    (ce la)
  ) ;_ end of repeat
  (command "ucs" "p")
  (setvar "clayer" cur)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-layer (sname / tab) 
  (setq tab           (entget sname)
        pu_layer_name (cdr (assoc 8 tab))
  ) ;_ end of setq
) ;_ end of defun

(defun get-type (name / tab) 
  (setq tab      (entget name)
        pub_type (cdr (assoc 0 tab))
  ) ;_ end of setq
) ;_ end of defun

(defun get-value (name tag / tab) 
  (setq tab   (entget name)
        value (cdr (assoc tag tab))
  ) ;_ end of setq
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-entity-locked (ename / la fl) 
  (setq la (get-layer ename)
        fl (cdr (assoc 70 (tblsearch "LAYER" la)))
  ) ;_ end of setq
  (if 
    (or (= fl 4) 
        (= fl 5)
    ) ;_ end of or
    t
    nil
  ) ;_ end of if
) ;_ end of defun

(defun is-ss-unlocked (ss / i unlocked do ename) 
  (if ss 
    (progn 
      (setq i        0
            unlocked nil
            do       t
      ) ;_ end of setq
      (while do 
             (setq ename (ssname ss i)
                   i     (1+ i)
             ) ;_ end of setq
             (if (= i (sslength ss)) 
               (setq do nil)
             ) ;_ end of if
             (if (not (is-entity-locked ename)) 
               (setq unlocked t
                     do       nil
               ) ;_ end of setq
             ) ;_ end of if
      ) ;_ end of while
      unlocked
    ) ;_ end of progn
    (progn 
      (alert "Selection null!")
      (exit)
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun layer-41 (/ c) 
  (setq c (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (tblsearch "layer" "41") 
    (progn 
      (command "layer" "on" "41" "u" "41" "T" "41" "")
      (setvar "clayer" "41")
    ) ;_ end of progn
    (command "layer" "n" "41" "c" 3 "41" "s" "41" "")
  ) ;_ end of if
  (setvar "cmdecho" c)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun line-start-point (line-name / tab) 
  (setq tab   (entget line-name)
        pu-pt (cdr (assoc 10 tab))
  ) ;_ end of setq
) ;_ end of defun

(defun line-end-point (line-name / tab) 
  (setq tab   (entget line-name)
        pu-pt (cdr (assoc 11 tab))
  ) ;_ end of setq
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lt-change (text scale / esel nam tab lsc i) 
  (setierr)
  (princ (strcat "选取线型比例" text "的实体[可复选]:"))
  (setq esel (ssget 
               '((-4 . "<OR")
                 (0 . "LINE")
                 (0 . "CIRCLE")
                 (0 . "ARC")
                 (-4 . "OR>")
                )
             ) ;end ssget
        i    0
  ) ;_ end of setq
  (if esel 
    (repeat (sslength esel) 
      (setq nam (ssname esel i)
            tab (entget nam)
      ) ;_ end of setq
      (if (assoc 48 tab) 
        (setq lsc (cdr (assoc 48 tab)))
        (setq lsc 1.0)
      ) ;end if
      (setq lsc (* lsc scale)
            i   (+ i 1)
      ) ;end setq
      (command "change" nam "" "p" "s" lsc "")
    ) ;end repeat
    (alert "No Selected!")
  ) ;end if
  (reerr)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scale (/ lfac ans ceco) 
  (setq lfac (getvar "dimlfac")
        ceco (getvar "cecolor")
        ans  ""
  ) ;end setq
  (if (/= lfac 1) 
    (progn 
      (while (AND (/= ans "N") (/= ans "Y")) 
             (setq ans (strcase 
                         (getstring 
                           (strcat "\nDimlfac = " 
                                   (rtos lfac 2 2)
                                   ", go on...?(Y/N)"
                           ) ;_ end of strcat
                         ) ;_ end of getstring
                       ) ;_ end of strcase
             ) ;end setq
             (if (= ans "N") 
               (progn 
                 (setvar "dimlfac" 1)
                 (command "color" "bylayer")
               ) ;end progn then
             ) ;end if  (= ans "N")
             (if (= ans "Y") 
               (command "color" "cyan")
             ) ;end if (= ans "Y")
      ) ;end while
    ) ;end progn then
    (progn 
      (command "color" "bylayer")
    ) ;end progn else
  ) ;end if (/= lfac 1.0)
) ;end defun scale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select-object (text type1 / s ent-name) 
  (while 
    (not 
      (progn 
        (setq ent-name (entsel (strcat "\nSelect " text ":")))
        (if ent-name 
          (= (get-type (car ent-name)) type1)
          nil
        ) ;_ end of if
      ) ;_ end of progn
    ) ;_ end of not
    (alert (strcat "\nMust " text " , go on..."))
  ) ;_ end of while
  (redraw (car ent-name) 3)
  ent-name
) ;_ end of defun

(defun select-object-two (text1 type1 text2 type2 / s ent-name) 
  (while 
    (not 
      (progn 
        (setq ent-name (entsel 
                         (strcat "\nSelect a " text1 " or " text2 ":")
                       ) ;_ end of entsel
        ) ;_ end of setq
        (if ent-name 
          (or (= (get-type (car ent-name)) type1) 
              (= (get-type (car ent-name)) type2)
          ) ;_ end of or
          nil
        ) ;_ end of if
      ) ;_ end of progn
    ) ;_ end of not
    (alert (strcat "\nMust " text1 " or " text2 " , go on..."))
  ) ;_ end of while
  (redraw (car ent-name) 3)
  ent-name
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sub-ltchange (ss type1 scale color / n nam tab rad len pt1 pt2 leg sca lts ss1 c) 
  (setq n   0
        ss1 (ssadd)
  ) ;_ end of setq
  (setq c (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (= (type ss) 'ename) 
    (ssadd ss ss1) ;if entity name ,then change to seletion
    (setq ss1 ss)
  ) ;_ end of if
  (repeat (sslength ss1) 
    (setq nam (ssname ss1 n)
          tab (entget nam)
          n   (1+ n)
    ) ;_ end of setq
    (setq lts (getvar "ltscale"))
    (setq sca 1)
    (if (not (tblsearch "LTYPE" type1)) 
      (command "-linetype" "l" type1 "acad.lin" "")
    ) ;_ end of if
    (command "change" nam "" "p" "c" color "lt" type1 "s" sca "") ;_ end of command
  ) ;_ end of repeat
  (setvar "cmdecho" c)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sub-qz (num /) 
  (setierr)
  (if 
    (NOT 
      (setq ss (ssget "i" 
                      '((-4 . "<OR")
                        (0 . "TEXT")
                        (0 . "MTEXT")
                        (0 . "DIMENSION")
                        (-4 . "OR>")
                       )
               ) ;_ end of ssget
      ) ;_ end of setq
    ) ;_ end of NOT
    (setq ss (ssget 
               '((-4 . "<OR")
                 (0 . "TEXT")
                 (0 . "MTEXT")
                 (0 . "DIMENSION")
                 (-4 . "OR>")
                )
             ) ;_ end of ssget
    ) ;_ end of setq
  ) ;_ end of if
  (if ss 
    (if (is-ss-unlocked ss) 
      (progn 
        (setq i 0)
        (repeat (sslength ss) 
          (setq ob (ssname ss i)
                tb (entget ob)
                va (cdr (assoc 1 tb))
                i  (1+ i)
          ) ;_ end of setq
          (if (= va "") 
            (setq va "<>")
          ) ;_ end of if
          (if (= num "1-") 
            (if (setq p (vl-string-search "-" va)) 
              (setq va (substr va (+ p 2) (- (strlen va) p 1)))
            ) ;_ end of if
            (if (setq p (vl-string-search "-" va)) 
              (progn 
                (setq str1 (substr va 1 p)) ;has not "-"
                (if (= (type (read str1)) 'INT) 
                  (setq va (strcat num (substr va (+ p 2) (strlen va))))
                  (setq va (strcat num va))
                ) ;_ end of if
              ) ;_ end of progn
              (setq va (strcat num va))
            ) ;_ end of if
          ) ;_ end of if
          (setq tb (subst (cons 1 va) (assoc 1 tb) tb)) ;_ end of setq
          (entmod tb)
          (entupd ob)
        ) ;_ end of repeat
      ) ;_ end of progn
      (alert "All locked!")
    ) ;_ end of if
    (alert "No selection!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(SetQ **SysVarNL** '("AUNITS" "AUPREC" "ATTDIA" "CECOLOR" "CELTYPE" "CMDECHO" 
                     "EXPERT" "HIGHLIGHT" "LUNITS" "LUPREC" "OSMODE" "ORTHOMODE" 
                     "TEXTSTYLE" "PLINEWID" "DIMPOST")
) ;_ end of SetQ

(Defun-Q SetIErr (/ sv) 
  (If (= 'LIST (Type *error*)) 
    (Alert "错误：最后一个(SetIErr)函数没有配对的(ReErr)!")
    (Progn (SetQ **svarl** '()) 
           (ForEach sv **SysVarNL** 
             (SetQ **svarl** (Cons (GetVar sv) **svarl**))
           ) ;_ end of ForEach
           (ForEach sv 
             '("ATTDIA" "BLIPMODE" "CMDECHO" ;"HIGHLIGHT"
              "OSMODE" "ORTHOMODE")
             (SetVar sv 0)
           ) ;_ end of ForEach
           (Defun-Q *error* (st) (ReErr))
    ) ;_ end of Progn
  ) ;_ end of If
) ;_ end of Defun-Q

(Defun-Q ReErr () 
  (If (= 'LIST (Type *error*)) 
    (Progn (MapCar 'SetVar **SysVarNL** (Reverse **svarl**)) 
           (SetQ *error* nil)
           (SetVar "modemacro" ".")
           (PrinC)
    ) ;_ end of Progn
    (Alert "错误：没有对应的(SetIErr)!")
  ) ;_ end of If
) ;_ end of Defun-Q
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tole7 (up low / di c) 
  (setierr)
  (if (setq di (ssget '((0 . "DIMENSION")))) 
    (if (is-ss-unlocked di) 
      (command "dimoverride" 
               "dimdec"
               "3"
               "dimzin"
               "8"
               "dimtdec"
               "3"
               "dimtzin"
               "8"
               "dimtol"
               "on"
               "dimtfac"
               "0.7"
               "dimtp"
               up
               "dimtm"
               low
               ""
               di
               ""
      ) ;_ end of command
      (alert "All locked!")
    ) ;_ end of if
    (alert "No selection!")
  ) ;_ end of if
  (reerr)
  (princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;