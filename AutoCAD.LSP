;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The Main Key                                                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun c:DC (/ i ss_li name p0 p1 p2 p1x p2x p1y p2y tab typ ss i count na ang dis 
             angle_count tp1x tp2x tp1y tp2y ta ltyp) 
  (setierr)
  (command "ucs" "w")
  (layer-41)
  (scale)
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
      (SETQ S (* S1 tx)
            a (* ta lfac)
      )
      (if (> a 0.99) 
        (setq a (RtoS a 2 0))
        (setq a (RtoS a 2 1))
      )
      (setq ltyp (cdr (assoc 6 (entget name))))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The sub Key                                                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angset () 
  (setvar "angbase" 0)
  (setvar "angdir" 0)
) ;_ end of defun
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;