;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:MC (/)
	;(setierr)
	(command "ucs" "w")
	(angset)
	(setq	line1 (car (select-object "the fist line" "LINE"))
		tab1	(entget line1)
		p1		(cdr (assoc 10 tab1))
		p2		(cdr (assoc 11 tab1))
		ang1	(angle p1 p2)
		line2	(car (select-object "the second line" "LINE"))
		tab2	(entget line2)
		p3		(cdr (assoc 10 tab2))
		p4		(cdr (assoc 11 tab2))
		ang2	(angle p3 p4)
	) ;_ end of setq

	(if (not (or (equal (angtos ang1) (angtos ang2) 0.0001)
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

	(setq	pt-int (inters p1 (polar p1 (+ ang1 (/ pi 2)) 1) p3 p4 nil)
		dis (distance p1 pt-int)
	) ;_ end of setq

	(command "offset" (/ dis 2) line1 p3 "")
	(cc (ssget "l"))
	(redraw line1 4)
	(redraw line2 4)
	(command "ucs" "p")
	;(Reerr)
	(princ)
) ;_ end of defun mc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angset ()
	(setvar "angbase" 0)
	(setvar "angdir" 0)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc (ss / nam i tb p1 len inc)
	(angset)
	(command "ucs" "w")
	(setq	i   0
		obj (ssadd)
	) ;_ end of setq
	(if (= (type ss) 'ename)
		(ssadd ss obj)
		(setq obj ss)
	) ;_ end of if
	(repeat (sslength obj)
		(setq nam (ssname obj i)
			i (+ i 1)
			tb  (entget nam)
		) ;_ end of setq
		(setq p1  (cdr (assoc 10 tb))	;Coordinate of the start point
			p2 (cdr (assoc 11 tb))			;Coordinate of the end point
			len (distance p1 p2)				;Length of the Line
			inc (* len 0.08)						;Increment
		) ;_ end of setq

		(command "change" nam "" (polar p1 (angle p2 p1) inc))
		(command "change" nam "" (polar p2 (angle p1 p2) inc))
	) ;_ end of repeat
	(ce obj)

	(princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ce (ss /)
	(sub-ltchange ss "center" 4 "red")
	(princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-type	(name / tab)
	(setq	tab (entget name)
		pub_type (cdr (assoc 0 tab))
	) ;_ end of setq
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sub-ltchange (ss type1 scale color / n	nam tab rad len pt1 pt2 leg sca lts ss1 c)
	(setq	n 0 ss1 (ssadd))
	(setq c (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (= (type ss) 'ename)
		(ssadd ss ss1)
		(setq ss1 ss)
	) ;_ end of if
	(repeat (sslength ss1)
		(setq nam (ssname ss1 n)
			tab (entget nam)
			n (1+ n)
		) ;_ end of setq
		(setq lts (getvar "ltscale"))
		;;circle
		(if	(= (get-type nam) "CIRCLE")
			(setq rad	(cdr (assoc 40 tab))
				len	(* 2 (* 3.14 rad))
			)
      ;;ARC
			(if (= (get-type nam) "ARC")
				(setq rad (cdr (assoc 40 tab))
					len (* 3.14 rad)
				) ;_ end of setq
				;;LINE
				(if (= (get-type nam) "LINE")
					(setq	pt1 (cdr (assoc 10 tab))
						pt2 (cdr (assoc 11 tab))
						len (distance pt1 pt2)
					) ;_ end of setq
					(progn
						(alert "\nEntity type error!")
						(redraw nam 3)
						(exit)
					) ;_ end of progn
				) ;_ end of if
			) ;_ end of if
		) ;_ end of if

		(cond ((and (> len 0)		(<= len 2))		(setq leg 2))
					((and (> len 2)		(<= len 5))		(setq leg 6))
	  			((and (> len 5)		(<= len 30))	(setq leg 20))
					((and (> len 30)	(<= len 50))	(setq leg 40))
					((and (> len 50)	(<= len 100))	(setq leg 75))
							 ((> len 100)								(setq leg 100))
		) ;_ end of cond
		(setq sca (/ leg scale lts 2))
		(if	(not (tblsearch "LTYPE" type1))
			(command "-linetype" "l" type1 "acad.lin" "")
		) ;_ end of if
		(command "change" nam "" "p" "c" color "lt"	type1 "s" sca "") ;_ end of command
	) ;_ end of repeat

	(setvar "cmdecho" c)
	(princ)
) ;_ end of defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;