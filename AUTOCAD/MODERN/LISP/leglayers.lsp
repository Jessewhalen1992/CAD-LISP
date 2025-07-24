;;
;;following routines are used when converting construction plan to legal layers
;;this routine uses external file C:\AUTOCAD-SETUP\Lisp_2000\legal-layers-to-stay.csv
;;

;freeze all layers except legal layers listed in csv file
(defun c:legf ( /)
  (command "cmdecho" 0)
  (setq layerfile (findfile "legal-layers-to-stay.csv"))
;"C:\AUTOCAD-SETUP\Lisp_2000\legal-layers-to-stay.csv")
  ;(setq layerfile (getfiled "Name of file to import"  "" "" 0))
  (setq cnt 0)
  (setvar "clayer" "0")
  (command "-layer" "thaw" "*" "on" "*" "")
  (if (setq f (open layerfile "r"))
    (progn
      (setq ListItem (read-line f))
      (while ListItem
	(setq ListItem (read-line f))
	(command "-layer" "freeze" listitem "")
	);end while
      );end progn
    );end if
  (if f (close f))
(command "zoom" "extents")
(command "zoom" "0.95xp")
  (alert "** Legal layers frozen **\nReview objects shown on remaining layers to ensure they are not required for legal plan")
  (command "cmdecho" 1)
  ); end defun

;thaw all legal layers listed in csv file
(defun c:legt ( /)
  (command "cmdecho" 0)
  (setq layerfile (findfile "legal-layers-to-stay.csv"))
  (setq cnt 0)
  (setvar "clayer" "0")
  (command "-layer" "freeze" "*" "on" "*" "")
  (if (setq f (open layerfile "r"))
    (progn
      (setq ListItem (read-line f))
      (while ListItem
	(setq ListItem (read-line f))
	(command "-layer" "thaw" listitem "")
	);end while
      );end progn
    );end if
  (if f (close f))
(command "zoom" "extents")
(command "zoom" "0.95xp")
  (alert "** Legal layers thawed **\nReview objects shown to ensure they are required for legal plan")
  (command "cmdecho" 1)
  ); end defun

;delete items on a specified layer
(defun c:legdel (/ l e)
      (setq l (strcase (getstring "\nEnter Layer To Delete:   ")))
      (setq e (entnext))
            (while e
            (prompt "\nReading Database")
            (if (= L (cdr (assoc 8 (entget e))))
                  (entdel e)
            )
      (setq e (entnext e))
     )
(prin1)   
   ) 

;;TO BE WORKED ON******************
;;darci anglehart October 22 2012
;;following routines are used when converting construction plan to legal layers
;;this routine uses external file legal-layers-to-stay-with-disp.csv
;;These routines work same as others except they retain the layers for access roads and well sites as 
;;these are requested to be left on in some uns areas in the north

;freeze all layers except legal layers listed in csv file
(defun c:legfd ( /)
  (command "cmdecho" 0)
  (setq layerfile (findfile "legal-layers-to-stay-with-disp.csv"))
  (setq cnt 0)
  (setvar "clayer" "0")
  (command "-layer" "thaw" "*" "on" "*" "")
  (if (setq f (open layerfile "r"))
    (progn
      (setq ListItem (read-line f))
      (while ListItem
	(setq ListItem (read-line f))
	(command "-layer" "freeze" listitem "")
	);end while
      );end progn
    );end if
  (if f (close f))
(command "zoom" "extents")
(command "zoom" "0.95xp")
  (alert "** Legal & disposition layers frozen **\nReview objects shown on remaining layers to ensure they are not required for legal plan")
  (command "cmdecho" 1)
  ); end defun

;thaw all legal layers listed in csv file
(defun c:legtd ( /)
  (command "cmdecho" 0)
  (setq layerfile (findfile "legal-layers-to-stay-with-disp.csv"))
  (setq cnt 0)
  (setvar "clayer" "0")
  (command "-layer" "freeze" "*" "on" "*" "")
  (if (setq f (open layerfile "r"))
    (progn
      (setq ListItem (read-line f))
      (while ListItem
	(setq ListItem (read-line f))
	(command "-layer" "thaw" listitem "")
	);end while
      );end progn
    );end if
  (if f (close f))
(command "zoom" "extents")
(command "zoom" "0.95xp")
  (alert "** Legal & disposition layers thawed **\nReview objects shown to ensure they are required for legal plan")
  (command "cmdecho" 1)
  ); end defun

(setq count 0)
(setq a (ssget "x"))
(setq slen (sslength a))
(while (< count slen)
(setq inspt (cdr (assoc 1 (entget (setq ent (ssname a count))))))
(if 
(OR
(= inspt "High Grade Gravel Road Allowance")
(= inspt "Undeveloped Road Allowance")
(= inspt "Paved Road Allowance")
(= inspt "Undeveloped Road Allowance (Trail)")
)
(progn
(setq newtxt "Road  Allowance")
(setq ent1 (ssname a count))
(setq ent2 (entget ent1))
(setq ent3 (assoc 1 ent2))
(setq newlist (cons (car ent3) newtxt))
(setq newent (subst newlist ent3 ent2))
(entmod newent)
)
()
)
(setq count (+ 1 count))
)

    (if        (setq ss (ssget "_X" '((0 . "TEXT") (8 . "L-AREAREQUIRED"))))
      (progn
                (setq i   0
                      n       (sslength ss)
                )
                (repeat n
                  (setq   e (ssname ss i)
                                OldText (cdr (assoc 1 (setq ed (entget e))))
                                i (1+ i)
                                p (vl-string-search " ha " OldText)
                  )
                  (if p
                    (progn
                      (setq ed
                                     (subst (cons 1 (substr OldText 1 (+ 3 p))) (assoc 1 ed) ed)
                      )
                      (entmod ed)
                    )
                  )
                )
      )
    )

     (if (setq ss (ssget "_X" '((0 . "DIMENSION") (-4 . "<OR") (8 . "F-RW-T") (8 . "C-RW-T") (-4 . "OR>"))))
      (progn
                (setq i   0
                      n       (sslength ss)
                )
                (repeat n
                  (setq   e (ssname ss i)
                                OldText (cdr (assoc 1 (setq ed (entget e))))
                                i (1+ i)
                                p (vl-string-search "\\P" OldText)
                  )
                  (if p
                    (progn
                      (setq ed
                                     (subst (cons 1 (substr OldText (+ 3 p))) (assoc 1 ed) ed)
                      )
                      (entmod ed)
                    )
                  )
                )
      )
    )


(princ)