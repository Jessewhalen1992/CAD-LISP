;;; closure.LSP ver 1.0
;;; Command name is CLOSURE
;;; Select plan, polyline and where to place the text

;;; By Jimmy Bergmark
;;; Copyright (C) 2013 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2013-02-24 - First release

(vl-load-com)

(defun getcoords (ent)
  (vlax-safearray->list
    (vlax-variant-value
      (vlax-get-property
	(vlax-ename->vla-object ent)
	"Coordinates"
      )
    )
  )
)

(defun getclosed (ent)
  (vlax-get-property
    (vlax-ename->vla-object ent)
    "Closed"
  )
)

(defun c:closure (/ kw ent coords p1 p_before sum text p d bearing
		  closure InsertionPoint ad texto)
  (initget "Our otHer Twp")
  (setq kw (getkword "Select Plan [Our/otHer/Twp] <Our>: "))
  (princ "\nSelect open polyline: ")
  (setq ent (car (entsel)))
  (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent))))
    (if	(= :vlax-false (getclosed ent))
      (progn
	(setq coords (getcoords ent))
	(setq p1 (list (car coords) (cadr coords)))
	(setq coords (cddr coords))
	(setq p_before p1)
	(setq sum 0)
	(setq text "")
	(while coords
	  (setq p (list (car coords) (cadr coords)))
	  (setq d (distance p_before p))
	  (setq sum (+ sum d))
	  (setq bearing (angtos (angle p_before p) 1 3))
	  (setq	text
		 (strcat
		   text
		   (rtos d 2 2)
		   " "
		   (strcat
		     (vl-string-subst
		       "°"
		       "d"
		       (substr bearing
			       1
			       (+ 1 (vl-string-search "'" bearing))
		       )
		     )

		     (rtos
		       (*
			 (fix
			   (/
			     (+	(atoi
				  (vl-string-right-trim
				    "\""
				    (substr
				      bearing
				      (+ 2
					 (vl-string-search "'" bearing)
				      )
				    )
				  )
				)
				2.5
			     )
			     5
			   )
			 )
			 5
		       )
		       2
		       0
		     )
		     "\""
		   )
		   "\n"
		 )
	  )



	  (setq coords (cddr coords))
	  (setq p_before p)
	)
	(setq text
	       (strcat
		 text
		 "total="
		 (rtos sum 2 2)
		 "\n"
		 "misclosure="
		 (rtos (distance p1 p) 2 2)
		 "\n"
		 "closure="
		 (rtos (setq closure (/ sum (distance p1 p))) 2 2)
		 "\n"
	       )
	)

	(setq InsertionPoint (vlax-3D-Point (getpoint "Select point: "))
	      ad	     (vla-get-ActiveDocument (vlax-get-acad-object))
	)
	(setq texto (vla-addMText
			(if (= 1 (vla-get-activespace ad))
			  (vla-get-modelspace ad)
			  (if (= (vla-get-mspace ad) :vlax-true)
			    (vla-get-modelspace ad)
			    (vla-get-paperspace ad)
			  )
			)
			InsertionPoint
			0.0
			text
		      )
	)
	(vla-put-layer texto "C-CLOSURE")
	(vla-put-height texto 9.0)
	(cond
	  ((or (not kw) (= "Our" kw))
	   (if (< closure 7500.0)
	     (vla-put-color texto 240)
	     (vla-put-color texto 90)
	   )
	  )
	  ((= "otHer" kw)
	   (if (< closure 5000.0)
	     (vla-put-color texto 240)
	     (vla-put-color texto 90)
	   )
	  )
	  ((= "Twp" kw)
	   (if (< closure 2500.0)
	     (vla-put-color texto 240)
	     (vla-put-color texto 90)
	   )
	  )
	)



      )
    )
  )
  (princ)
)

(princ)