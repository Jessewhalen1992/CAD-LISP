;;; pointimport.LSP
;;; By Jimmy Bergmark, JTB World, www.jtbworld.com 2013-03-29
;;; Rev 2015-09-06

(defun c:pi (/ fn fdata row strings featuredata inslay
	     split createlayer createstyle
	     noinsert
	    )
  (initget "Yes No")
  (setq noinsert (getkword "Do you want to insert blocks [Yes/No]<Yes>: "))
  (setq noinsert (= noinsert "No"))
  ;				
  (defun split (st / pos s lst);split st with "," as delimiter
    (setq pos 1
	  s   ""
	  lst nil
    )
    (repeat (strlen st)
      (if (= (substr st pos 1) ",")
	(setq lst (append lst (list s))
	      s	  ""
	)
	(setq s (strcat s (substr st pos 1)))
      )
      (setq pos (+ pos 1))
    )
    (setq lst (append lst (list s)))
  )
  ;									

  (if (not noinsert) (progn
  (setq	fn   "C:\\AUTOCAD-SETUP-CG\\CG-LISP\\feature codes_blocks.csv"
	path "C:\\AUTOCAD-SETUP-CG\\BLOCKS\\symbols\\"
	fdata (open fn "r")
	featuredata nil
  )
  (read-line fdata)

  (while (setq row (read-line fdata))
    (setq strings (split row))
    (if	(= (length strings) 2)
      (setq featuredata
	     (cons (list (nth 0 strings) (nth 1 strings))
		   featuredata
	     )
      )
    )
  )
  (close fdata)
  ))
  ;				
  (defun createlayer (layer)
    (if
      (not
	(tblsearch "layer" layer)
      )
       (command "._-layer" "_m" layer "_s" "0" "")
       (
       )
    )
  )
  ;				
  (defun createstyle (style)
    (if
      (not
	(tblsearch "style" style)
      )
       (command "._-style" style "" "" "" "" "" "" "")
       (
       )
    )
  )

  ;							
  (createlayer "Z-MSPOINT")
  (createlayer "Z-POINTNUMBER")
  (createlayer "Z-DESCRIPTION")
  (createlayer "Z-ELEVATION")
  (createstyle "POINTNUMBER")
  (createstyle "DESCRIPTION")
  (createstyle "ELEVATION")
  (setq fn (getfiled "Select a CSV File" "" "csv" 0))
  (setq fdata (open fn "r"))

  (while (setq row (read-line fdata))
    (setq strings (split row))
    (if	(>= (length strings) 5)
      (progn
					; point number
	(setq vl
	       (list
		 (cons 0 "TEXT")
		 (cons 100 "AcDbEntity")
		 (cons 410 "Model")
					; layer name
		 (cons 8 "Z-POINTNUMBER")
		 (cons 100 "AcDbText")
					; insertion point
		 (cons 10
		       (list (atof (nth 2 strings))
			     (atof (nth 1 strings))
			     0.0
		       )
		 )
					; text height
		 (cons 40 1.5)
					; text value
		 (cons 1 (nth 0 strings))
					; text rotation
		 (cons 50 0.0)
					; Text style name
		 (cons 7 "POINTNUMBER")
	       )
	)
	(entmake vl)

					; point description
	(setq vl
	       (list
		 (cons 0 "TEXT")
		 (cons 100 "AcDbEntity")
		 (cons 410 "Model")
					; layer name
		 (cons 8 "Z-DESCRIPTION")
		 (cons 100 "AcDbText")
					; insertion point
		 (cons 10
		       (list (atof (nth 2 strings))
			     (atof (nth 1 strings))
			     0.0
		       )
		 )
					; text height
		 (cons 40 1.5)
					; text value
		 (cons 1 (nth 4 strings))
					; text rotation
		 (cons 50 0.418879)
					; Text style name
		 (cons 7 "DESCRIPTION")
	       )
	)
	(entmake vl)

					; point elevation
	(setq vl
	       (list
		 (cons 0 "TEXT")
		 (cons 100 "AcDbEntity")
		 (cons 410 "Model")
					; layer name
		 (cons 8 "Z-ELEVATION")
		 (cons 100 "AcDbText")
					; insertion point
		 (cons 10
		       (list (atof (nth 2 strings))
			     (atof (nth 1 strings))
			     0.0
		       )
		 )
					; text height
		 (cons 40 1.5)
					; text value
		 (cons 1 (nth 3 strings))
					; text rotation
		 (cons 50 0.785398)
					; Text style name
		 (cons 7 "ELEVATION")
	       )
	)
	(entmake vl)

					; point
	(setq vl
	       (list
		 (cons 0 "POINT")
		 (cons 100 "AcDbEntity")
		 (cons 410 "Model")
		 (cons 8 "Z-MSPOINT")
		 (cons 100 "AcDbPoint")
		 (cons 10
		       (list (atof (nth 2 strings))
			     (atof (nth 1 strings))
			     (atof (nth 3 strings))
		       )
		 )
	       )
	)
	(entmake vl)
					
	(if (not noinsert)(progn ; insert					
	(setq bn "")
	(foreach row featuredata
	  (if (wcmatch (strcase (nth 4 strings)) (strcase (nth 0 row)))
	    (setq bn (nth 1 row))
	  )
	)
	(setq bn (strcase bn))
	(setq inslay
	 (cond
	  ((= bn "ASCM.DWG") "L-MON")
	  ((= bn "DYN_ANCHOR.DWG") "AS-SYMBOL")
	  ((= bn "FDI.DWG") "L-MON")
	  ((= bn "FDIBAR.DWG") "L-MON")
	  ((= bn "FDSPIKE.DWG") "L-MON")
	  ((= bn "FLARE.DWG") "AS-SYMBOL")
	  ((= bn "GAS.DWG") "AS-SYMBOL")
	  ((= bn "LIGHTPOLE.DWG") "AS-SYMBOL")
	  ((= bn "PIG.DWG") "AS-SYMBOL")
	  ((= bn "PILET.DWG") "AS-SYMBOL")
	  ((= bn "PLHUB.DWG") "L-MON")
	  ((= bn "PLI.DWG") "L-MON")
	  ((= bn "PLSPIKE.DWG") "L-MON")
	  ((= bn "POWERPOLE.DWG") "AS-POWER-EXISTING")
	  ((= bn "RES_OCC.DWG") "AS-RESIDENCE")
	  ((= bn "RISER.DWG") "AS-SYMBOL")
	  ((= bn "SP.DWG") "AS-SYMBOL")
	  ((= bn "VALVE.DWG") "AS-SYMBOL")
	  ((= bn "WATER_WELL.DWG") "0")
	  ((= bn "WC.DWG") "L-MON")
	  ((= bn "WITF.DWG") "L-MON")
	  (t "0")
	 )
	)
	(createlayer inslay)
	(command "._-LAYER" "_S" inslay "")
	(if (findfile (strcat path bn))
	  (command "._-INSERT"
		   (strcat path bn)
		   (list (atof (nth 2 strings))
			 (atof (nth 1 strings))
			 0
		   )
		   5
		   ""
		   ""
	  )
	);if
	));if noinsert
      )
    )
  )
  (close fdata)
  (command "._-LAYER" "_S" "0" "")
  (princ)
)


