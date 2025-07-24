;GTECH Lisp by JTB World
;08 Dec 2015
;06 Oct 2015
;---------------------
(vl-load-com)

(prompt "\n-----------------------------------")
(prompt "\nGTECH Lisp | 08 Dec 2015 | JTB World")
(prompt "\n-----------------------------------\n")

;-----------------------------------------------------------
(defun C:PPE (/ 
	      *error* l*sshighlight l*savevars
	      ss nezdss i ent res filename file pline res pps ppbref
	      bref el inspo elv number pdesc dist text

	      j m c
	     )


  (l+begin)

  (prompt "Select AS-POWER-GTECH polylines and NEZD points: ")
  (setq ss ;(ssget (list (cons 0 "LWPOLYLINE")(cons 8 "AS-POWER-GTECH"))))
	    (ssget '((-4 . "<OR")
		     (-4 . "<AND")(0 . "INSERT") (2 . "NEZD POINT")(-4 . "AND>")
		     (-4 . "<AND")(0 . "LWPOLYLINE")(8 . "AS-POWER-GTECH")(-4 . "AND>")
		     (-4 . "OR>")
		    )
		      
	    )
  )

  ;(setq logss (ssget "_P")
	;ss (ssadd)
  ;)

  ;;;
  (if (and ss
	   (setq nezdss (ssget "_P" '((0 . "INSERT") (2 . "NEZD POINT"))))
      )
    (progn
      (setq i -1)
      
      (repeat (sslength nezdss)
	(setq i (1+ i)
	      ent (ssname nezdss i)
	      	      
	)

	(ssdel ent ss)
      )

      (if (= (sslength ss) 0)
      (prompt "*No pline found*")
      (progn ;									;
	
      (prompt "\n1. Grouping powerpoles and anchors ... ")

      (setq nezdss (l+groupNEZDpoints nezdss nil));list of (ppref anchor1 anchor2 ...)

      (prompt "\n2. Find attached powerpoles at plines vertices ... ")

      (setq res (l+collectPoints_on_pline ss))

      ;extract to csv									;
      (setq filename (getstring "\n3. Enter csv filename (Esc to skip) <PPE>: ")
	    filename (l+trim filename)
      )

      (if (= "" filename)(setq filename "PPE"))

      (setq filename (strcat (getvar "DWGPREFIX") filename ".csv"))
  
      (if (not (setq file (open filename "w")))
	(alert (strcat "\nCan not write to file: " filename))

	(foreach pline res
	  (foreach pps pline
	    ; = list (powerpole anchor1 anchor2 ...)
	    ;(prompt "\n# ...")
	  
	    (setq ppbref (car pps))
	    
	    (foreach bref pps
	      (setq el (entget bref)
		    inspo (cdr(assoc 10 el))
		    elv     (l+getatt3 bref "ELEVATION"   nil)
		    number  (l+getatt3 bref "NUMBER"      nil)
		    pdesc   (l+getatt3 bref "DESCRIPTION" nil)
		    pdesc   (strcase pdesc)
	      )

	      (if (= pdesc "EXPP_")(setq pdesc "EXPP_0_0_0_"))

	      (if (or (wcmatch pdesc "PP_*");this is a powerpole
		      (wcmatch pdesc "EXPP_*")
		      (wcmatch pdesc "ANC_*")
		   )
		(progn
		  ;manipulate the description a litte:
	        (if (equal bref ppbref)
		(setq ppinspo inspo)
		(progn
		  ;this is an anchor, desc = "ANC_*"
		  (setq dist (distance ppinspo inspo)
			pdesc(strcat "ANC_" (rtos dist 2 1) "_")
		  )
		)
	        )
	        );progn

		;(setq pdesc "P/L_UGABC12")

		;(if (not (wcmatch pdesc "ANC_*"))
		  (if (wcmatch pdesc "*#")
		    (progn;this is other block 
		      (setq j (strlen pdesc)
			    m nil)
		      
		      (repeat (1- (strlen pdesc))
			(setq j (1- j)
			      c (substr pdesc j 1)
			)
			(if (and (null m) (not (wcmatch c "#")))
			  (setq m j)
			)
		      );repeat

		      (setq pdesc (substr pdesc 1 m)
			    c (substr pdesc m 1)
		      )

		      (if (/= c "_")(setq pdesc (strcat pdesc "_")))		      
		    )		    
		  );if
		;);if
	      )

	      (setq text (strcat number
				 "," (rtos (cadr inspo))
				 "," (rtos (car  inspo))
				 "," elv
				 "," pdesc
				 ;"," (cdr(assoc 5 (entget bref)))
				 (if (wcmatch pdesc "EXPP*") (strcat ",," number) "")
			 )
	      )
	      
	      (write-line text file)	      
	    )
	  )

	  (write-line "" file)
         )
      )

      (if file (close file))
      (if file (l+OpenFile filename))
      ));if got plines
    );progn
  );if ss and nezdss

  (l+end)
)
;--------------------------------------------
(defun l+adjust_anchorRot (anchor inspo / el apo dist rot0
			   xscale yscale zscale rot1
			   )  
  (setq ;obj (vlax-ename->vla-object anchor)
	el  (entget anchor)
	apo (cdr (assoc 10 el))
	dist (distance inspo apo)
	rot0 (cdr(assoc 50 el))
	xscale (cdr (assoc 41 el))
	yscale (cdr (assoc 42 el))
	zscale (cdr (assoc 43 el))	
	rot1 (angle apo inspo)
  )

  (if (< yscale 0)(setq rot0 (+ rot0  pi)));only yscale effect

  (l+letdynprop anchor "Distance1" dist)
  (l+letdynprop anchor "Angle1"    (- rot1 rot0))  
)

;-----------------------------------------------------------
(defun l+selectNEZDpoints (plines /
			      pline
			      minx miny maxx maxy ext
			      ss i dist  vertices vertex
			      topright btmleft
			      
			     )

  (setq i -1)

  (repeat (sslength plines)
    (setq i (1+ i)
	  pline (ssname plines i)
	  vertices (l+assoc_all 10 (entget pline))
    )

  (foreach vertex vertices
	  (if minX
	    (setq minX (min minX (car  vertex))
		  minY (min minY (cadr vertex))
		  maxX (max maxX (car  vertex))
		  maxY (max maxY (cadr vertex))	      
	    )
	    (setq minX (car vertex)
		  maxX minX
		  minY (cadr vertex)
		  maxY minY
	    )
	  )
  );foreach
  );repeat

  (setq ext (/ (+ (- maxx minx) (- maxy miny)) 5)
	minx (- minx ext)
	maxx (+ maxx ext)
	miny (- miny ext)
	maxy (+ maxy ext)
	btmleft (list minx miny 0.)
	btmleft (l+transw2c btmleft nil)
	topright(list maxx maxy 0.)
	topright (l+transw2c topright nil)
  )

  ;Zoom all the pline:
  (command "._zoom" btmleft topright)
  (command "._zoom" "0.5x")
  (command ^C^C)

  (setq ss (ssget "_C"
		      topright btmleft
		      '((0 . "INSERT")(2 . "nezd point"))
	       );powerpole*,dyn_anchor*,
      )

  (command "._zoom" "_P")
  (command "._zoom" "_P")

  ss
)
;-----------------------------------
(defun l+groupNEZDpoints (nezdss flagNoAnchor / res i pbref inspo pdesc pps pnumber j
			  abref desc number pps usedanchors others)
  (setq i -1)
  
  (repeat (sslength nezdss)
    (setq i (1+ i)
	  pbref (ssname nezdss i)
	  pdesc   (l+getatt3 pbref "DESCRIPTION" nil)
	  pdesc (strcase pdesc)
    )

    (cond
      ;((wcmatch desc "ANC_*"))
      
      ((or (wcmatch pdesc "PP_*");this is a powerpole
	   (wcmatch pdesc "EXPP_*")
       )              

       (setq pps (list pbref)
	     pnumber (l+getatt3 pbref "NUMBER"      nil)
	     pnumber (atoi pnumber)
	     j -1
       )

       (if flagNoAnchor nil (progn
			       
	(repeat (sslength nezdss)
	  (setq j (1+ j)
		abref  (ssname nezdss j)
		desc   (l+getatt3 abref "DESCRIPTION" nil)
		desc   (strcase desc)
		number (l+getatt3 abref "NUMBER" nil)
	  )

	  (if (wcmatch desc "ANC_*")
	    (if (= pnumber (atoi number))
	      (if (member abref usedanchors)
		(prompt (strcat "\nAnchor " number " has more than 1 associated powerpole."))
		(setq pps (cons abref pps)
		      usedanchors (cons abref usedanchors)
		)
	      )
	    )
	  )
	 )));repeat progn if flagNoAnchor

	(setq res (cons (reverse pps) res))
      );if pdesc = PP*

      (T
       (setq others (cons pbref others));including anchors
      )
    );cond	;
  );repeat	;

  (foreach pbref others
    (if (member pbref usedanchors)
      nil
      ;else: can be an anchor, or an other bref
      (setq res (cons (list pbref) res))
    )
  )

  
  ;>>> {{pp anchor1 anchor2 anchor3}} ... no dupplicated blocks
  res
)
;-------------------------------------------------------------
(defun l+collectPoints_on_pline (ss / pline i
				 
			      vertices vertex
			      pps pbref inspo 
			      res usedpbrefs
			     )

  

  (setq i -1)

  (repeat (sslength ss)
    (setq i (1+ i)
	  pline (ssname ss i)
	  vertices (l+assoc_all 10 (entget pline))
	  pline nil
    )
    
    (foreach vertex vertices
    ;they are already associate with each other ?
    ;(setq i -1)

    (foreach pps nezdss
      (setq pbref (car pps)
	    inspo (cdr(assoc 10 (entget pbref)))
      )

      ;(if (not(member pbref usedpbrefs))      
	(if (< (distance vertex inspo) 0.001)
	  (setq pline (cons pps pline)
		;usedpbrefs (cons pbref usedpbrefs)
	  )
	)
      ;)
    );foreach
    );foreach

    ;(if pline
      (setq res (cons (reverse pline) res)););otherwise, no powerpole found

  );repeat

  res
)
;-----------------------------------------------------------
;;; pointimport.LSP
;;; By Jimmy Bergmark, JTB World, www.jtbworld.com 2013-03-29
;;; Rev 2015-09-06

(defun c:pi (/ *error* l*sshighlight l*savevars
	     fn fdata row strings featuredata inslay
	     split createlayer createstyle
	     noinsert vl 	     
	     brefs bref importNEZD
	     ;number inspo desc elv curpoints ;<<< local shared variable
	     aOverride aSkip prekey
	    )

  (l+begin)
  (initget "Yes No")
  (setq noinsert (getkword "Do you want to insert symbols [Yes/No] <Yes>: "))
  (setq noinsert (= noinsert "No"))

  (initget "Yes No")
  (setq importNEZD (getkword "\nDo you want import points as blocks [Yes/No] <No>: "))
  (setq importNEZD (= importNEZD "Yes"))


  (if importNEZD
    (progn
      (setq bn "nezd point")
      (if (tblsearch "block" bn)
	nil
	(progn
	  (setq bn "nezd point.dwg")

	  (if (findfile bn)
	    nil
	    (progn
	      (setq importNEZD nil)
	      (prompt "\nCan not find NEZD point in support paths.")	      
	    )
	  )
	);progn
      );if
    );progn
  );if
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
  (setq	fn   "C:\\AUTOCAD-SETUP CG\\CG_LISP\\feature codes_blocks.csv"
	path "C:\\AUTOCAD-SETUP CG\\BLOCKS\\symbols\\"
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
  
  (if (setq fn (getfiled "Select a CSV File" (getvar "DWGPREFIX") "csv" 0))
    (setq fdata (open fn "r"))
  )
  ;(setq curpoints (l+getCurrentPoints));this will make alot of redundant points

  (while (and fdata (setq row (read-line fdata)))
    
    (setq strings (split row))	  
	  
    
    (if	(progn (>= (length strings) 5)
	  (setq inspo (list (atof (nth 2 strings))
			    (atof (nth 1 strings))
			    0.0
		      )
		number (car strings)
		elv    (nth 3 strings)
		desc   (nth 4 strings)
	  )
	  
	  (l+checkImportingPoints); number inspo desc elv curpoints <<< shared variables, will be changed
	  ;if identical >>> return nil >>> skip
	);progn
      (progn

	(if (not importNEZD)
	(progn					; point number
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
		       inspo
		 )
					; text height
		 (cons 40 1.5)
					; text value
		 (cons 1 number)
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
		       inspo
		 )
					; text height
		 (cons 40 1.5)
					; text value
		 (cons 1 desc)
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
		       inspo
		 )
					; text height
		 (cons 40 1.5)
					; text value
		 (cons 1 elv)
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
	)

	;else: import points as nezd blocks
	(progn
	  
	  (command "._-INSERT"
		   "nezd point.dwg"		   
		   (l+transw2c inspo nil)
		   1		   
	  )

	  (while (>(getvar "cmdactive") 0)(command ""))

	  (setq bref (entlast)
		vl (entget bref)
		;move block to MSPoint elevation, but keeping attribute on 0 plan:
		vl (subst (cons 10 (list (car  inspo)
					 (cadr inspo)
					 (atof (nth 3 strings))
				   )
			  )
			  (assoc 10 vl)
			  vl
		   )
	  )

	  (entmod vl)	  

	  (l+getAtt3 bref "DESCRIPTION" desc)
	  (l+getAtt3 bref "NUMBER" number)
	  (l+getAtt3 bref "ELEVATION" elv)
	  
	)
	)


	
					
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
          ((= bn "PP_ANNO.DWG") "ATCO-PROPOSED")
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
	  (progn
	  (command "._-INSERT"
		   (strcat path bn)
		   
		   (l+transw2c inspo nil)
		   5
		   ""
		   ""
	  )
	  (setq brefs (cons (list (entlast) inspo desc number) brefs))
	  )
	);if
	));if noinsert
      )
    )
  )

  
  (if fdata (close fdata))
  
  (command "._-LAYER" "_S" "0" "")

  (if brefs
    (l+pi_rotateAnchors brefs)
  )
  ;(princ)
  (l+end)

  ;(C:FindDupplicates)
)
;------------------------------
(defun l+checkImportingPoints (/			       
			       ssNu ssPo ssDe ssEV ssBL i
			       _findSamePosPoints
			       _findSameNumberPoints
			       table continue identical

			       table1 table2 key cinspo
			       row ents ent value ar promptstr
			      )
  ;all can be modified

  ;1st: find identical >> exit loop
  ;2nd: find same number
  ;3nd: find same X, Y
  ;>>> it can be no need curpoints, just find the right thing for each imported row !!!
  (setq ssNu (ssget "ALL" (list (cons 0 "TEXT")   (cons 8 "Z-POINTNUMBER") (cons 1 number)))
	ssPo (ssget "ALL" '((0 . "POINT")  (8 . "Z-MSPOINT")))	
	ssDe (ssget "ALL" '((0 . "TEXT")   (8 . "Z-DESCRIPTION")))
	ssEV (ssget "ALL" '((0 . "TEXT")   (8 . "Z-ELEVATION")))

	ssBL (ssget "ALL" '((0 . "INSERT") (2 . "NEZD POINT")))	
	i -1
  )

  ;							;
  (defun _findSameNumberPoints (/ i table bref bnumber el cinspo desc elv row)
    (setq table (_groupPoints ssPo ssNu ssDe ssEV))
    ;(since ssNu is all Z-POINTNUMBER of number
    ;this table is dupplicated collection of number)
    
    (setq i -1)
    ;nezd:
    (repeat (if ssBL (sslength ssBL) 0)
      (setq i (1+ i)
	    bref (ssname ssBL i)
	    bnumber (l+getAtt3 bref "NUMBER" nil)
      )

      (if (= (strcase bnumber)(strcase number))
	(setq el (entget bref)
	      cinspo  (cdr (assoc 10 el))
	      desc   (l+getAtt3 bref "DESCRIPTION" nil)      
	      elv    (l+getAtt3 bref "ELEVATION" nil)
	      row (list bnumber cinspo desc (list bref) elv)
	      table (cons row table)
	)
      )
    )
    
    table
  )
  ;							;
  (defun _findSamePosPoints (/ sspo2 i po el cinspo ssNuAll table)
    (setq i -1 sspo2 (ssadd))
    
    (repeat (if sspo (sslength sspo) 0)
      (setq i (1+ i)
	    po (ssname sspo i)
	    el (entget po)
	    cinspo (cdr (assoc 10 el))
      )

      (if (< (distance (list (car inspo) (cadr inspo))
		       (list (car cinspo)(cadr cinspo))
		       )
	     0.001
	  )
	(ssadd po sspo2)
      )      
    )

    (setq ssNuAll (ssget "ALL" '((0 . "TEXT")   (8 . "Z-POINTNUMBER"))))
    (setq table (_groupPoints ssPo2 ssNuAll ssDe ssEV))
  )  
  ;							;


  ;MAIN 						;
  (setq table1 (_findSameNumberPoints)
	continue T
  )

  (foreach row table1
    (setq cinspo (cadr row))    
    (if (< (distance (list (car inspo) (cadr inspo))
		     (list (car cinspo)(cadr cinspo))
		     )
	   0.001
	)
      (setq identical T)
    )
  )
  
  (if identical ;it is the number and XY are identical. other might different
    (progn
      (setq continue nil)
      (prompt (strcat "\nSkipped: There is existing point of similar number, north, east like importing point " number))
    )
    (progn
      (setq table2 (_findSamePosPoints))

      (if (or table1 table2)
	(setq promptstr (strcat "\nPoint " number " got dupplicated: "
		)
	)
      )
      
      (if table1;there is/are same number(s)
	(setq promptstr (strcat promptstr (itoa (length table1)) " same number"))
      )

      
      (if table2	
	(setq promptstr (strcat promptstr (if table1 ", " "") (itoa (length table2)) " same position"))
      )

      (if (or table1 table2)
	;Ask
	(progn

	  (l+highlight l*sshighlight 0)
	  
	  (foreach row (append table1 table2)
	       (setq ents (nth 3 row))
	       (foreach ent ents (l+highlight ent 1))
	  )

	  (if aOverride
	    (setq key "AlwaysOverride")
	    (if aSkip
	      (setq key "alwaySkip")
	      (progn
		(initget "Override Skip Rename AlwaysOverride alwayssKip")
		(if (null prekey)(setq prekey "Override"))
		(setq key (getkword (strcat promptstr " [Override/Skip/Rename/Always override/always sKip]<" prekey ">: ")))
		(if (null key)(setq key prekey)(setq prekey key))		
	      )
	    )
	  )

	  
	  (if (= key "AlwaysOverride")
	    (setq aOverride T
		  key "Override"
	    )	    
	  )
	  
	  (if (= key "alwayssKip")
	    (setq aSkip T
		  key "Skip")	    
	  )

	  (cond
	    ((= key "Override")	     
	     (foreach row (append table1 table2)
	       (setq ents (nth 3 row))
	       (foreach ent ents (entdel ent))
	     )
	     (prompt "\nOld points deleted.")
	    )

	    ((= key "Skip")(setq continue nil))
	    ((= key "Rename")
	     ;enter new name or description
	     ;(initget 1)
	     (setq value (getstring T (strcat "\nRename point " number ": Enter new point number > description: "))
		   value (l+trim value)
	     )

	     (if (/= "" value)
	       (setq ar (l+split value ">")
		     ar (l+trim ar)
		     number (car ar)
		     desc (if (cadr ar)(cadr ar) desc)
	       )
	     )	     
	     
	    )
	  )	
	);progn
      )
    )
  )
  

  (if (= key "Rename")
    (l+checkImportingPoints);recursive
    continue
  )
)
;------------------------------
(defun l+getCurrentPoints (/ ss i bref el inspo desc number elv
			   row table
			   po ipo nu de ev
			  )
  ;both NEZD points and texts points !!!
  ;>>> table of Nu, N, E, Z, D, ss
  (setq ss (ssget "ALL" '((0 . "INSERT")(2 . "NEZD point")))
	i -1
  )

  (repeat (if ss (sslength ss) 0)
    (setq i (1+ i)
	  bref (ssname ss i)
	  el (entget bref)
	  inspo (cdr(assoc 10 el))
	  desc (l+getAtt3 bref "DESCRIPTION" nil)
	  number (l+getAtt3 bref "NUMBER" nil)
	  elv (l+getAtt3 bref "ELEVATION" nil)
	  row (list number inspo desc (list bref) elv)
	  table (cons row table)
    )    
  )

  (setq ssPo (ssget "ALL" '((0 . "POINT")(8 . "Z-MSPOINT")))
	ssNu (ssget "ALL" '((0 . "TEXT") (8 . "Z-POINTNUMBER")))
	ssDe (ssget "ALL" '((0 . "TEXT") (8 . "Z-DESCRIPTION")))
	ssEV (ssget "ALL" '((0 . "TEXT") (8 . "Z-ELEVATION")))
	i -1
  )
  
  (setq table (append (_groupPoints ssPo ssNu ssDe ssEV) table))
  
  (reverse table)
)
;-----------------
(defun _groupPoints (ssPo ssNu ssDe ssEV / i po el inspo ipo Nu De Ev
		     number desc elv row table
		    )
  ;find group of Point, Point Number, Description, Elevation texts in ssPo, ssNu, ssDe, ssEV
  ;>>> group them in to 1 record
  ;>>> make a table of every records
  (setq i -1)
  (repeat (if sspo (sslength ssPo) 0)
    (setq i   (1+ i)
	  po  (ssname ssPo i)
	  el  (entget po)
	  inspo (cdr(assoc 10 el))
	  ipo (list (car inspo)(cadr inspo) 0.)
    )

    (if (and (setq Nu (_findtext ipo ssNu))
	     (setq De (_findtext ipo ssDe))
	     (setq Ev (_findtext ipo ssEV))
	)
      
      (setq el (entget nu)
	    number (cdr (assoc 1 el))
	    el (entget de)
	    desc (cdr (assoc 1 el))
	    el (entget ev)
	    elv (cdr (assoc 1 el))
	    
	    row (list number inspo desc (list po nu de ev) elv)
	    table (cons row table)
      )
    )
  )

  table
)
;-----------------
(defun _findtext (ipo sstext / text i el inspo dist mindist
		    mintext
		   )
    ;it can be used with sspoints as well
    (setq i -1)
    
    (repeat (if sstext (sslength sstext) 0)
      (setq i (1+ i)
	    text (ssname sstext i)
	    el (entget text)
	    inspo (cdr (assoc 10 el));ext can be at different alignment >> dxf 10 or 11
	    inspo (list (car inspo)(cadr inspo) 0.)
	    dist (distance ipo inspo);ipo is flatten
      )
      
      (if (and (< dist 0.1) (or (null mindist) (> mindist dist)))
	(setq mindist dist
	      mintext text
	)
      )
    )

    mintext
)
;----------------------------------------
(defun l+findtextAlignmentPoint (el / f7)
  (setq f72 (cdr (assoc 72 el))
	f73 (cdr (assoc 73 el))
  )

  (if (or (null f72)(= f72 73 0))
    (cdr(assoc 10 el));text can be at different alignment
    (cdr(assoc 11 el))
  )
)

;----------------------------------------------------------
(defun l+ss2list (ss / i ent res)
  (setq i -1)
  
  (repeat (if ss (sslength ss) 0)
    (setq i (1+ i)
	  ent (ssname ss i)
	  res (cons ent res)
    )    
  )

  (reverse res)
)
;-----------------------------------------------------------
(defun l+pi_rotateAnchors (brefs / bref inspo desc number
			   anchor anumber 
			  )
  ;(setq brefs (cons (list (entlast) inspo desc) brefs))
  (foreach bref brefs
    (setq desc (caddr bref)
	  desc (strcase desc)
	  number (nth 3 bref)
	  number (atoi number)
	  inspo(cadr  bref)
	  bref (car   bref)
    )

    (if (or (wcmatch desc "PP*")
	    (wcmatch desc "EXPP*")
	)
      (foreach anchor brefs
	(setq desc (caddr anchor)
	      desc (strcase desc)
	      anumber (nth 3 anchor)
	      anchor (car   anchor)
        )

	(if (or (wcmatch desc "ANC*")
		(wcmatch desc "EXANC*")
	    )
	  (if (= (atoi anumber) number)
	    (l+adjust_anchorRot anchor inspo)
	  )
	)	
      )
    )
  )
)
;-----------------------------------
(defun C:PPL (/ *error* l*sshighlight l*savevars
	      nezdss layer
	      ss i pline res pps ppbref text desc inspo flagNoAnchor
	     )
  (l+begin)
  (prompt "Select AS-POWER-GTECH polylines: ")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE")(cons 8 "AS-POWER-GTECH"))))
  
  (if ss
    (progn

      (setq nezdss (l+selectNEZDpoints ss));
      
      (setq nezdss (l+groupNEZDpoints nezdss T));list of (ppref anchor1 anchor2 ...)

      (setq res (l+collectPoints_on_pline ss))
      
      (foreach pline res
	(foreach pps pline
	  (setq ppbref (car pps)
		desc   (l+getatt3 ppbref "DESCRIPTION" nil)
		desc (strcase desc)
	  )

	  (if (or (wcmatch desc "PP_*");this is a powerpole
		  (wcmatch desc "EXPP_*")
	      )              
	  
	    (progn
	      (setq inspo (cdr(assoc 10 (entget ppbref)))
		    text  (l+getatt3 ppbref "NUMBER" nil)
		    inspo (list (car inspo)(cadr inspo) 0.)
	      )

	      (if (wcmatch desc "PP_*")
		(setq text (strcat "STR #" text)
		      layer "AS-POWER-STAKED-T"
		)
		(setq layer "AS-POWER-EXISTING-T")
	      )

	      (l+PPL_AddText inspo text layer)
	    );progn
	   );if	   
	)
      )
    )
  )
  (l+end)
)
;---------------------------------------------------------------
(defun l+PPL_AddText (inspo text layer / el)
  ;text height = 10
  ;italics & throw a background hatch on it
  (if (not (tblsearch "LAYER" layer))
    (command "._-layer" "_make" layer "")
  )
  
  (setq text (strcat "{\\fBook Antiqua|b0|i1|c0|p18;" text "}")
	;inspo (l+transc2w inspo nil); this is entmake and entget, all points in WCS already
  )

  (setq el (list (cons 0 "MTEXT")		 
		 (cons 100 "AcDbEntity")
		 (cons 8 layer)		 
		 (cons 100 "AcDbMText")
		 (cons 10 inspo)
		 (cons 40 10);height
		 (cons 41 0.);reference rectang width
		 (cons 46 0.)		 
		 (cons 71 1);attachment point
		 (cons 72 5);direcction = bystyle
		 (cons 1 text)
		 (cons 7 (getvar "TEXTSTYLE"))
		 (cons 210 '(0 0 1.))		 
		 (cons 11 '(0 0 0))		 
		 ;adjustment:
		 (cons 42 0.);defined width
		 (cons 43 0.);defined height
		 (cons 50 0.)
		 (cons 73 1)
		 (cons 44 1.)
		 ;background:
		 (cons 90 3);use drawing background
		 (cons 63 256);background fill color
		 (cons 45 1.1)
		 (cons 441 0)
	   )
  )

  (if (entmake el)  (entlast))
  
  ;move the text up or down to find a good space
  ;... it should be move around ... Cancel !  
  ;(setq count 0)
  ;(while (< count 9)
  ;  (setq count (1+ count))
  ;)
)

;-----------------------------------------------------------
(defun l+roundUp (value /)
  (if (= value (fix value)) value (fix (+ 1 value)))
)
;-----------------------------------------
(defun l+Assoc_All (code entl / return el)
  (foreach el entl
    (if
      (= (car el) code)
      (setq return (cons (cdr el) return))
      );if
    );foreach

  (reverse return)
)
;-------------------------------------
(defun l+DimScale ()
  (if (= 1 (getvar "DimAnno"))
    (/ 1 (getvar "cannoscalevalue"))
    (getvar "DimScale")
  )
)
;ERROR HANDLE FUNCTIONS ------------------------------------
(defun l+begin (/ l+undo_init undo_ctl)
  (defun *error* (msg /)
    (if command-s;(> (atof (getvar 'acadver)) 19.0)
      (progn
	;it depends: sometime it is good to undo all thing. sometime, just undo 1 step back.
	(command-s "_.undo" "_back")
	(command-s "_.undo" "_end")
	;(command-s "_.U")
      )
      (command "_.undo" "_back" "_.undo" "_end")
    )    

    ;(prompt (strcat "\n" msg "\n"));debug purpose
    
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))    )
    
    (l+highlight l*sshighlight 0)
    (setq l*sshighlight nil)
    (mapcar 'setvar (list "cmdecho" "osnapcoord") l*savevars)
    (princ)
  );defun *error*
  
  (defun l+undo_init (/ undo_ctl)
  ;(l+b_set_sysvars (list "cmdecho" 0))
  (setq undo_ctl (getvar "undoctl"))

  ;1st. Make sure undo is fully enabled:
  (if (equal 0 (getvar "UNDOCTL")) 
    (command "_.undo" "_all")  )

  (if (or (not (equal 1 (logand 1 (getvar "UNDOCTL"))))
          (equal 2 (logand 2 (getvar "UNDOCTL")))      )
    (command "_.undo" "_control" "_all")  )

  ;2nd. Ensure undo auto is off:
  (if (equal 4 (logand 4 (getvar "undoctl")))
    (command "_.undo" "_Auto" "_off")  )

  ;3rd. Place an end mark down if needed:
  (while (equal 8 (logand 8 (getvar "undoctl")))
    (command "_.undo" "_end")  )

  ;4th. Start new undo group:
  (while (not (equal 8 (logand 8 (getvar "undoctl"))))
    (command "_.undo" "_begin")  )

  ;(l+b_restore_sysvars) 
  ;return original value of undoctl
  undo_ctl
  );defun l+undo_init

  ;MAIN:
  (setq l*sshighlight nil)
  (setq l*savevars (mapcar 'getvar (list "cmdecho" "osnapcoord")))

  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)

  (setq undo_ctl (l+undo_init)) ;= setting up undoctl + (command "_.undo" "_begin")
)
;------------------------------------
(defun l+end ()
  (l+highlight l*sshighlight 0)
  (setq l*sshighlight nil)
  (command "_.undo" "_end")
  (mapcar 'setvar (list "cmdecho" "osnapcoord") l*savevars)
  (princ)
)
;---------------------
(defun l+highlight (ent flag / entx newent i_ent i el)
  (setq i -1)
  (if (null l*sshighlight)(setq l*sshighlight (ssadd)))
  
  (cond
    ((null ent) nil)
    ((= 'ENAME (type ent))
    (progn
      (setq el (entget ent)
	    etype (cdr(assoc 0 el))
	    )
      
      (if (= "ATTRIB" etype)
	(setq ent (cdr(assoc 330 el)))
      )
	
      (setq entx (vlax-ename->vla-object ent))
      (if entx (vlax-invoke-method entx 'Highlight flag))
      (if flag
	(ssadd ent l*sshighlight)
      )
    ))    
    
    ((repeat (sslength ent)
      (setq i (1+ i)
	    i_ent (ssname ent i)	    
	    )
      (l+highlight i_ent flag)
    ))
  )
)


;--------------
(defun l+curspace (/ acadapp thisdrawing space curspace)
  (setq acadapp     (vlax-get-acad-object)
	thisdrawing (vlax-get-property acadapp 'ActiveDocument)
	curspace    (vlax-get-property
		      (vlax-get-property thisdrawing 'ActiveLayout)
		      'Block
		    )
	space   (vla-get-activespace thisdrawing)
	curpace (if (and (= space 0)(= :vlax-false (vla-get-mspace thisdrawing)))
		    curspace
		    (vla-get-modelspace thisdrawing)		  
		)
  )
)
;-----------------
(defun l+GetCusDwgProp (doc Propname DefaultValueIfNone newvalue / suminfo res)
  (if (null doc) (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))))
  
  (setq suminfo (vla-get-summaryinfo doc))

  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-getcustombykey (list suminfo propname 'res)))
    (if defaultvalueifnone
      (progn
	(vla-addcustominfo suminfo propname defaultValueIfNone)
	(setq res defaultvalueifnone)	
      )
      (setq res nil)
    )    
  )  

  (if newvalue
    (if (vl-catch-all-error-p (vl-catch-all-apply  'vla-setcustombykey (list suminfo propname newvalue)))
      (vla-addcustominfo suminfo propname newvalue)
    )
  )
  ;but alway return the old value:
  res  
)
;------------------------------------------------
(defun l+ListReplace (li newItem index / _loop i)
  (setq i -1)
  (defun _loop (x /)
    (setq i (1+ i))
    (if (= i index) newItem x)    
  )
  (mapcar '_loop li)
)

;------------------------------------------------

(defun l+CutLeft (str n /)
  (substr str (1+ n) (strlen str))
)

;---------------------------------
(defun l+CutRight (str n /)
  (substr str 1 (- (strlen str) n))
)
;----------------------------------
(defun l+transC2W (point vector /)
  (trans point 1 0 vector)
)
;----------------
(defun l+transW2C (point vector /)
  (trans point 0 1 vector)
)
;------------------------------------------------------------
(defun l+GetAtt3 (ent tag value / att atl flag name atag res newatl)
  ;new version of l+GetAtt should not conflict with old version l+GetAtt
  ;(setq ent (car (entsel)))
  (setq flag T
	ent (entnext ent)
  )
  
  (while (and ent flag)
    (setq 
	  atl (entget ent)
	  name (cdr (assoc 0 atl)))
    (if (or(= name "SEQEND")(/= name "ATTRIB"))

      (setq flag nil)
      ;ELSE IF
      ;(if (= name "ATTRIB")
      (progn
	(setq atag (cdr (assoc 2 atl)))

	(if (= (strcase atag)  (strcase tag))
	  (progn
	  (setq res (cdr (assoc 1 atl))
		flag (if value T nil); if value, continue to assign it
	  )

	  (if value
	    (if (= value 'ENAME)
	      (setq res ent)

	      (progn
		;old method: entmod:
		;(setq newatl (subst (cons 1 value) (assoc 1 atl) atl))
		;(entmod newatl)
		;<< sometime the (7 . TextStyle) is removed !!! ???
		;new method with VL-COM:
		(setq newatl (vlax-ename->vla-object ent))
		(vlax-put-property newatl 'TextString value)
		(entupd ent)
	      );progn
	    )
	  );if
	  );progn
	  );(if (= (strcase atag)  (strcase tag))
	););(if (or (= name "ATTRIB")(= name "ATTDEF"))
      
      )

    (setq ent (entnext ent))
  )

  res
)
;------------------------------------------------------
(defun v_x_r (vec r / x re)

  (foreach x vec
    (setq x (* x r)
          re (cons x re)
    )
  )

  (reverse re)
)
;----------------
(defun v_plus_v (vec1 vec2 / l i x1 x2 re)
  (if (null vec1)
    vec2

    (progn

      (setq i -1
	    l (max (length vec1) (length vec2))
      )

      (repeat l
	(setq i (1+ i)
	      x1 (nth i vec1)
	      x2 (nth i vec2)
	)

	(if (null x1) (setq x1 0))
	(if (null x2) (setq x2 0))
	(setq re (cons (+ x1 x2) re))
      )

      (reverse re)
    )
  )
)
;----------------
(defun v_div_r (vec r /)
  (v_x_r vec (- r))
)
;---------------
(defun v_minus_v (vec1 vec2 / )
  (v_plus_v vec1 (v_x_r vec2 -1))
)
;------------------
(defun l+Vector (A B /)
  (v_minus_v B A)
)


;------------------------------------------------------------------------
(defun l+getdyninfo (bref / entx return n dps dyn i pname value)
  (setq entx (vlax-ename->vla-object bref))
  (setq dps (vlax-invoke-method entx "GetDynamicBlockProperties")
	dps  (vlax-variant-value dps)
	n (1+ (vlax-safearray-get-u-bound dps 1))
	;n (vlax-safearray-get-dim dps)
	i 0
  )
  (repeat n
    (setq dyn (vlax-safearray-get-element dps i)
	  pname (vlax-get-property dyn "PropertyName")
    	  value (vlax-get-property dyn "Value")
    	  value (vlax-variant-value value)
    )
    (if (=(type value) 'safearray)
      (setq value (vlax-safearray->list value))
    )
    (setq return (cons (list dyn pname value) return )
    	  i (1+ i)
    )
  )
  return
)
;------------------------------------------------------------------------
(defun l+getDynProp (bref DName / dyninfo d return)
  (setq dyninfo (l+getdyninfo bref))
  
  (foreach d dyninfo
    (if (= (strcase(cadr d)) (strcase DName))
      (setq return (caddr d))
    )
  )
  return
)
;------------------------------------------------------------------------
(defun l+letDynProp (bref DName newV / dyninfo d)
  (setq dyninfo (l+getdyninfo bref))

  (if (= (type newV) 'INT)(setq newV (* newV 1.)))

  (foreach d dyninfo
    (if (= (strcase(cadr d)) (strcase DName))
      (vla-put-value (car d) newV)
    )
  )
)

;----------------------
(defun l+Split (txt delimiter / artexts text i txtlength ch);from Ycad
  (if (null delimiter)(setq delimiter (chr 9)))
  
  (setq txtlength (strlen txt) i 0 text "" artexts (list))
  
  (setq dlength (strlen delimiter))
  
  (repeat txtlength
    (setq i (1+ i)
	  ch (substr txt i dlength)
    )
    
    (if (/= ch delimiter)
      (setq text (strcat text ch))
      (progn	
	(setq text (l+Trim text))	
	(setq artexts (cons text artexts)
	      text ""
	)
      )
    )    
  )
  
  (if (/= text "") (setq artexts (cons text artexts)));the last field in the record
  
(reverse artexts)
)


;-----------------------------------------------
(defun l+Trim (str / i l m1 m2 f ch)
  (cond
    ((listp str)
     (mapcar 'l+Trim str)
    )
    
    (T
     (setq l (strlen str)
	   i 0
	   f T
	   m1 1 m2 l
     )


     (while f (setq i (1+ i)  ch nil)
       (if (> i l) (setq f nil) (setq ch (substr str i 1)))
       (if (/= ch " ") (setq m1 i f nil))
     )

     (setq i (1+ l) f T)

     (while f (setq i (1- i)  ch nil)
       (if (= i 0) (setq f nil) (setq ch (substr str i 1)))
       (if (/= ch " ") (setq m2 i f nil))
     )

     (substr str m1 (- m2 m1 -1))
     )
  );cond
)

;--------------------------
(defun l+OpenFile (filepath / shlobj)
  
   (setq
     ShlObj (vla-getInterfaceObject
             (vlax-get-acad-object)
             "Shell.Application"
           )
   )
   (vlax-invoke-method ShlObj 'ShellExecute filepath)
   (vlax-release-object ShlObj)
)

;----------------------
(princ)