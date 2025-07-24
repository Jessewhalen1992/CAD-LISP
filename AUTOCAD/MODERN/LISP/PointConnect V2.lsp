;;; PointConnect ver 2.0
;;; Command name is PCON
;;; Connect points with polyline or 3D polyline
;;; if input is 1-10 and only 1,2 and 10 exists these three will be connected
;;; accepted input examples:
;;; 1-10
;;; 1,2,3
;;; 1-10,25,35-37

;;; By Jimmy Bergmark
;;; Copyright (C) 2014 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2013-08-04 - First release
;;; 2014-03-03 - Repeat command added
;;; 2015-10-21 - Searching for NEZD Point block
;;; 2016-11-12 : New command PCONALL
;;; 2016-11-25 : speed it up from 5000 points, 1 mins to ... 10s

(vl-load-com)
(setq PCONpath  "C:/AUTOCAD-SETUP/Lisp_2000/PCON.csv")
;------------------------------------------------------------------------------
(defun c:PCON (/ points	xyz pointlist list1 str	list2 from to curpoint p coord
	       String2List ax-pointconnect-getCoord getcoordNEZD
	       pointconnect-error pointconnect-old-error
	      )
  (l+begin1)

  (defun String2List (InpStr CarDlm / SttPos EndPos TmpLst);= split
    (setq CarDlm (ascii CarDlm)
	  SttPos 0
	  EndPos (vl-string-position CarDlm InpStr)
    )
    (while EndPos
      (setq TmpLst (cons (substr InpStr (1+ SttPos) (- EndPos SttPos)) TmpLst)
	    SttPos (1+ EndPos)
	    EndPos (vl-string-position CarDlm InpStr SttPos)
      )
    )
    (reverse (cons (substr InpStr (1+ SttPos)) TmpLst))
  )

  
  (defun ax-pointconnect-getCoord (ptext xyz / sset en coord)
    (setq sset (ssget "X"
		      (list (cons 0 "TEXT")
			    (cons 8 "Z-POINTNUMBER")
			    (cons 1 ptext)
		      )
	       )
    )
    
    (if	(and sset (> (sslength sset) 0))
      (progn (setq en (ssname sset 0))
	     (setq coord (assoc 10 (entget en)))
 ; find point with same coordinate but not same z
	     (setq sset	(ssget "X"
			       (list (cons 0 "POINT")
				     (cons 8 "Z-MSPOINT")
				     (cons -4 "=,=")
				     coord
			       )
			)
	     )
	
	     (if (and sset (> (sslength sset) 0))
	       (progn (setq en (ssname sset 0))
		      (setq coord (assoc 10 (entget en)))
		      (setq coord (cdr coord))
	       )
	     )
      )
    )
    
    (if	xyz
      coord
      (if coord
	(list (car coord) (cadr coord) 0.0)
	nil
      )
    )
  )

  (defun getcoordNEZD (ptext xyz / ss bref i att coord)
    (if (setq ss (ssget "X"
		      (list (cons 0 "INSERT")
			    (cons 2 "NEZD Point")			    
		      )
	       )
       )
      (progn
	(setq i -1)
	(while (and (null coord)
		    (< (setq i (1+ i)) (sslength ss))
	       )	
	
	  (setq bref (ssname ss i)
		att (l+getAtt3 bref "NUMBER" nil)
	  )
	  
	  (if (= att ptext)
	    (setq coord (cdr (assoc 10 (entget bref))))
	  )
	)
      );progn
    )

    (if	xyz
      coord
      (if coord
	(list (car coord) (cadr coord) 0.0)
	nil
      )
    )
  )

  ;MAIN:  
  (initget "Yes No")
  (setq kw (getkword "Include Z value? [Yes/No] <No>: "))
  (cond	((or (not kw) (= "No" kw)) (setq xyz nil))
	((= "Yes" kw) (setq xyz T))
  )

  (while (/= (setq points (getstring "\nEnter point series (e.g.: \"100,103,108-115\"): ")) "")
  
  (setq pointlist nil)
  (setq list1 (String2List points ","))
  (foreach str list1
    (setq list2 (String2List str "-"))
    (cond ((= (length list2) 1)
	   (setq pointlist (cons (atoi (car list2)) pointlist))
	  )
	  ((= (length list2) 2)
	   (setq from (atoi (car list2)))
	   (setq to (atoi (cadr list2)))
	   (setq curpoint from)
	   (repeat (if (> to from)
		     (- to from -1)
		     (- from to -1)
		   )
	     (setq pointlist (cons curpoint pointlist))
	     (if (> to from)
	       (setq curpoint (1+ curpoint))
	       (setq curpoint (1- curpoint))
	     )
	   )
	  )
	  (T (princ (strcat "\nError parsing " str)))
    )
  )
  (setq pointlist (reverse pointlist))
  (if (>= (length pointlist) 2)
    (progn (if xyz
	     (command "._3DPOLY")
	     (command "._PLINE")
	   )
	   (foreach p pointlist
	     (setq coord (ax-pointconnect-getCoord     (itoa p) xyz)
		   coord (if coord coord (getcoordNEZD (itoa p) xyz))
	     )

	     (if coord (setq coord (pcon-transw2c coord nil)))
	     
	     (if coord
	       (command "_non" coord)
	       (prompt (strcat "\nPoint not found: " (itoa p)))
	     )
	   )
	   (command "")
	   (if (/= (getvar "cmdnames") "")
	     (command "")
	   )
    )
  )
  )
  (princ)
  (l+end1)
)

;------------------------------------------------------------
(defun C:PCONALL (/ table ar i desc layer res kw xyz ssNEZDs ssTEXTs ss
		  ssRes listNEZDs bref atts
		 )
  (l+begin1)

  (prompt "\n")
  (initget "Yes No")
  (setq kw (getkword "Include Z value? [Yes/No] <No>: "))
  (cond	((or (not kw) (= "No" kw)) (setq xyz nil))
	((= "Yes" kw) (setq xyz T))
  )

  (prompt "\nSelect NEZD points or description texts: ")
  (setq ss (ssget (list
		    (cons -4 "<OR")
		    (cons -4 "<AND")
		    (cons 0 "INSERT")
		    (cons 2 "NEZD Point")
		    (cons -4 "AND>")
		    (cons -4 "<AND")
		    (cons 0 "TEXT")
		    (cons 8 "Z-DESCRIPTION")
		    (cons -4 "AND>")
		    (cons -4 "OR>")
		  )
	   )	        
  )

  (if (null ss)(exit))
  
  (setq i -1
	ssNEZDs (ssadd)
	ssTEXTs (ssadd)
	ssres (ssadd)
  )

  (repeat (sslength ss)
    (setq i (1+ i)
	  ent (ssname ss i)
	  el  (entget ent)
	  en  (cdr(assoc 0 el))
    )
    (if (= en "INSERT")
      (ssadd ent ssNEZDs)
      (ssadd ent ssTEXTs)
    )
  )

  
  
  (if (setq table (l+pcon-table1))
    (progn
      (setq i -1)
      (repeat (if ssNEZDs (sslength ssNEZDs) 0)
	(setq i (1+ i)
	      bref (ssname ssNEZDs i)
	      atts (l+getatts bref); "Description" nil)
	      listNEZDs (cons (list atts bref) listNEZDs)
	)
      )

      (setq i 0)
      (foreach ar table
	(if (> i 0)
	  (setq desc  (car  ar)
		layer (cadr ar)
		res   (l+pconAll desc layer);will work with ssNEZDs and ssTEXTs
	  )
	)
	(setq i (1+ i))
      )
    )
    ;else:
    (alert (strcat "Can not get PCON.csv: " pconpath))
  )

  (prompt "\nDone. ")
  (l+end1)
  (sssetfirst nil ssres)
  (princ)
)
;------------------------------------------------------------
(defun l+pconAll (desc layer / ss i bref desc1 res number inspo ss1 text
		  el _sort
		  ar
		  removeNEZDs removeTexts item atts
		 )


  (setq i -1
	desc (strcase desc))

  (foreach item listNEZDs
    (setq atts (car item)
	  desc1 (cdr(assoc "DESCRIPTION" atts))
    )
    (if (wcmatch (strcase desc1) desc)
      (setq bref (cadr item)
	    number (cdr (assoc "NUMBER" atts)) ;(l+getatt3 bref "Number" nil)
	    inspo (cdr(assoc 10 (entget bref)))
	    res (cons (list inspo number desc1) res)
	    removeNEZDs (cons bref removeNEZDs)
      )
    )
  )
  
  (foreach bref removeNEZDs
    (ssdel bref ssNEZDs);this will make next process runs quicker
  )
  
  ;now the point
  (setq i -1)

  (repeat (if ssTEXTS (sslength ssTEXTS) 0)
    (setq i (1+ i)
	  text  (ssname ssTexts i)
	  el    (entget text)
	  inspo (cdr(assoc 10 el))
	  desc1 (cdr(assoc 1  el))
    )

    (if (wcmatch (strcase desc1) desc)
      (progn
	;find number						;
	(setq removetexts (cons text removetexts))
	
	(setq ss1	(ssget "X"
			       (list (cons 0 "TEXT")
				     (cons 8 "Z-POINTNUMBER")
				     (cons -4 "=,=")
				     (cons 10 inspo)
			       )
			)
	     )

	(if ss1 (setq text  (ssname ss1 0)
		      number (cdr(assoc 1 (entget text)))
		)
	  (setq number nil)
	)
	;							;
	(setq ss1	(ssget "X"
			       (list (cons 0 "POINT")
				     (cons 8 "Z-MSPOINT")
				     (cons -4 "=,=")
				     (cons 10 inspo)
			       )
			)
	     )

	(if ss1 (setq point (ssname ss1 0)
		      inspo (cdr(assoc 10 (entget point)));including Z coordinate
		)
	  ;(setq inspo nil)
	)
	
	;							;
	(if number
	  (setq res (cons (list inspo number desc1) res))
	)
      );progn      
    );if
  )    

  ;					;
  (foreach text removeTexts
    (ssdel text ssTEXTs);this will make next process runs quicker
  )

  ;					;
  (defun _sort (ar1 ar2 / n1 n2 i1 i2)
    (setq n1 (cadr ar1)
	  n2 (cadr ar2)
	  i1 (atoi n1)
	  i2 (atoi n2)
    )
    (if (= i1 i2)
      (< n1 n2)
      (< i1 i2)
    )
  )

  (if res (progn
  ;sort res
  (setq res (vl-sort res '_sort))
  
  ;at last, connect using 3D poly or 2D polyline, then move to layer
  (if (not(tblsearch "layer" layer))(command "-._layer" "_make" layer ""))

  (if xyz
    (command "._3DPOLY")
    (command "._PLINE")
  )	       
	       
  (foreach ar res
    (setq inspo (car ar))
    (if xyz nil (setq inspo (list (car inspo)(cadr inspo))))
    (setq inspo (pcon-transw2c inspo nil))    
    (command "_non" inspo)
  )

  (command "")
  (if (> (getvar "cmdactive")0)(command ""))

  (setq el (entget(entlast))
	el (subst (cons 8 layer)(assoc 8 el) el)
  )

  (entmod el)

  (command "._-layer" "_on" layer "thaw" layer "")
  (ssadd (entlast) ssres)
  ));if res
)

;------------------------------------------------------------
(defun l+PCON-Table1 (/ path ar fileID textline res)  

  (if (setq fileID (open pconpath "r"))
    (progn
      (while (setq textline (read-line fileid))
	(setq textline (l+trim textline)
	      textline (strcase textline t)
	)
	
	(if (/= textline "")
	  (progn
	    (setq ar (l+split textline ","))		  
	    
	    (if (> (length ar) 1)
	      (setq res (cons (l+trim ar) res))
	    )
	  )
	)
      );while
      
      (close fileID)
    );progn
  );if
  
  (reverse res)
)
















;COMMONS FUNCTIONS:
;----------------------------------------------------

;ERROR HANDLE FUNCTIONS ------------------------------------
(defun l+begin1 (/)
  (command-s "_.undo" "_begin")

  (setq pointconnect-old-error *error* *error* pointconnect-error)
  
  (defun pointconnect-error (msg /)
    (if command-s
      (command-s "_.undo" "_end")
      (command "_.undo" "_back" "_.undo" "_end")
    )        
    
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))
    )
    (setvar "osnapcoord" 1)
    (princ)
  );defun pointconnect-error
)
  
  
;------------------------------------
(defun l+end1 ()
  (command "_.undo" "_end")
  (setq *error* pointconnect-old-error)
  (princ)
)
;----------------------------
(defun l+Split (txt delimiter / artexts text i txtlength ch dlength)
  ;fix bug of delimeter longer than 1 char (14-04-2016)
  ;require fix:
  
  (if (null delimiter)(setq delimiter (chr 9)))
  
  (setq txtlength (strlen txt) i 0 text "" artexts (list))
  
  (setq dlength (strlen delimiter))
  
  (while (< i txtlength)
    (setq i (1+ i)
	  ch (substr txt i dlength)
    )
    
    (if (/= ch delimiter)
      (setq text (strcat text (substr txt i 1)))
      (progn
	;format text
	(setq text (l+Trim text))	
	(setq artexts (cons text artexts)
	      text ""
	      i (1-(+ i dlength))
	)

	;fix bug: "a,," split with "," delimiter >>> ("a","") only
	(if (= i txtlength)
	  (setq artexts (cons text artexts));case "a,b,c,,"
	)
      )
    )    
  )
  
  (if (/= text "") (setq artexts (cons text artexts)));the last field in the record
  
(reverse artexts)
)

;----------------------
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

;--------

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
(defun pcon-transW2C (point vector /)
  (trans point 0 1 vector)
)
;-------------------
;(setq res (list nil nil "a" "b" nil "c" "d" nil nil))
(defun l+trimList (res / list1 col)
  

  (foreach col res
    (if (or list1 col)
      (setq list1 (cons col list1))
    )
  )

  (setq res nil)

  (foreach col list1
    (if (or res col)
      (setq res (cons col res))
    )
  )

  res
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

;--------------------
;------------------------------------------------------------
(defun l+GetAtts (ent / tag att atl name atag res newatl)
  (setq ent (entnext ent))
  
  (while ent
    (setq atl (entget ent)
	  name (cdr (assoc 0 atl)))
    (if (or(= name "SEQEND")(/= name "ATTRIB"))
      (setq ent nil)
      
      (progn
	(setq atag (cdr (assoc 2 atl)))
	(setq res (cons (cons (strcase atag) (cdr (assoc 1 atl))) res))
	(setq ent (entnext ent))
      )
    )
  )

  res
)

;--------------------------------------------------
(defun l+insertblock2 (BName inspo scale rotation / inspox curspace insert space slock layer err)
  
  (setq curspace (l+curspace))

  (setq inspox (vlax-3d-point inspo))

  (if (not (tblsearch "BLOCK" bname))
    (setq bname (strcat bname ".dwg"))
  )

  (setq layer (getvar 'CLAYER)
	slock (l+unlocklayer layer T)
  )

  ; BricsCAD will insert just using Normal = 0 0 1. While AutoCAD uses currently Uz as normal !      
  (setq err (vl-catch-all-apply 'vlax-invoke-method (list Curspace 'InsertBlock inspox bname scale scale scale rotation))	
	err (vl-catch-all-error-p err)
  )

  (if (= slock :vlax-true) (l+unlocklayer layer nil))
  (if (null err)  (entlast))
)
;---------------------------------------------------
(defun l+unlocklayer (layer flag /  acadapp thisdrawing layers slock)
  	
  (setq acadapp (vlax-get-acad-object) ;<<< the root one left
	thisdrawing (vlax-get-property acadapp 'ActiveDocument)
	layers (vlax-get-property thisdrawing 'layers)	
	       
	layer (vlax-invoke-method layers 'item layer)	
  )

  (setq slock (vlax-get-property layer 'lock))

  (vlax-put-property layer 'lock (if flag 0 1))
  slock
)

;-------------------------------------------------
(defun l+StringShow (text cols /)
  ;show string in a specify length format: "abc" 8 >>> "abc     "
  (repeat cols (setq text (strcat text " ")))
  (substr text 1 cols)
)
;--------------------------
(princ)