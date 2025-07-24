;;; CrossingExtractor.LSP ver 1.4
;;; Command name is CE

;;; By Jimmy Bergmark
;;; Copyright (C) 2013 JTB World, All Rights Reserved
;;; Website: www.jtbworld.com
;;; E-mail: info@jtbworld.com
;;; 2013-05-17 - First release

(vl-load-com)
(defun c:ce (/	       ActiveDocument	   mSpace    strBlockName
	     sset      attdata	 en	   br	     atts
	     tag       tagstring pt	   str	     cross
	     own       descr	 loca	   DWG_REF attdata   myTable
	     nRows     nCols	 row	   cell	     blkID
	     blkObj    eblk	 attributes
	    )
  (vl-load-com)
  
  (defun parse-crossing-attribute (crossing)
    (if (= (substr crossing 1 1) "X")
      (setq crossing (substr crossing 2))
    )
    (atoi crossing)
  )

  (setq ActiveDocument (vla-get-activedocument (vlax-get-acad-object)))
  (setq mSpace (vla-get-modelspace ActiveDocument))
  (setq strBlockName "XING2")
  (if (not (tblsearch "block" strBlockName))
    (progn ;;if it is not found, alert user and exit program
	   (alert "Block \"XING2\" does not exist. Exit program.")
	   (exit)
	   (princ)
    )
  )

  ; Select objects
  (setq sset (ssget '((0 . "INSERT") (2 . "XING2"))))
  (if (not sset)
    (progn
      (prompt "\nNo objects found matching selection criteria. Proceeding with an empty selection.")
      ;; You can choose to exit the program here or continue processing
      ;; For demonstration purposes, we'll continue processing with an empty selection
    )
  )

  (setq attdata nil)
  (if sset
    (progn
      (while (setq en (ssname sset 0))
        (setq br (vlax-ename->vla-object en))
        (if	(and (= (vla-get-hasattributes br) :vlax-true)
                 (safearray-value
                   (setq atts (vlax-variant-value (vla-getattributes br)))
                 )
            )
          (foreach tag (vlax-safearray->list atts)
            (setq tagstring (strcase (vla-get-tagstring tag)))
            (setq str (vla-get-TextString tag))
            (cond ((= tagstring "CROSSING") (setq cross str))
                  ((= tagstring "OWNER") (setq own str))
                  ((= tagstring "DESCRIPTION") (setq descr str))
                  ((= tagstring "LOCATION") (setq loca str))
                  ((= tagstring "DWG_REF") (setq DWG_REF str))
            )
          )
        )
        (setq attdata (cons (list cross own descr loca DWG_REF) attdata))
        (ssdel en sset)
      ) ; end while
    )
  )

  ; Sort list by Crossing Attribute (numeric part)
  (setq
    attdata (vl-sort attdata
                     (function (lambda (e1 e2)
                                 (< (parse-crossing-attribute (car e1)) (parse-crossing-attribute (car e2)))
                               )
                     )
           )
  )

  (setq pt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  ; Insertion point for the table
  (vlax-safearray-fill
    pt
    (getpoint "\nSpecify insertion point of the block: ")
  )
  (setq myTable (vla-addtable mSpace pt (1+ (length attdata)) 5 26 100))
  (vla-put-layer myTable "CG-NOTES")
  (vla-put-color myTable acbylayer)
  (vla-put-regeneratetablesuppressed myTable :vlax-true)
  (vla-put-StyleName myTable "Standard")
  (vla-setColumnWidth myTable 0 50)
  (vla-setColumnWidth myTable 1 152)
  (vla-setColumnWidth myTable 2 428)
  (vla-setColumnWidth myTable 3 141)
  (vla-setColumnWidth myTable 4 141)
  (vla-put-HeaderSuppressed myTable :vlax-true)
  (vla-put-TitleSuppressed myTable :vlax-true)

  ; Rows and columns zero based
  (setq nRows (- (vla-get-rows myTable) 1))
  (setq nCols (- (vla-get-columns myTable) 1))

  ; Rows and columns after row 0, column 0
  (setq row 1)
  (setq cell 1)
  (setq blkID (vla-get-objectID
               (setq blkObj (vla-item (vla-get-blocks ActiveDocument)
                                      strBlockName
                             )
               )
             )
  )

  (vlax-for eblk blkObj
    (if (= (vla-get-objectname eblk) "AcDbAttributeDefinition")
      (if (= (vla-get-tagstring eblk) "CROSSING")
        (setq attid (vla-get-objectid eblk))
      )
    )
  ) ; loop through cells

  (while (<= row nRows)
    (setq attributes (nth (1- row) attdata))
    (vla-setcelltype myTable row 0 acBlockCell)
    (vla-setblocktablerecordid myTable row 0 blkID :vlax-false)
    (vla-setblockscale myTable row 0 1.0)
    (vla-setcellalignment myTable row 0 acMiddlecenter)
    (vla-setblockattributevalue
      myTable
      row
      0
      attid
      (nth 0 attributes)
    )
    (while (<= cell nCols)
      (vla-setCelltextHeight myTable row cell 9.5)
      (setq cellvalue (nth cell attributes))
      (if (not cellvalue)
        (setq cellvalue "")
      )
      (vla-settext myTable row cell cellvalue)
      (vla-setCellAlignment myTable row cell 5)
      (setq cell (1+ cell))
    ) ;while
    (setq row (1+ row))
    (setq cell 1)
  ) ;while

  (vla-DeleteRows myTable 0 1)
  (vla-put-regeneratetablesuppressed myTable :vlax-false)
  (princ)
) ;defun
