;;; lto.LSP ver 1.0
;;; Command name is PLTO

;;; change all colors to white

 


(vl-load-com)
(defun c:plto (/ Layout str prevstr InsertionPoint ad texto)
  (defun layermerge (oldlay newlay)
    (if	(tblsearch "layer" oldlay)
      (progn
	(command "._-layer" "_u" oldlay "")
	(if (tblsearch "layer" newlay)
	  (progn
	    (command "._-layer" "_u" newlay "")
	    (command "._laymrg" "_n" oldlay "" "_n" newlay)
	    (if	(wcmatch (getvar "cmdnames") "*LAYMRG*")
	      (command "_yes")
	    )
	  )
	  (command "._rename" "_la" oldlay newlay)
	)
      )
    )
  )

;;; turn layers on
(command "layoN")
 (princ)

;;; thaw layers
(command "laythw")
 (princ)

;;; change all colors to white

(command "_.layer" "_co" "7" "*" "")
 (princ)

  

					; detach all images
  (command "-IMAGE" "D")
  (if (wcmatch (getvar "cmdnames") "*IMAGE*")
    (command "*")
  )
;;
(command "._-LAYER" "_S" "0" "")
;;;;;
;;; merge layers

  (layermerge "L-1" "1")
  (layermerge "L-2" "2")
  (layermerge "L-3" "3")
  (layermerge "L-4" "4")
  (layermerge "L-5" "5")
  (layermerge "L-6" "6")
  (layermerge "L-AREAREQUIRED" "7")
  (layermerge "P-PROPOSED" "8")
  (layermerge "C-RW" "9")
  (layermerge "C-RW-DASHED" "9")
  (layermerge "F-RW" "9")
  (layermerge "F-RW-DASHED" "9")
  (layermerge "L-SEC" "9")
  (layermerge "L-QSEC" "9")
  (layermerge "L-SECQ" "9")
  (layermerge "L-TITLEDB" "9")
  (layermerge "P-TRAV" "9")
  (layermerge "C-RW-T" "10")
  (layermerge "F-RW-T" "10")
  (layermerge "L-TITLEDB-T" "10")
  (layermerge "L-BD" "10")
  (layermerge "L-SECLBL" "10")
  (layermerge "C-SYMBOL" "10")
  (layermerge "L-PHANTOM" "11")
  (layermerge "L-ASCM-N-T" "12")
  (layermerge "L-ASCM-N" "12")
  (layermerge "L-MON" "13")
  (layermerge "L-ROAD-T" "15")
  (layermerge "L-21" "21")
  (layermerge "L-ASCM-TRUE" "25")
  (layermerge "L-ASCM-TRUE-T" "25")
  (layermerge "L-BD1" "26")
  (layermerge "P-TXT" "26")
  (layermerge "P-LTO" "L-31")
  (layermerge "T-WATER-L-T" "32")
  (layermerge "T-WATER-T" "32")
  (layermerge "L-33" "33")
  (layermerge "CG-NOTES" "34")
  (layermerge "L-TIE" "36")
  (layermerge "L-TIE-T" "36")
  (layermerge "L-MON-PLAN" "37")
  (layermerge "L-38" "38")


;;
 (command "-PURGE" "Blocks" "*" "N")
    (command "-PURGE" "Dimstyles" "*" "N")
    (command "-PURGE" "LTypes" "*" "N")
    (command "-PURGE" "MAterials" "*" "N")
    (command "-PURGE" "MUltileaderstyles" "*" "N")
    (command "-PURGE" "Plotstyles" "*" "N")
    (command "-PURGE" "SHapes" "*" "N")
    (command "-PURGE" "textSTyles" "*" "N")
    (command "-PURGE" "Mlinestyles" "*" "N")
    (command "-PURGE" "Tablestyles" "*" "N")
    (command "-PURGE" "Visualstyles" "*" "N")
    (command "-PURGE" "Regapps" "*" "N")
    (command "-PURGE" "Zero")
    (command "-PURGE" "Empty")
;;    (command "laydel" "Name" "Parcel Mapping" "" "Y")

;;
;;; Purge named layer
;;; Example: (ax:purge-layer (vla-get-activedocument (vlax-get-acad-object)) "testlayer")
;;; Argument: doc {document}
;;;           name {a layer name}
;;; Return values: T if successful, nil if not successful
  (defun ax:purge-layer	(doc name)
    (if	(vl-catch-all-error-p
	  (vl-catch-all-apply
	    'vla-delete
	    (list (vl-catch-all-apply
		    'vla-item
		    (list (vla-get-layers doc) name)
		  )
	    )
	  )
	)
      nil				; name cannot be purged or doesn't exist
      T					; name purged
    )
  )
;;;;;
 ; Delete everything on layer 0 and Defpoints
  (setq ss1 (ssget "X" (list (cons 8 "0"))))
  (if ss1 (command "._erase" ss1 ""))
  (setq ss1 (ssget "X" (list (cons 8 "Defpoints"))))
  (if ss1 (command "._erase" ss1 ""))
;;;
  (command "._-LAYER" "_S" "0" "_F" "1,2,3,4,5,6,7,8,9,10,11,12,13,15,21,25,26,28,31,32,33,34,35,36,37,38" "")
  (command "._ERASE" "_ALL" "")
  (command "._-LAYER" "_T" "1,2,3,4,5,6,7,8,9,10,11,12,13,15,21,25,26,28,31,32,33,34,35,36,37,38" "")
;;;

  (princ)
)