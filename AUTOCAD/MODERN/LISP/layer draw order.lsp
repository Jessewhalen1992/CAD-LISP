
(defun c:ldo (/ layersabove layersunder)
 ; Layers above so layer 1 is above layer 2
 ; (setq layersabove '("Layer1" "Layer2" "Layer3"))
  (setq layersabove '("L-MON" "P-APEX RECON PAY ITEMS" "P-WS-S" "C-WS-S" "F-WS-S" "P-CROSSINGS" "P-ELEVATION" "P-STATION" "AS-SYMBOL" "AS-POWER-EXISTING-PP" "AS-POWER-STAKED-PP" "AS-POWER-EXISTING-ANC" "AS-POWER-STAKED-ANC" "AS-POWER-STAKED-ANC" "AS-SYMBOL"
		      "AS-RESIDENCE" "0" "AS-OWNER" "T-TIE" "T-TIE-T" "P-TEMP-T" "P-DRILLPATH" "AS-TELEPHONE-T"
		      "AS-POWER-T" "AS-POWER-EXISTING-T" "AS-POWER-STAKED-T" "T-BUSH-T" "T-LOW-AREA-T" "T-LOW AREA-T" "T-WATER-T" "T-WATER-L-T" "T-TEXT" "P-TXT" " ATCO-EXISTING-T" "ATCO-PROPOSED-T" "L-TITLEDB-T"
		      "F-WS-T" "F-VCE-T" "F-RW-T" "R- URW-T" "F-FLOWLINE-T" "F-EZE-T" "F-AR-T" "C-WS-T" "C-RW-T"
		      "C-FLOWLINE-T" "C-AR-T" "L-BD" "L-BD1" "P-TEMP-BLOCKS" "L-TIE" "P-OFFSETS" "S-7" "CG-NOTES" "DETAIL-T" "AUX-BUFFER" "P-IOP20" "P-IOP19" "P-IOP18" "P-IOP17" "P-IOP16" "P-IOP15" "P-IOP14" "P-IOP13" "P-IOP12" "P-IOP11" "P-IOP10" "P-IOP9" "P-IOP8" "P-IOP7"
		      "P-IOP6" "P-IOP5" "P-IOP4" "P-IOP3" "P-IOP2" "P-IOP1" "AUX-LABELS" "P-STATION" "P-PROPOSED" "AS-GENERAL" "R- OTHER" "R- LOT" "P-TEMP_WORKSPACE" "P-WORKAREA"
		      "P-TEMP_REMOTE SUMP" "P-TEMP_PUSHOUT" "P-TEMP_LOG DECK" "P-TEMP_CAMP SITE" "P-WORKAREA-COUNTY"
		      "P-TEMP_BORROW PIT" "P-TEMP_BANK STABILIZATION" "P-TEMP_ACCESS ROAD" "P-BRUSHING"))
  
 ; Layers under so layer 4 is under layer 5
 ; (setq layersunder '("Layer5" "Layer4"))
  (setq layersunder '("T-BUSH" "AS-ROADS" "AS-TARGET-GAS-H" "AS-TARGET-OIL-H" "P-PROPOSED-H" "P-WORKAREA-H" "P-WORKAREA-COUNTY-H" "P-PROPOSED-EXISTING-H" "P-PHATCH" "P-IOP1-H" "P-IOP2-H" "P-IOP3-H"
		      "P-IOP4-H" "P-IOP5-H" "P-IOP6-H" "P-IOP7-H" "P-IOP8-H" "P-IOP9-H" "P-IOP10-H"
		      "P-IOP11-H" "P-IOP12-H" "P-IOP13-H" "P-IOP14-H" "P-IOP15-H" "P-IOP16-H"
		      "P-IOP17-H" "P-IOP18-H" "P-IOP19-H" "P-IOP20-H" "P-CROSSING-H1" "P-CROSSING-H2" "P-CROSSING-H3" "P-CROSSING-H4" "P-CROSSING-H5" "P-CROSSING-H6" "P-CROSSING-H7" "P-CROSSING-H8" "P-CROSSING-H9"
"P-CROSSING-H10" "P-CROSSING-H11" "P-CROSSING-H12" "P-CROSSING-H13" "P-CROSSING-H14" "P-CROSSING-H15" "P-TEMP-H" "P-HUSKY 30-H" "P-FNC BUFFER-H" "P-BRUSHING-H" "P-BUFFER-PURPLE" "P-BUFFER-PURPLE-H" "AS-PHOTO" "P-CROSSINGS-PIPE" "P-CROSSINGS-LINE" "P-CROSSINGS-GROUND" "P-CROSSINGS-GRID" "P-BUFFER-H 30m"))
 
  (defun LDOrder (layers
		  toFront
		  /
		  acdoc

		 )
    (defun ax:SortentsTable (space / dict res)
      (cond
	(
	 (not
	   (vl-catch-all-error-p
	     (setq res
		    (vl-catch-all-apply
		      'vla-item
		      (list (setq dict (vla-GetExtensionDictionary space))
			    "ACAD_SORTENTS"
		      )
		    )
	     )
	   )
	 )
	 res
	)
	((vla-AddObject dict "ACAD_SORTENTS" "AcDbSortentsTable"))
      )
    )

    (defun ax:List->VariantArray (dtype lst)
      (vlax-Make-Variant
	(vlax-SafeArray-Fill
	  (vlax-Make-SafeArray
	    dtype
	    (cons 0 (- (length lst) 1))
	  )
	  lst
	)
      )
    )


    (setq acdoc (vla-get-activedocument (vlax-get-acad-object)))
    (
     (lambda (sortents func / ss)
       (foreach	x (if toFront
		    (reverse layers)
		    layers
		  )
	 (if (setq
	       ss (ssget "_X"
			 (list (cons 8 x) (cons 410 (getvar 'CTAB)))
		  )
	     )
	   (
	    (lambda (/ l i)
	      (repeat (setq i (sslength ss))
		(setq l	(cons (vlax-ename->vla-object
				(ssname ss (setq i (1- i)))
			      )
			      l
			)
		)
	      )
	      (func sortents (ax:List->VariantArray vlax-vbobject l))
	    )
	   )
	 )
       )
     )
      (ax:SortentsTable
	(vlax-get-property
	  acdoc
	  (if (= 1 (getvar 'CVPORT))
	    'Paperspace
	    'Modelspace
	  )
	)
      )
      (if toFront
	vla-movetotop
	vla-movetobottom
      )
    )
    (vla-regen acdoc acactiveviewport)
  )
  (LDOrder layersabove T)
  (LDOrder layersunder nil)
  (princ)
)

(vl-load-com)
(princ)