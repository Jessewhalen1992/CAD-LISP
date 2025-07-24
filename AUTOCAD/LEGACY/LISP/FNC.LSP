;;;===============================================================
;;; FNC.LSP - Boundary Delete/Recolor (No Leader Logic)
;;;===============================================================
;;; 1) User picks a closed LWPolyline boundary.
;;; 2) Selects all objects in that boundary ("_CP").
;;; 3) For each object:
;;;    - If layer in 'delLayers' => delete
;;;    - If layer in 'colorLayers' => force color=255
;;;      * If dimension => also dimension lines, ext lines, text => 255
;;; 4) No references to leaders, MLeaders, BURST, or MATCHPROP.
;;;===============================================================

(vl-load-com)

(defun c:FNC ( / delLayers colorLayers ent vlaObj polyPoints mainSS )

  ;;-------------------------------------------------------------
  ;; A) Configure layer lists
  ;;-------------------------------------------------------------
  (setq delLayers
         '("T-BUSH" "T-BUSH-T" "AS-GENERAL" "P-TEMP-H" "T-BUSH-T"
           "L-BD" "L-BD1" "T-WATER-T" "T-WATER" "T-DRAW" "WETLAND"
           "WETLAND-T" "AS-WILDLIFE" "T-WATER-P3" "L-MON" "P-PROPOSED-H"
           "AS-ROAD" "AS-PIPE-UG" "L-ROAD-T" "L-SECTION-LSD" "L-DETAIL-T"
           "T-TEXT" "AUX-BUFFER" "T-TIE-T" "T-TIE" "L-TIE" "AS-ROADS"
           "Z-DESCRIPTION" "Z-MSPOINT" "Z-ELEVATION" "Z-POINTNUMBER"
           "C-SYMBOL" "T-SLOPE" "T-VEGETATION CHANGE" "C-SYMBOL" "P-TEMP-BLOCKS" 
           "P-CROSSINGS" "AS-UNDERLAY-H" "AS-OWNER" "L-UNS-T"  "AS-TELUS" 
           "AS-POWER" "AS-CABLE" "C-SYMBOL" "AS-SYMBOL" "35" "T-CREEK" "T-CREEK-T" "P-TRAV" "CG-NOTES"
         )
        colorLayers
         '("C-RW" "C-RW-T" "F-RW" "F-RW-T"
           "C-WS" "C-WS-T" "F-WS" "F-WS-T"
           "F-AR" "F-AR-T" "C-AR" "C-AR-T"
           "L-USEC" "L-SEC" "L-SECQ" "L-QSEC" "9" "10" "D-TEXT" "F-EZE" "F-EZE-T" "D-TXT" "P-TXT" "P-STATION" "P-BORE" "DETAIL-T" "AS-PIPE-UG-T" "T-TEXT" "WETLAND" "WETLAND-T"
         )
  )

  ;;-------------------------------------------------------------
  ;; B) ENTSEL the boundary (must click a closed LWPolyline)
  ;;-------------------------------------------------------------
  (prompt "\nSelect existing closed polyline to define boundary: ")
  (setq ent (entsel))
  (if (not ent)
    (progn
      (prompt "\nNo boundary selected. Canceling.\n")
      (princ)
      (exit)
    )
  )
  (setq vlaObj (vlax-ename->vla-object (car ent)))

  ;;-------------------------------------------------------------
  ;; C) Build (x y) points for ssget "_CP"
  ;;-------------------------------------------------------------
  (setq polyPoints (GetPolylinePointList vlaObj))
  (if (< (length polyPoints) 3)
    (progn
      (prompt "\nBoundary has fewer than 3 corners. Canceling.\n")
      (princ)
      (exit)
    )
  )

  ;;-------------------------------------------------------------
  ;; D) SSGET all objects in boundary => delete or recolor
  ;;-------------------------------------------------------------
  (setq mainSS (ssget "_CP" polyPoints))
  (if (not mainSS)
    (prompt "\nNo objects found in boundary.\n")
    (ProcessSelection mainSS delLayers colorLayers)
  )

  (prompt "\nDone.\n")
  (princ)
)

;;-----------------------------------------------------------------------
;; GETPOLYLINEPOINTLIST
;;  Converts an AcDbPolyline's .Coordinates to ((x1 y1)(x2 y2) ...).
;;  If coords are (x,y,z), we skip z. If not multiple of 2 or 3 => exit.
;;-----------------------------------------------------------------------
(defun GetPolylinePointList (vlaObj / varCoords arr rawList chunkSize pts i)
  (setq varCoords (vla-get-Coordinates vlaObj))
  (setq arr       (vlax-variant-value varCoords))
  (setq rawList   (vlax-safearray->list arr))

  (cond
    ((= 0 (rem (length rawList) 3)) (setq chunkSize 3)) ;; x,y,z
    ((= 0 (rem (length rawList) 2)) (setq chunkSize 2)) ;; x,y
    (T
     (prompt (strcat
       "\nError: " (itoa (length rawList))
       " coords, not multiple of 2 or 3. Canceling.\n"
     ))
     (exit)
    )
  )

  (setq pts '() i 0)
  (while (< i (length rawList))
    (if (>= (+ i 1) (length rawList)) (exit))
    (setq pts (append pts (list (list (nth i rawList) (nth (1+ i) rawList)))))
    (setq i (+ i chunkSize))
  )
  pts
)

;;-----------------------------------------------------------------------
;; PROCESSSELECTION
;;  Loops over SS => if layer in delLayers => delete,
;;                   if layer in colorLayers => ForceColorWhite
;;-----------------------------------------------------------------------
(defun ProcessSelection (ss delList colList / i eName)
  (setq i 0)
  (while (< i (sslength ss))
    (setq eName (ssname ss i))
    (ProcessEntity eName delList colList)
    (setq i (1+ i))
  )
)

(defun ProcessEntity (eName delList colList / obj layer)
  (setq obj   (vlax-ename->vla-object eName)
        layer (vla-get-Layer obj))
  (cond
    ((member layer delList)
      (vla-delete obj)
    )
    ((member layer colList)
      (ForceColorWhite obj)
    )
  )
)

;;-----------------------------------------------------------------------
;; FORCECOLORWHITE
;;   - color=255 at top level
;;   - if dimension => dimension lines, extension lines, text => 255
;;-----------------------------------------------------------------------
(defun ForceColorWhite (vlaObj / objName)
  (setq objName (vla-get-ObjectName vlaObj))

  ;; Force top-level color
  (vla-put-Color vlaObj 255)

  ;; If it's a standard dimension, also override subcomponents:
  (cond
    ((member objName
             '("AcDbAlignedDimension"
               "AcDbRotatedDimension"
               "AcDb2LineAngularDimension"
               "AcDb3PointAngularDimension"
               "AcDbRadialDimension"
               "AcDbDiametricDimension"
               "AcDbOrdinateDimension"
              )
      )
      (vl-catch-all-apply 'vla-put-DimensionLineColor (list vlaObj 255))
      (vl-catch-all-apply 'vla-put-ExtensionLineColor (list vlaObj 255))
      (vl-catch-all-apply 'vla-put-TextColor          (list vlaObj 255))
    )
    ;; else do nothing special
  )
)
;;;===============================================================
;;; End of File
;;;===============================================================