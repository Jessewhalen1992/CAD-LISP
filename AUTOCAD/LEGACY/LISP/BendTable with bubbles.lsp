(defun C:BT (/ plObj plPoints i ang nextAng prevAng lastPt thisPt nextPt dir angleStr station dist distStr
                  tableRow rowHeight insertionPoint tableObj angleThreshold bendCount blockName blockRefObj
                  blockInsertionPoint attributeObj attributesList rowCount colCount rowIndex colIndex cellValue
                  roundedValue newValue quotient remainder deg min minStr angDegrees direction diff attValue
                  headingBlockName headingBlockRefObj headingAttributesList sel remainderStr frac)

  ;; -------------------------------------------------------------------------
  ;; Utility functions
  ;; -------------------------------------------------------------------------

  ;; Convert degrees to radians
  (defun dtr (a) (* pi (/ a 180.0)))

  ;; Convert radians to degrees
  (defun rtd (a) (/ (* a 180.0) pi))

  ;; Cross product to determine turn direction
  (defun crossProduct (p1 p2 p3)
    (- (* (- (nth 0 p2) (nth 0 p1)) (- (nth 1 p3) (nth 1 p1)))
       (* (- (nth 1 p2) (nth 1 p1)) (- (nth 0 p3) (nth 0 p1)))))

  ;; ----> UPDATED angToStr function with rounding <----
  (defun angToStr (ang / deg frac min)
    ;; Separate the integer degrees from the fractional part
    (setq deg  (fix ang))
    (setq frac (- ang deg))

    ;; Convert fractional part to minutes
    (setq min (* 60.0 frac))

    ;; Round minutes
    (setq min (fix (+ 0.5 min)))

    ;; If rounding minutes hits 60, bump degrees
    (if (>= min 60)
      (progn
        (setq deg (1+ deg))
        (setq min (- min 60))
      )
    )

    ;; Zero-pad single-digit minutes
    (setq minStr (if (< min 10)
                     (strcat "0" (itoa min))
                     (itoa min)))

    ;; Return something like 19%%D05'
    (strcat (itoa deg) "%%D" minStr "'")
  )

  ;; Determine whether turn is Left or Right
  (defun getDir (p1 p2 p3)
    (if (> (crossProduct p1 p2 p3) 0) "Left" "Right"))

  ;; Get the difference between two angles (in radians, smallest side)
  (defun angDiff (a1 a2)
    (setq diff (abs (- a1 a2)))
    (if (> diff pi)
      (setq diff (- (* 2 pi) diff)))
    diff)

  ;; -------------------------------------------------------------------------
  ;; Main routine
  ;; -------------------------------------------------------------------------

  ;; ----- REMOVED the UTM check or fboundp calls -----
  ;; If needed, re-add your custom function calls here.

  ;; Prompt user for angle threshold
  (setq angleThreshold (getreal "\nEnter the angle threshold (in degrees): "))

  ;; Ensure a polyline is selected
  (setq plObj nil)
  (while (null plObj)
    (prompt "\nSelect a polyline: ")
    (setq sel (entsel))
    (if sel
      (setq plObj (vlax-ename->vla-object (car sel))))
    (if (or (null plObj)
            (not (member (vla-get-ObjectName plObj) '("AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline"))))
      (progn
        (prompt "\nInvalid selection. Please select a polyline.")
        (setq plObj nil)
      )
    )
  )

  (setq plPoints (vlax-get plObj 'Coordinates))

  ;; Build initial table data (header + rows)
  (setq tableRow (list (list "BEND #" "ANGLE" "DIRECTION" "CHAINAGE (m)")))
  (setq rowHeight 25)

  (setq lastPt (list (nth 0 plPoints) (nth 1 plPoints)))
  (setq station 0.0)
  (setq i 2)
  (setq bendCount 1)  ; Initialize bend counter

  (setq blockName "Induction_Bend_No")  ; Block name for bend markers

  ;; Loop over points and detect bends
  (while (< i (length plPoints))
    (setq thisPt (list (nth i plPoints) (nth (1+ i) plPoints)))
    (setq nextPt (if (< (+ 2 i) (length plPoints))
                   (list (nth (+ 2 i) plPoints) (nth (+ 3 i) plPoints))
                   nil))
    (setq dist (distance lastPt thisPt))
    (setq station (+ station dist))
    (setq distStr (rtos station 2 2))

    (setq prevAng (angle lastPt thisPt))
    (setq nextAng (if nextPt (angle thisPt nextPt) prevAng))
    (setq ang (angDiff prevAng nextAng))

    ;; Convert angle to degrees
    (setq angDegrees (rtd ang))

    ;; If bend exceeds threshold, record it
    (if (and nextPt (> angDegrees angleThreshold))
      (progn
        (setq angleStr  (angToStr angDegrees))
        (setq direction (getDir lastPt thisPt nextPt))

        ;; Add row to table data
        (setq tableRow (append tableRow (list (list (itoa bendCount) angleStr direction distStr))))

        ;; Insert bend block at bend point
        (setq blockInsertionPoint (vlax-3D-point (car thisPt) (cadr thisPt) 0.0))
        (setq blockRefObj
              (vla-InsertBlock
                (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                blockInsertionPoint
                blockName
                1.0 1.0 1.0 0.0))

        ;; If block reference inserted, set its BEND attribute
        (if (and blockRefObj (eq (vla-get-ObjectName blockRefObj) "AcDbBlockReference"))
          (progn
            (setq attributesList (vlax-safearray->list (vlax-variant-value (vla-GetAttributes blockRefObj))))
            (if attributesList
              (foreach attributeObj attributesList
                (if (and (eq (vla-get-ObjectName attributeObj) "AcDbAttribute")
                         (equal (strcase (vla-get-TagString attributeObj)) "BEND"))
                  (vla-put-TextString attributeObj (itoa bendCount))
                )
              )
            )
          )
        )

        ;; Increment bend counter
        (setq bendCount (1+ bendCount))
      )
    )
    (setq lastPt thisPt)
    (setq i (+ i 2))
  )

  ;; Ask for table insertion point
  (setq insertionPoint (vlax-3D-point (getpoint "\nSpecify table insertion point: ")))

  ;; Create table object in model space
  (setq tableObj
        (vla-AddTable
          (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
          insertionPoint
          (length tableRow)
          4
          rowHeight
          50))

  ;; Place table on the desired layer
  (vla-put-Layer tableObj "CG-NOTES")

  ;; Set table style
  (vla-put-StyleName tableObj "Induction Bend")

  ;; Populate table cells
  (foreach row tableRow
    (foreach item row
      (vla-SetText tableObj (vl-position row tableRow) (vl-position item row) item)
    )
  )

  (princ "\nTable created successfully.")

  ;; Adjust column widths as needed
  (vla-SetColumnWidth tableObj 0 50)
  (vla-SetColumnWidth tableObj 1 60)
  (vla-SetColumnWidth tableObj 2 60)
  (vla-SetColumnWidth tableObj 3 90)

  ;; Delete the first row (original header row)
  (vla-DeleteRows tableObj 0 1)

  ;; Convert values in the last column to chainage format with 3 digits after the "+"
  (setq rowCount (vla-get-rows tableObj))
  (setq colCount (vla-get-columns tableObj))
  (setq rowIndex 0)
  (while (< rowIndex rowCount)
    (setq cellValue (vla-GetText tableObj rowIndex 3))
    (if (and cellValue (/= cellValue ""))
      (progn
        ;; Round to nearest integer
        (setq roundedValue (fix (+ 0.5 (atof cellValue))))

        ;; Break into thousands and remainder
        (setq quotient (/ roundedValue 1000))
        (setq remainder (- roundedValue (* quotient 1000)))

        ;; Zero-pad remainder to always be 3 digits
        (setq remainderStr (itoa remainder))
        (cond
          ((< remainder 10)   (setq remainderStr (strcat "00" remainderStr)))
          ((< remainder 100)  (setq remainderStr (strcat "0"  remainderStr)))
        )

        ;; Combine into final chainage string
        (setq newValue (strcat (itoa quotient) "+" remainderStr))
        (vla-SetText tableObj rowIndex 3 newValue)
      )
    )
    (setq rowIndex (1+ rowIndex))
  )

  ;; Insert "INDUCTION HEADING" block at the same insertion point (top-left)
  (setq headingBlockName "INDUCTION HEADING")
  (setq headingBlockRefObj
        (vla-InsertBlock
          (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
          insertionPoint
          headingBlockName
          1.0 1.0 1.0 0.0))

  ;; Place heading block on the desired layer
  (if headingBlockRefObj
    (vla-put-Layer headingBlockRefObj "CG-NOTES"))

  ;; Update the ANGLE_THRESHHOLD attribute in heading block
  (if (and headingBlockRefObj (eq (vla-get-ObjectName headingBlockRefObj) "AcDbBlockReference"))
    (progn
      (setq headingAttributesList
            (vlax-safearray->list (vlax-variant-value (vla-GetAttributes headingBlockRefObj))))

      ;; If angleThreshold = 0, set blank. Otherwise show e.g. (≥19°).
      (if (zerop angleThreshold)
        (setq attValue "")
        (setq attValue (strcat "(" "\\U+2265" (itoa (fix angleThreshold)) "%%D" ")")))

      (if headingAttributesList
        (foreach attributeObj headingAttributesList
          (if (and (eq (vla-get-ObjectName attributeObj) "AcDbAttribute")
                   (equal (strcase (vla-get-TagString attributeObj)) "ANGLE_THRESHHOLD"))
            (vla-put-TextString attributeObj attValue)
          )
        )
      )
    )
  )

  ;; Refresh table
  (vla-Update tableObj)

  (princ "\nFormatting applied successfully.")
  (princ)
)
