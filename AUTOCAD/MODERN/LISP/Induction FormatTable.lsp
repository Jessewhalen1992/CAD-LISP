(defun c:IFT (/ tableObj selSet rowCount colCount rowIndex colIndex cellValue roundedValue newValue quotient remainder)
  ; Prompt the user to select a table
  (prompt "\\nSelect a table: ")
  (setq selSet (ssget '((0 . "ACAD_TABLE"))))
  (if (not selSet)
    (progn
      (prompt "\\nNo table selected.")
      (exit)
    )
  )

  ; Get the table object
  (setq tableObj (vlax-ename->vla-object (ssname selSet 0)))
  (prompt "\\nTable selected.")

  ; Remove the first row
  (vla-DeleteRows tableObj 0 1)

  ; Get the number of rows and columns
  (setq rowCount (vla-get-rows tableObj))
  (setq colCount (vla-get-columns tableObj))
  (prompt (strcat "\\nRows: " (itoa rowCount)))
  (prompt (strcat "\\nColumns: " (itoa colCount)))

  ; Unmerge cells in the first row first
  (prompt "\\nUnmerging cells in the first row...")
  (setq colIndex 0)
  (while (< colIndex colCount)
    (vl-catch-all-apply
      '(lambda ()
         (vla-UnmergeCells tableObj 0 colIndex 0 colIndex)
      )
    )
    (setq colIndex (1+ colIndex))
  )

  ; Set the column widths
  (prompt "\\nSetting column widths...")
  (vl-catch-all-apply
    '(lambda ()
       (vla-SetColumnWidth tableObj 0 50.0)  ; Column A
       (vla-SetColumnWidth tableObj 1 60.0)  ; Column B
       (vla-SetColumnWidth tableObj 2 60.0)  ; Column C
       (vla-SetColumnWidth tableObj 3 90.0)  ; Column D
    )
  )

  ; Set the row heights
  (prompt "\\nSetting row heights...")
  (vl-catch-all-apply
    '(lambda ()
       (setq rowIndex 0)
       (while (< rowIndex rowCount)
         (vla-SetRowHeight tableObj rowIndex 30.0) ; Adjusted subsequent row heights to 30.0
         (setq rowIndex (1+ rowIndex))
       )
    )
  )

  ; Convert values in column D to chainage format
  (prompt "\\nConverting values in column D to chainage format...")
  (setq rowIndex 0) ; Start from the first row after deletion
  (while (< rowIndex rowCount)
    (setq cellValue (vla-GetText tableObj rowIndex 3))
    (if (and cellValue (/= cellValue ""))
      (progn
        (setq roundedValue (fix (+ 0.5 (atof cellValue)))) ; Round to nearest integer
        (if (< roundedValue 1000)
          (setq newValue (strcat "0+" (itoa roundedValue)))
          (progn
            (setq quotient (/ roundedValue 1000)) ; Get quotient
            (setq remainder (- roundedValue (* quotient 1000))) ; Get remainder
            (setq newValue (strcat (itoa quotient) "+" (itoa remainder)))
          )
        )
        (vla-SetText tableObj rowIndex 3 newValue)
      )
    )
    (setq rowIndex (1+ rowIndex))
  )

  ; Force a refresh
  (vla-Update tableObj)

  ; Confirm the changes
  (princ "\\nColumn widths, row heights, and text properties set successfully.")
  (princ)
)
