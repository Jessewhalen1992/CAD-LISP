(defun c:SIFT (/ tbl numTPA numFMA targetRow numRows row col cellWidths offset)
  ;; Define the widths for each column
  (setq cellWidths '(115 130 250))  ; Adjust this list based on your table's columns

  ;; Define the offset based on your table's structure
  ;; If you have 1 header row, set offset to 1
  ;; If you have 2 header rows, set offset to 2, etc.
  (setq offset 1)  ; Adjust this value as needed

  ;; Prompt the user to select a table
  (princ "\nSelect the table to modify: ")
  (setq tbl (vlax-ename->vla-object (car (entsel))))

  ;; Validate the selected object
  (if (and tbl
           (or
             (equal (vla-get-ObjectName tbl) "AcDbTable")
             (equal (vla-get-ObjectName tbl) "AcDbDatalinkTable")))
    (progn
      ;; Prompt the user for the number of TPA entries
      (setq numTPA (getint "\nEnter the number of TPA entries: "))
      (if (not (numberp numTPA))
        (progn
          (princ "\nInvalid input for the number of TPA entries.")
          (exit)
        )
      )

      ;; Prompt the user for the number of FMA entries
      (setq numFMA (getint "\nEnter the number of FMA entries: "))
      (if (not (numberp numFMA))
        (progn
          (princ "\nInvalid input for the number of FMA entries.")
          (exit)
        )
      )

      ;; Calculate the target row (zero-based indexing)
      (setq targetRow (+ numTPA numFMA offset))

      ;; Debugging statement to verify targetRow
      (princ (strcat "\nCalculated targetRow index: " (itoa targetRow)))

      ;; Get the total number of rows in the table
      (setq numRows (vla-get-Rows tbl))

      ;; Debugging statement to verify numRows
      (princ (strcat "\nTotal number of rows in the table: " (itoa numRows)))

      ;; Validate the target row
      (if (> targetRow (- numRows 1))
        (progn
          (princ (strcat "\nError: The calculated target row (" (itoa targetRow) ") exceeds the total number of rows (" (itoa numRows) ")."))
          (princ "\nNo row will be set to height 40.")
        )
        (progn
          ;; Set column widths
          (princ "\nSetting column widths...")
          (setq col 0)
          (while (< col (vla-get-Columns tbl))
            (if (< col (length cellWidths))
              (vla-SetColumnWidth tbl col (nth col cellWidths))
            )
            (setq col (1+ col))
          )

          ;; Set all row heights to default (20)
          (princ "\nSetting all row heights to 20...")
          (setq row 0)
          (while (< row numRows)
            (vla-SetRowHeight tbl row 20.0)
            (setq row (1+ row))
          )

          ;; Set the target row height to 40
          (princ (strcat "\nSetting row " (itoa targetRow) " height to 40..."))
          (vla-SetRowHeight tbl targetRow 40.0)

          ;; Completion Message
          (princ "\nRow height adjustments completed successfully.")
        )
      )
    )
    (princ "\nError: The selected object is not a recognized table type (AcDbTable or AcDbDatalinkTable).")
  )

  (princ "\nDone.")
)
