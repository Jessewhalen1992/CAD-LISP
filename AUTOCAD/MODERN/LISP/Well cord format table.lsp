(defun c:WFT (/ tbl row col numRows numCols cellWidths cellHeight)
  (setq cellWidths '(100.0 100.0 60.0 60.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0 80.0))  ; List of widths for each column (adjust as needed)
  (setq cellHeight 25)  ; Default height for all rows (adjust as needed)

  ; Get the selected table object
  (setq tbl (vlax-ename->vla-object (car (entsel "Select table: "))))
  (if (and tbl (eq (vla-get-ObjectName tbl) "AcDbTable"))
    (progn
      (setq numRows (vla-get-Rows tbl))
      (setq numCols (vla-get-Columns tbl))

      ; Loop through each column to adjust widths
      (setq col 0)
      (while (< col numCols)
        (if (< col (length cellWidths))
          (vla-SetColumnWidth tbl col (nth col cellWidths))
        )
        (setq col (1+ col))
      )

      ; Loop through each row to adjust heights
      (setq row 0)
      (while (< row numRows)
        (vla-SetRowHeight tbl row cellHeight)
        (setq row (1+ row))
      )
    )
  )
  (princ)
)
