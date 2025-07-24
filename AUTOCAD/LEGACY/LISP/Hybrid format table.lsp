(defun c:HFT (/ tbl row col numRows numCols cellWidths cellHeight textSize)
  (setq cellWidths '(100.0 150.0 150.0 100.0 400.0))  ; List of widths for each column
  (setq cellHeight 25)  ; Default height for all rows
  (setq textSize 10.0)  ; Desired text height

  ; Get the selected table object
  (princ "\nSelect table: ")
  (setq tbl (vlax-ename->vla-object (car (entsel))))
  (if (eq (vla-get-ObjectName tbl) "AcDbTable")
    (progn
      (setq numRows (vla-get-Rows tbl))
      (setq numCols (vla-get-Columns tbl))

      ; Loop through each cell to adjust sizes
      (princ "\nAdjusting column widths and row heights...")
      (setq col 0)
      (while (< col numCols)
        (if (< col (length cellWidths))
          (vla-SetColumnWidth tbl col (nth col cellWidths))
        )
        (setq col (1+ col))
      )

      (setq row 0)
      (while (< row numRows)
        (vla-SetRowHeight tbl row cellHeight)
        (setq row (1+ row))
      )

      ; Loop through each cell to adjust text height
      (princ "\nAdjusting text heights...")
      (setq row 0)
      (while (< row numRows)
        (setq col 0)
        (while (< col numCols)
          (vla-SetTextHeight (vla-GetCellObject tbl row col) textSize)
          (setq col (1+ col))
        )
        (setq row (1+ row))
      )
    )
    (princ "\nSelected object is not a table.")
  )
  (princ "\nDone.")
)
