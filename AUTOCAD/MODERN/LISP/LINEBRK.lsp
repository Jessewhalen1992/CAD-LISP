(defun c:LB (/ p1 p2 ang rot doc ms blockName blockPath)
  ;; Prompt for first point (insertion point)
  (setq p1 (getpoint "\nPick first point: "))
  (if (not p1)
    (progn (prompt "\nNo point specified. Command canceled.") (exit))
  )

  ;; Prompt for second point (direction)
  (setq p2 (getpoint p1 "\nPick second point (direction): "))
  (if (not p2)
    (progn (prompt "\nNo second point specified. Command canceled.") (exit))
  )

  ;; Compute the angle from p1 to p2 (AutoCAD standard: 0 = East, CCW)
  (setq ang (angle p1 p2))

  ;; If the block's default orientation is North-South,
  ;; we subtract 90° (π/2) so 0 rotation = pointing up (North).
  (setq rot (- ang (/ pi 2)))

  ;; Prepare references
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        ms  (vla-get-ModelSpace doc)
        blockName "Linebrk")

  ;; Check if block is already in the drawing
  (if (tblsearch "BLOCK" blockName)
    ;; If found, insert using that block definition
    (vla-insertBlock
      ms
      (vlax-3D-point p1)
      blockName
      5000.0 5000.0 1.0
      rot
    )
    ;; Otherwise, try to find an external file named "Linebrk.dwg"
    (progn
      (setq blockPath (findfile (strcat blockName ".dwg")))
      (if blockPath
        (vla-insertBlock
          ms
          (vlax-3D-point p1)
          blockPath
          5000.0 5000.0 1.0
          rot
        )
        (prompt "\nCould not find the block 'Linebrk' or a file named 'Linebrk.dwg' in the support path.")
      )
    )
  )

  (princ)
)
