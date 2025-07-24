(defun c:PLC (/ p1 p2 dist ang localVerts worldVerts bulgeUp bulgeDn entName)

  ;; Prompt for start/end points
  (setq p1 (getpoint "\nPick start point of break line: "))
  (if (not p1)
    (progn (prompt "\nNo point selected. Canceling...") (exit))
  )
  (setq p2 (getpoint p1 "\nPick end point (direction) of break line: "))
  (if (not p2)
    (progn (prompt "\nNo second point selected. Canceling...") (exit))
  )

  (setq dist (distance p1 p2)
        ang  (angle p1 p2))

  (if (< dist 1e-8)
    (progn
      (prompt "\nPoints too close; cannot create a wave.")
      (exit)
    )
  )

  ;; Two arcs, each ~90°, so bulge ~ 0.414
  (setq bulgeUp  0.414
        bulgeDn -0.414)

  ;; Three vertices = two segments:
  ;;   Vertex0 (bulge for segment0->1), Vertex1 (bulge for segment1->2), Vertex2 (no bulge).
  (setq localVerts
    (list
     (list 0.0 0.0 bulgeUp)
     (list 0.5 0.0 bulgeDn)
     (list 1.0 0.0 0.0)
    )
  )

  ;; Transform from local [0..1 in X] to world coords
  (defun local->world (lx ly / cosA sinA xw yw)
    (setq cosA (cos ang)
          sinA (sin ang)
          xw   (+ (car p1) (- (* lx dist cosA) (* ly dist sinA)))
          yw   (+ (cadr p1) (+ (* lx dist sinA) (* ly dist cosA)))
    )
    (list xw yw 0.0)
  )

  (setq worldVerts '())
  (foreach v localVerts
    (setq worldVerts
      (append worldVerts (list
        (append
          (local->world (car v) (cadr v))
          (list (caddr v))  ;; bulge is last
        )
      )))
  )

  ;; Create a 2D polyline (70 . 128) => linetype generation bit
  (setq entName
    (entmakex
      (list
       (cons 0 "POLYLINE")
       (cons 100 "AcDbEntity")
       (cons 8 "0")    ;; layer
       (cons 100 "AcDb2dPolyline")
       (cons 70 128)   ;; 128 => linetype generation on
      )
    )
  )

  ;; Create VERTEX records, each storing the bulge in group code 42
  (foreach wv worldVerts
    (entmakex
      (list
       (cons 0 "VERTEX")
       (cons 100 "AcDbEntity")
       (cons 8 "0")
       (cons 100 "AcDb2dVertex")
       (cons 10 (list (car wv) (cadr wv) (caddr wv)))
       (cons 42 (last wv))  ;; bulge
      )
    )
  )

  ;; SEQEND
  (entmakex
    (list
     (cons 0 "SEQEND")
     (cons 8 "0")
    )
  )

  ;; (setvar "PLINEGEN" 1) ;; optional for dashed linetypes

  (prompt "\nArc-based break line created (~90° arcs). Adjust bulges for shape.")
  (princ)
)
 