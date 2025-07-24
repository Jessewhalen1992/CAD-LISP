(defun c:CURVE (/ ent en edata rad arcLen startAng endAng rawAngle angRad angDeg d m s sRounded
                   deltaChar angleStr radiusStr arcStr textStr insPt)

  ;; -- Utility Functions --
  (defun rtd (x) (* 180.0 (/ x pi)))  ; radians -> degrees
  (defun round-to-nearest-5 (val)
    "Rounds VAL (in seconds) to the nearest 5."
    (* 5 (fix (/ (+ val 2.5) 5))))

  ;; Greek capital Delta, scaled 2×
  (setq deltaChar "{\\H1.0x;\\U+0394}")

  ;; -- Prompt user to select an arc --
  (setq ent (entsel "\nSelect an arc: "))
  (if ent
    (progn
      (setq en (car ent)
            edata (entget en))

      ;; Extract radius and angles (in radians)
      (setq rad      (cdr (assoc 40 edata))) ; radius
      (setq startAng (cdr (assoc 50 edata))) ; start angle
      (setq endAng   (cdr (assoc 51 edata))) ; end angle

      ;; -- Compute the smaller arc angle --
      (setq rawAngle (- endAng startAng))
      (if (< rawAngle 0)
        (setq rawAngle (+ rawAngle (* 2 pi))))
      (if (> rawAngle pi)
        (setq rawAngle (- (* 2 pi) rawAngle)))
      (setq angRad rawAngle)

      ;; Arc length
      (setq arcLen (* rad angRad))

      ;; Convert radians to degrees, then D/M/S
      (setq angDeg (rtd angRad))
      (setq d (fix angDeg))
      (setq m (fix (* 60.0 (- angDeg d))))
      (setq s (* 3600.0 (- angDeg d (/ m 60.0))))

      ;; Round seconds to nearest 5
      (setq sRounded (round-to-nearest-5 s))
      (if (>= sRounded 60)
        (progn
          (setq sRounded 0)
          (setq m (1+ m))))
      (if (>= m 60)
        (progn
          (setq m (- m 60))
          (setq d (1+ d))))

      ;; Build the strings (use \U+00B0 for degree symbol)
      (setq angleStr (strcat (itoa d) "\\U+00B0" (itoa m) "'" (itoa sRounded) "\""))
      (setq radiusStr (rtos rad 2 2))
      (setq arcStr    (rtos arcLen 2 2))

      (setq textStr (strcat
        deltaChar " = " angleStr "\\P"
        "RADIUS = " radiusStr "\\P"
        "ARC = " arcStr
      ))

      ;; Ask user for an insertion point
      (setq insPt (getpoint "\nSpecify location for arc info text: "))

      ;; Ensure layer LB-D exists and set it
      (command "._-layer" "make" "LB-D" "")

      ;; Create the MText on layer LB-D
      (entmake
        (list
          (cons 0    "MTEXT")
          (cons 100  "AcDbEntity")
          (cons 8    "LB-D")       ; layer
          (cons 100  "AcDbMText")
          (cons 10   insPt)        ; insertion point
          (cons 40   10.0)         ; text height
          (cons 1    textStr)      ; MText content
          (cons 7    "STANDARD")   ; text style
          (cons 71   1)            ; top-left
          (cons 72   1)            ; left-to-right
        )
      )
      (princ "\nArcDim finished.")
    )
  )
  (princ)
)
