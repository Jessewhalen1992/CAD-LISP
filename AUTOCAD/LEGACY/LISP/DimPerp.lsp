
(defun C:DIMPERP (/ pline pt closestPt dist txtPos distStr)
  ;; UTM check logic
  (if (l+getUTMstatus nil)
    (C:UTM)
  )
  (setq pline (car (entsel "\nSelect a polyline: ")))

  (if pline
    (progn
      (while (setq pt (getpoint "\nSelect a location (press Enter to finish): "))
        (setq closestPt (vlax-curve-getClosestPointTo pline pt))
        (setq dist (distance pt closestPt))

        (princ (strcat "\nSelected point: " (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2)))
        (princ (strcat "\nClosest point on polyline: " (rtos (car closestPt) 2 2) ", " (rtos (cadr closestPt) 2 2)))
        (princ (strcat "\nDistance: " (rtos dist 2 2)))

        (command "_.LINE" pt closestPt "")

        ;; Calculate the midpoint of the line for text placement
        (setq txtPos (polar pt (angle pt closestPt) (/ dist 2.0)))
        (setq distStr (rtos dist 2 2))

        ;; Create the MTEXT centered on the line
        (command "_.MTEXT" txtPos "W" 2.5 (strcat "{\\C1;" distStr "}") "")
      )
    )
    (prompt "\nInvalid polyline selection.")
  )
  (princ)
)

(princ "\nType test to run the program.")
