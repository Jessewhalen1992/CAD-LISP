(defun c:toltest (/ pline pt closestPt dist txtPos distStr maxTol minTol mtextObj activeDoc modelSpace)
  ;; Prompt for maximum and minimum tolerance values
  (setq maxTol (getreal "\\nEnter the maximum tolerance value: "))
  (setq minTol (getreal "\\nEnter the minimum tolerance value: "))

  (setq pline (car (entsel "\\nSelect a polyline: ")))

  (if pline
    (progn
      (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq modelSpace (vla-get-ModelSpace activeDoc))

      (while (setq pt (getpoint "\\nSelect a location (press Enter to finish): "))
        (setq closestPt (vlax-curve-getClosestPointTo pline pt))
        (setq dist (distance pt closestPt))

        (princ (strcat "\\nSelected point: " (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2)))
        (princ (strcat "\\nClosest point on polyline: " (rtos (car closestPt) 2 2) ", " (rtos (cadr closestPt) 2 2)))
        (princ (strcat "\\nDistance: " (rtos dist 2 2)))

        (command "_.LINE" pt closestPt "")

        ;; Calculate the midpoint of the line for text placement
        (setq txtPos (polar pt (angle pt closestPt) (/ dist 2.0)))
        (setq distStr (rtos dist 2 2))

        ;; Determine text color based on tolerance
        (cond
          ((> dist maxTol)
           (setq distStr (strcat "{\\C1;" distStr "}"))  ;; Red color for exceeding max tolerance
          )
          ((< dist minTol)
           (setq distStr (strcat "{\\C1;" distStr "}"))  ;; Green color for within minimum tolerance
          )
          (t
           (setq distStr (strcat "{\\C7;" distStr "}"))  ;; Default color for within tolerance
          )
        )

        ;; Create the MTEXT centered on the line with current text style and height 0.001
        (setq mtextObj (vla-AddMText modelSpace (vlax-3d-point txtPos) 0.001 distStr))
        (vla-put-Height mtextObj 0.001)
      )
    )
    (prompt "\\nInvalid polyline selection.")
  )
  (princ)
)

(princ "\\nType test to run the program.")
