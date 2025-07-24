(defun c:BPL (/ pl ent i pt1 pt2 lineAngle textAngle bearingAngle angleDegrees totalSeconds dist bearing
                 textPos bearingPos distancePos degrees minutes seconds pi numSegments
                 originalClosedFlag layerName distanceTextResult bearingTextResult
                 textEntitiesToDelete positionOption dx dy offsetAngle flipped)
  (setq pi (* 4 (atan 1.0))) ; Define pi
 
 ;; UTM check logic
  (if (l+getUTMstatus nil)
    (C:UTM)
  )

  ;; Prompt the user to choose between "L-BD" and "L-BD1"
  (defun getLayerName ()
    (initget "L-BD L-BD1")
    (setq layerName (getkword "\nEnter layer name for text [L-BD/L-BD1]: "))
    (if (member layerName '("L-BD" "L-BD1"))
        layerName
        (progn
          (prompt "\nInvalid layer name. Please enter 'L-BD' or 'L-BD1'.")
          (getLayerName)
        )
    )
  )
  (setq layerName (getLayerName))

  ;; Prompt the user to choose whether the text should be above or below (per line direction)
  (defun getPositionOption ()
    (initget "Above Below")
    (setq positionOption (getkword "\nEnter position for text [Above/Below]: "))
    (if (member positionOption '("Above" "Below"))
        positionOption
        (progn
          (prompt "\nInvalid option. Please enter 'Above' or 'Below'.")
          (getPositionOption)
        )
    )
  )
  (setq positionOption (getPositionOption))

  ;; Check if the layer exists
  (if (null (tblsearch "layer" layerName))
    (progn
      (prompt (strcat "\nLayer \"" layerName "\" does not exist. Please create it before running the command."))
      (exit)
    )
    (progn
      (setq pl (car (entsel "\nSelect a polyline: ")))
      (if (and pl (wcmatch (cdr (assoc 0 (entget pl))) "*POLYLINE"))
        (progn
          ;; Get the entity list of the polyline
          (setq ent (entget pl))
          ;; Get the original closed status of the polyline
          (setq originalClosedFlag (cdr (assoc 70 ent)))
          ;; Temporarily close the polyline if it's open
          (if (/= (logand originalClosedFlag 1) 1)
            (progn
              (setq ent (subst (cons 70 (boole 7 originalClosedFlag 1)) (assoc 70 ent) ent))
              (entmod ent)
              (entupd pl)
            )
          )

          (setq numSegments (fix (vlax-curve-getEndParam pl)))
          (setq i 0)
          (setq textEntitiesToDelete '())
          (prompt "\nGenerating text for each segment...\n")

          (while (< i numSegments)
            (setq pt1 (vlax-curve-getPointAtParam pl i))
            (setq pt2 (vlax-curve-getPointAtParam pl (if (< (1+ i) numSegments) (1+ i) 0)))

            (setq dist (distance pt1 pt2))
            (setq dx (- (car pt2) (car pt1)))
            (setq dy (- (cadr pt2) (cadr pt1)))

            ;; lineAngle: direction of the segment
            (setq lineAngle (atan dy dx))

            ;; Calculate bearing angle from North, measured clockwise
            (setq bearingAngle (- (/ pi 2) lineAngle))
            (if (< bearingAngle 0)
                (setq bearingAngle (+ bearingAngle (* 2 pi))))

            ;; Convert bearingAngle to degrees/minutes/seconds
            (setq angleDegrees (* bearingAngle (/ 180.0 pi)))
            (setq totalSeconds (+ 0.5 (* angleDegrees 3600.0)))
            (setq degrees (fix (/ totalSeconds 3600.0)))
            (setq minutes (fix (/ (- totalSeconds (* degrees 3600.0)) 60.0)))
            (setq seconds (- totalSeconds (* degrees 3600.0) (* minutes 60.0)))
            (setq seconds (* 5 (fix (+ 0.5 (/ seconds 5.0)))))

            (if (>= seconds 60)
              (progn
                (setq seconds 0)
                (setq minutes (1+ minutes))))
            (if (>= minutes 60)
              (progn
                (setq minutes 0)
                (setq degrees (1+ degrees))))

            (setq bearing (strcat (itoa degrees) "\U+00B0"
                                  (if (< minutes 10) (strcat "0" (itoa minutes)) (itoa minutes)) "'"
                                  (if (< seconds 10) (strcat "0" (itoa seconds)) (itoa seconds)) "\""))

            ;; Midpoint
            (setq textPos (list (/ (+ (car pt1) (car pt2)) 2.0)
                                (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                                0.0))

            ;; Normalize text angle so text isn't upside-down
            (setq textAngle lineAngle)
            (setq flipped nil)
            (cond
              ((> textAngle (/ pi 2)) 
               (setq textAngle (- textAngle pi))
               (setq flipped t)
              )
              ((< textAngle (- (/ pi 2)))
               (setq textAngle (+ textAngle pi))
               (setq flipped t)
              )
            )

            ;; Determine offset angle based on Above/Below:
            ;; Above = lineAngle + 90° (pi/2)
            ;; Below = lineAngle - 90° (pi/2)
            (setq offsetAngle (if (equal positionOption "Above")
                                  (+ lineAngle (/ pi 2))
                                  (- lineAngle (/ pi 2))
                                ))

            ;; If we flipped the text angle by pi, also flip the offset by pi to maintain correct side
            (if flipped
              (setq offsetAngle (+ offsetAngle pi))
            )

            ;; Fixed distances
            (setq distancePos (polar textPos offsetAngle 9.5))
            (setq bearingPos  (polar textPos offsetAngle 23.75))

            (prompt (strcat "\nCreating distance text at: "
                            (rtos (car distancePos) 2 2) ", "
                            (rtos (cadr distancePos) 2 2)))

            (setq distanceTextResult
                  (entmakex
                    (list
                      (cons 0 "TEXT")
                      (cons 8 layerName)
                      (cons 10 distancePos)
                      (cons 40 10.0)
                      (cons 1 (rtos dist 2 2))
                      (cons 50 textAngle)
                      (cons 7 "Standard")
                      (cons 72 1)
                      (cons 73 2)
                      (cons 11 distancePos)
                    )
                  )
            )
            (if (and distanceTextResult (eq (type distanceTextResult) 'ENAME))
                (prompt "\nDistance text created successfully.")
                (prompt "\nFailed to create distance text."))

            (prompt (strcat "\nCreating bearing text at: "
                            (rtos (car bearingPos) 2 2) ", "
                            (rtos (cadr bearingPos) 2 2)))

            (setq bearingTextResult
                  (entmakex
                    (list
                      (cons 0 "TEXT")
                      (cons 8 layerName)
                      (cons 10 bearingPos)
                      (cons 40 10.0)
                      (cons 1 bearing)
                      (cons 50 textAngle)
                      (cons 7 "Standard")
                      (cons 72 1)
                      (cons 73 2)
                      (cons 11 bearingPos)
                    )
                  )
            )
            (if (and bearingTextResult (eq (type bearingTextResult) 'ENAME))
                (prompt "\nBearing text created successfully.")
                (prompt "\nFailed to create bearing text."))

            ;; If originally open, remove last texts
            (if (and (/= (logand originalClosedFlag 1) 1) (= i (1- numSegments)))
              (progn
                (if (eq (type distanceTextResult) 'ENAME)
                  (setq textEntitiesToDelete (cons distanceTextResult textEntitiesToDelete)))
                (if (eq (type bearingTextResult) 'ENAME)
                  (setq textEntitiesToDelete (cons bearingTextResult textEntitiesToDelete)))
              )
            )

            (setq i (1+ i))
          )

          ;; If the polyline was originally open, delete the last distance and bearing texts
          (foreach ent textEntitiesToDelete
            (entdel ent)
          )

          ;; Restore original closed status
          (if (/= (logand originalClosedFlag 1) 1)
              (progn
                (setq ent (subst (cons 70 originalClosedFlag) (assoc 70 ent) ent))
                (entmod ent)
                (entupd pl)
              )
          )
        )
        (prompt "\nNo polyline selected or incorrect entity type.")
      )
    )
  )
  (princ)
)
