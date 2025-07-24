(defun c:UTM2LL ( / zone zoneCS pt result lat lon)
  ;; Prompt for zone ─ accept only 11 or 12
  (setq zone (getint "\nEnter UTM zone (11 or 12): "))
  (if (member zone '(11 12))
    (progn
      (setq zoneCS (strcat "UTM83-" (itoa zone)))     ; build CS name
      ;; Set coordinate systems
      (if (and (ade_projsetsrc zoneCS)                ; source = chosen UTM zone
               (ade_projsetdest "LL83"))              ; dest   = NAD83 Lat/Long
        (progn
          ;; Pick point
          (setq pt (getpoint (strcat "\nSelect point in " zoneCS ": ")))
          (if pt
            (progn
              ;; Forward projection (UTM → LL)
              (setq result (ade_projptforward (list (car pt) (cadr pt))))
              (if result
                (progn
                  (setq lon (car  result)
                        lat (cadr result))
                  (princ
                    (strcat "\nLatitude: "
                            (rtos lat 2 6)
                            ",  Longitude: "
                            (rtos lon 2 6)))
                )
                (prompt "\n** Coordinate conversion failed. **")
              )
            )
          )
        )
        (prompt "\n** Failed to set coordinate systems. **")
      )
    )
    (prompt "\n** Invalid zone – enter 11 or 12. **")
  )
  (princ)                                            ; exit quietly
)
