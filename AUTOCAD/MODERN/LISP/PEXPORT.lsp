(defun C:PEXPORT (/ *error* res utm)
  
  (defun *error* (msg)
    (princ)
  )
  
  (if C:UTM
    (PROGN
      (l+updUTM)
      (setq utm (l+getutmstatus nil))
      (initget "UTM Ground")
      (setq res (getkword "Select coordinate system [UTM/Ground]<Current>: "))
      (if (null res)(setq res (if utm "UTM" "Ground")))

      (if (/= utm (= res "UTM"))
	(c:utm)
      )
    )
  )

  (jtb+PEXPORT)
  (princ)
)