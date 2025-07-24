(defun
C:TO ()
(setvar "cmdecho" 0)
(setq OB (entsel "\nSelect object for base angle: "))
(setvar "osmode" 0)
(setq DB (entget (car OB)))
(setq SP (osnap (cadr OB) "nea"))
(setq ENT (cdr (assoc 0 DB)))
(cond
((equal "LINE" ENT)
(setq
ST (cdr (assoc 10 DB))
END (cdr (assoc 11 DB))
) ;_ end of setq
)
((or (equal "ARC" ENT) (equal "CIRCLE" ENT))
(setq DIST (distance (cdr (assoc 10 DB)) SP))
(setq AA (angle (cdr (assoc 10 DB)) SP))
(setq
ST (cdr (assoc 10 DB))
END (polar ST (+ AA (cvunit 90 "degree" "radian")) DIST)
) ;_ end of setq
)
((or (equal "POLYLINE" ENT) (equal "LWPOLYLINE" ENT))
(setq MID (osnap (cadr OB) "mid"))
(if (< (car MID) (car SP))
(setq
ST MID
END SP
) ;_ end of setq
) ;_ end of if
(setq
ST SP
END MID
) ;_ end of setq
)
) ;_ end of cond
(setq AG (angle ST END))
(if (and (<= AG 4.71239) (> AG 1.5708))
(setq AG (angle END ST))
) ;_ end of if
(setq AGL (car (list (cons 50 AG))))
(setq N 0)
(princ "\nNow, Select the text to rotate!")
(setq TXT (ssget))
(repeat (sslength TXT)
(setq
ELIST
(subst
AGL
(assoc 50 (entget (ssname TXT N)))
(entget (ssname TXT N))
) ;_ end of subst
) ;_ end of setq
(entmod ELIST)
(entupd (ssname TXT N))
(setq N (+ N 1))
) ;_ end of repeat
(setvar "cmdecho" 1)
(princ)
) ;_ end of defun
;(princ "\nCopyright (c) 2000, Joon Hong All Rights Reserved.")
;(princ
; "\nType 'rt' to rotate the text at the same angle of the selected object"
 ;_ end of princ
(princ)

