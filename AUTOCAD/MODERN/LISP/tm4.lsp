; Turns Background mask on
; Set 'Border Offset Factor' to 1.15
(defun c:tm4 (/ ss1 num cnt obj ent)
  (setq ss1 (ssget '((0 . "mtext")))
    num (sslength ss1)
    cnt 0)
  (repeat num
      (setq obj (vlax-ename->vla-object (ssname ss1 cnt)))
          (vlax-put-property obj 'BackgroundFill :vlax-true)
          (setq ent (vlax-vla-object->ename obj)
                elist (entget ent)
                elist (subst (cons 45 1.15)(assoc 45 elist) elist)
                elist (subst (cons 421 256)(assoc 421 elist) elist)
	  )
          (entmod elist)
      (setq cnt (1+ cnt))
  ); repeat
  (vl-cmdf "_draworder" ss1 "" "f")
  (princ)
)