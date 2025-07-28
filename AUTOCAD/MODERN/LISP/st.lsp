; THIS PROGRAM MATCHES TEXT AND CHANGES THE LAYER

(defun c:st ()
(setvar "cmdecho" 0)
(PROMPT "\nPICK LINE OF TEXT TO CHANGE: ")
(setq SAMETEXT1 (entget (car (entsel))))
(setq SAMETEXT2 (cdr (assoc 1 SAMETEXT1)))
(PROMPT "\nCHANGE TO: ")
(setq SAMETEXT3 (entget (car (entsel))))
(setq SAMETEXT4 (cdr (assoc 1 SAMETEXT3)))
(setq SAMETEXT1 (subst (cons 1 SAMETEXT4)
         	(assoc 1  SAMETEXT1) SAMETEXT1))
(entmod SAMETEXT1)

(setq ANS (getstring "\nMatch layer ? <'Enter' for YES or 'N' for NO> .... "))
(if (= ANS "")
(progn
(setq ln1 (cdr (assoc 8 sametext3)))
(command "layer" "s" ln1 "")
(setq clay (getvar "clayer"))
(setq ed1 (subst (cons 8 ln1) (assoc 8 sametext1) sametext1))
(entmod ed1)
(princ "** Layer Matched **")
)
(princ "** No Layer Matched **")
)

(menucmd "p0=POP0")

(PRINC)
)
