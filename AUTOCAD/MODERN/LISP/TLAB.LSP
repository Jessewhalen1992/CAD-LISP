(defun c:TLAB ()
(setvar "cmdecho" 0)
(setq oldtxt (getvar"textstyle"))
(setq oldlay (getvar"clayer"))

(prompt "\nPick the quarter section label ... ")
(setq ent (car (entsel)))
(setq inspt (cdr (assoc 10 (entget ent))))
(setq x1 (car inspt))
(setq y1 (cadr inspt))
(setq ent2 (entnext ent))
(if (and (= (cdr (assoc 0 (entget ent2))) "ATTRIB") (= (cdr (assoc 2 (entget ent2))) "SEC"))
  (setq sec (cdr (assoc 1 (entget ent2))))
)
(setq ent3 (entnext ent2))
(if (and (= (cdr (assoc 0 (entget ent3))) "ATTRIB") (= (cdr (assoc 2 (entget ent3))) "TWP"))
  (setq twp (cdr (assoc 1 (entget ent3))))
)
(setq ent4 (entnext ent3))
(if (and (= (cdr (assoc 0 (entget ent4))) "ATTRIB") (= (cdr (assoc 2 (entget ent4))) "RGE"))
  (setq rge (cdr (assoc 1 (entget ent4))))
)
(setq ent5 (entnext ent4))
(if (and (= (cdr (assoc 0 (entget ent5))) "ATTRIB") (= (cdr (assoc 2 (entget ent5))) "MER"))
  (setq mer (cdr (assoc 1 (entget ent5))))
)
;(setq qtrpt (getpoint "\nPick a point in the quarter ... "))


         (setq NW1 (list (- (car inspt) 402) (+ (cadr inspt) 402)))
  (setq qtr "N.W.")
(setvar "clayer" "C-symbol")
(setq qstxt1 (strcat "Theor. " qtr "1/4"))
(setq qstxt (strcat "Sec. " sec "-" twp "-" rge "-" "W." mer "M."))
(setvar "cecolor" "green")
(setvar"textstyle" "175l")
(command "text" "j" "bc" nw1 "90" qstxt1)
(command "text" "j" "bc" nw1 "90" qstxt)
(setq move1 (entlast))
(command "move" move1 "" nw1 "@30.7619<180" "")



         (setq NE1 (list (+ (car inspt) 402) (+ (cadr inspt) 402)))
  (setq qtr "N.E.")
(setvar "clayer" "C-symbol")
(setq qstxt1 (strcat "Theor. " qtr "1/4"))
(setq qstxt (strcat "Sec. " sec "-" twp "-" rge "-" "W." mer "M."))
(setvar "cecolor" "green")
(setvar"textstyle" "175l")
(command "text" "j" "bc" ne1 "90" qstxt1)
(command "text" "j" "bc" ne1 "90" qstxt)
(setq move2 (entlast))
(command "move" move2 "" ne1 "@30.7619<180" "")





         (setq SW1 (list (- (car inspt) 402) (- (cadr inspt) 402)))
  (setq qtr "S.W.")
(setvar "clayer" "C-symbol")
(setq qstxt1 (strcat "Theor. " qtr "1/4"))
(setq qstxt (strcat "Sec. " sec "-" twp "-" rge "-" "W." mer "M."))
(setvar "cecolor" "green")
(setvar"textstyle" "175l")
(command "text" "j" "bc" sw1 "90" qstxt1)
(command "text" "j" "bc" sw1 "90" qstxt)
(setq move3 (entlast))
(command "move" move3 "" sw1 "@30.7619<180" "")





         (setq SE1 (list (+ (car inspt) 402) (- (cadr inspt) 402)))
  (setq qtr "S.E.")
(setvar "clayer" "C-symbol")
(setq qstxt1 (strcat "Theor. " qtr "1/4"))
(setq qstxt (strcat "Sec. " sec "-" twp "-" rge "-" "W." mer "M."))
(setvar "cecolor" "green")
(setvar"textstyle" "175l")
(command "text" "j" "bc" se1 "90" qstxt1)
(command "text" "j" "bc" se1 "90" qstxt)
(setq move4 (entlast))
(command "move" move4 "" se1 "@30.7619<180" "")




(setvar "cecolor" "bylayer")
(setvar "textstyle" oldtxt)
(setvar "clayer" oldlay)
(setvar "cmdecho" 1)
(princ)
)
