;;; -------------------------------------------------------------------
;;; QSLAB3 – quarter-section label + ROW & Workspace areas
;;; 2025-05-08 – fixed vertical spacing: 22.5 and 20 units
;;; -------------------------------------------------------------------

(defun c:QSLAB3  (/ oldtxt oldlay
                     ent ent2 ent3 ent4 ent5
                     sec twp rge mer
                     inspt x1 y1 qtrpt x2 y2 qtr
                     qsha1 qsha qsac1 qsac qtrpt2 arealab
                     twha1 twha twac1 twac qtrpt3 twsplab)

  (vl-load-com)

  ;; ----------------------------------------------------------
  ;; save current text style / layer so we can restore later
  ;; ----------------------------------------------------------
  (setq oldtxt (getvar "TEXTSTYLE")
        oldlay (getvar "CLAYER"))

  ;; ----------------------------------------------------------
  ;; helper – fetch attribute value by tag (case-insensitive)
  ;; ----------------------------------------------------------
  (defun _getattr (ename tag / d)
    (if (and ename (= (cdr (assoc 0 (setq d (entget ename)))) "ATTRIB"))
      (if (equal (strcase (cdr (assoc 2 d))) (strcase tag))
        (cdr (assoc 1 d))
      )
    )
  )

  ;; ----------------------------------------------------------
  ;; pick the quarter-section label block & read its attributes
  ;; ----------------------------------------------------------
  (prompt "\nPick the quarter section label ... ")
  (setq ent (car (entsel)))
  (if (null ent) (progn (princ "\nNothing selected.") (exit)))

  (setq inspt (cdr (assoc 10 (entget ent)))
        x1    (car  inspt)
        y1    (cadr inspt))

  (setq ent2 (entnext ent)      sec (_getattr ent2 "SEC")
        ent3 (entnext ent2)     twp (_getattr ent3 "TWP")
        ent4 (entnext ent3)     rge (_getattr ent4 "RGE")
        ent5 (entnext ent4)     mer (_getattr ent5 "MER"))

  ;; ----------------------------------------------------------
  ;; ask for point in the quarter to know the quadrant
  ;; ----------------------------------------------------------
  (setq qtrpt (getpoint "\nPick a point in the quarter ... "))
  (setq x2 (car qtrpt)   y2 (cadr qtrpt))

  (cond
    ((and (< x2 x1) (> y2 y1)) (setq qtr "N.W."))
    ((and (> x2 x1) (> y2 y1)) (setq qtr "N.E."))
    ((and (< x2 x1) (< y2 y1)) (setq qtr "S.W."))
    ((and (> x2 x1) (< y2 y1)) (setq qtr "S.E."))
    (t                         (setq qtr ""))
  )

  ;; ----------------------------------------------------------
  ;; draw the quarter-section text (green)
  ;; ----------------------------------------------------------
  (setvar "CLAYER"    "C-SYMBOL")
  (setvar "CECOLOR"   "GREEN")
  (setvar "TEXTSTYLE" "175l")
  (command "_.TEXT" "J" "BC" qtrpt 90
           (strcat qtr "1/4 Sec. "
                   sec "-" twp "-" rge "-W." mer "M."))

  ;; ----------------------------------------------------------
  ;; Required ROW area (blue) – 22.5 units below qtrpt
  ;; ----------------------------------------------------------
  (setq qsha1 (getreal
               "\nRequired Right-of-Way Area in Hectares... "))
  (while (not (numberp qsha1))
    (prompt "\nPlease enter a numeric value.")
    (setq qsha1 (getreal
                 "\nRequired Right-of-Way Area in Hectares... "))
  )

  (if (< qsha1 1.0)
    (setq qsha (rtos qsha1 2 3))
    (setq qsha (rtos qsha1 2 2)))

  (setq qsac1  (* qsha1 2.471)
        qsac   (rtos qsac1 2 2)
        qtrpt2 (list x2 (- y2 22.5)))      ;fixed offset

  (setvar "CLAYER"    "L-AREAREQUIRED")
  (setvar "TEXTSTYLE" "100l")
  (setvar "CECOLOR"   "BLUE")
  (setq arealab (strcat "Area Required = "
                        qsha " ha  " qsac " Ac."))
  (command "_.TEXT" "J" "BC" qtrpt2 90 arealab)

  ;; ----------------------------------------------------------
  ;; Workspace area (blue) – another 20 units down
  ;; ----------------------------------------------------------
  (setq twha1 (getreal
               "\nRequired Workspace Area in Hectares... "))
  (while (not (numberp twha1))
    (prompt "\nPlease enter a numeric value.")
    (setq twha1 (getreal
                 "\nRequired Workspace Area in Hectares... "))
  )

  (if (< twha1 1.0)
    (setq twha (rtos twha1 2 3))
    (setq twha (rtos twha1 2 2)))

  (setq twac1  (* twha1 2.471)
        twac   (rtos twac1 2 2)
        qtrpt3 (list x2 (- y2 42.5)))      ;22.5 + 20

  (command "_.LAYER" "_M" "L-AREAWORK" "_C" "BLUE" "" "")
  (setvar "TEXTSTYLE" "100l")
  (setvar "CECOLOR"   "BLUE")
  (setq twsplab (strcat "Workspace = "
                        twha " ha  " twac " Ac."))
  (command "_.TEXT" "J" "BC" qtrpt3 90 twsplab)

  ;; ----------------------------------------------------------
  ;; tidy up
  ;; ----------------------------------------------------------
  (setvar "CECOLOR"   "BYLAYER")
  (setvar "TEXTSTYLE" oldtxt)
  (setvar "CLAYER"    oldlay)

  (princ)
)
(princ "\nType QSLAB3 to label a quarter section.")
