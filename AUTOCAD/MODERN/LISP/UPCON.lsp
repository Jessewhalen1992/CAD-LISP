;====================================================================
; UPCON.LSP – Connect Z-DESCRIPTION notes to Z-POINTNUMBERs
; rev 2025-07-14 – removed WHEN, exact matching, optional 3DPOLY
;====================================================================

(vl-load-com)

;; Transform World coordinates → Current UCS
(defun l+transw2c (pt)
  (trans pt 0 1)
)

(defun c:UPCON ( / upcon-error upcon-old-error kw xyz dEnt dStr dPt
                   ssDesc pairL dupSel n pr chosen
                   vtxL coord el i pt entry)

  ;; Generic error handler
  (defun upcon-error (msg)
    (if (and msg (/= msg "Function cancelled"))
      (princ (strcat "\nError: " msg)))
    (setq *error* upcon-old-error)
    (if upcon-old-error (upcon-old-error msg))
    (princ))
  (setq upcon-old-error *error* *error* upcon-error)

  ;; Ask whether to include Z values
  (initget "Yes No")
  (setq kw  (getkword "\nInclude Z value? [Yes/No] <No>: ")
        xyz (if (or (null kw) (= kw "No")) nil T))

  ;; Helper: get the true (x y z) for point-number string near XY, or NIL
  (defun ax-upcon-getCoord (ptext xy / sset ent coord)
    (setq sset
          (ssget "X"
                 (list (cons 0 "TEXT")
                       (cons 8 "Z-POINTNUMBER")
                       (cons 1 ptext)
                       (cons -4 "=,=")
                       (cons 10 xy))))
    (if (or (null sset) (= (sslength sset) 0))
      (setq sset
            (ssget "X"
                   (list (cons 0 "TEXT")
                         (cons 8 "Z-POINTNUMBER")
                         (cons 1 ptext)))))
    (if (and sset (> (sslength sset) 0))
      (progn
        (setq ent   (ssname sset 0)
              coord (cdr (assoc 10 (entget ent))))
        (if xyz
          (progn
            (setq sset
                  (ssget "X"
                         (list (cons 0 "POINT")
                               (cons 8 "Z-MSPOINT")
                               (cons -4 "=,=")
                               (cons 10 coord))))
            (if (and sset (> (sslength sset) 0))
              (setq coord
                    (cdr (assoc 10 (entget (ssname sset 0))))))))
        coord)
      nil))

  ;; STEP 1: pick reference description
  (prompt "\nSelect a Z-DESCRIPTION text to match: ")
  (setq dEnt (car (entsel)))
  (if (null dEnt)
      (progn (prompt "\nNothing selected.") (exit)))
  (setq dEnt (entget dEnt))
  (if (/= (strcase (cdr (assoc 8 dEnt))) "Z-DESCRIPTION")
      (progn (prompt "\nNot on Z-DESCRIPTION layer.") (exit)))
  (setq dStr (cdr (assoc 1 dEnt))
        dPt  (cdr (assoc 10 dEnt)))

  ;; STEP 2: find all descriptions with that exact text
  (setq ssDesc
        (ssget "X"
               (list (cons 0 "TEXT")
                     (cons 8 "Z-DESCRIPTION")
                     (cons 1 dStr))))
  (if (null ssDesc)
      (progn (prompt "\nNo matching descriptions found.") (exit)))

  ;; STEP 3: pair each description with its point-number at same XY
  (setq pairL '() i 0)
  (while (< i (sslength ssDesc))
    (setq el    (entget (ssname ssDesc i))
          dupSel
            (ssget "X"
                   (list (cons 0 "TEXT")
                         (cons 8 "Z-POINTNUMBER")
                         (cons -4 "=,=")
                         (cons 10 (cdr (assoc 10 el))))))
    (if (and dupSel (> (sslength dupSel) 0))
      (progn
        (setq n     (atoi (cdr (assoc 1 (entget (ssname dupSel 0))))))
        (setq pairL (cons (cons n (cdr (assoc 10 el))) pairL))))
    (setq i (1+ i)))
  (if (< (length pairL) 2)
      (progn (prompt "\nNeed at least two description/number pairs.") (exit)))

  ;; STEP 4: remove duplicate point-numbers keeping the one
  ;; nearest the originally selected description
  (setq pairL  (vl-sort pairL '(lambda (a b) (< (car a) (car b))))
        chosen '())
  (foreach pr pairL
    (setq entry (assoc (car pr) chosen))
    (cond
      ((null entry)
       (setq chosen (cons pr chosen)))
      ((> (distance dPt (cdr entry)) (distance dPt (cdr pr)))
       (setq chosen (subst pr entry chosen)))))
  (setq chosen (vl-sort chosen '(lambda (a b) (< (car a) (car b)))) )
  (if (< (length chosen) 2)
      (progn (prompt "\nFewer than two unique point-numbers.") (exit)))

  ;; STEP 5: build vertex list with or without Z
  (setq vtxL '())
  (foreach pr chosen
    (setq coord nil
          pt (cdr pr))
    (cond
      (xyz
       (setq coord (ax-upcon-getCoord (itoa (car pr)) pt)))
      ((and pt (> (length pt) 1))
       (setq coord (list (car pt) (car (cdr pt)) 0.0)))
      (t 
       (princ (strcat "\nWarning: Invalid point data for number " (itoa (car pr))))))
    (if coord
      (setq vtxL (cons (l+transw2c coord) vtxL))))
  (setq vtxL (reverse vtxL))
  (if (< (length vtxL) 2)
      (progn (prompt "\nInsufficient valid vertices – nothing drawn.") (exit)))

  ;; STEP 6: draw the polyline
  (prompt (strcat "\nDrawing " (if xyz "3D " "") "polyline..."))
  (command (if xyz "_3DPOLY" "_PLINE"))
  (foreach pt vtxL
    (command "_non" pt))
  (command "")
  (prompt (strcat "\nDone - " (itoa (length vtxL)) " points."))
  (setq *error* upcon-old-error)
  (princ)
)

(princ "\nUPCON loaded.  Type UPCON to run.")
(princ)