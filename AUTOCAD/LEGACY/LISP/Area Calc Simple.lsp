;; ======================================================================
;; AREACALC.LSP – Calculates area metrics & drops MTEXT summary (h = 10)
;; No fancy predicates, works on any AutoCAD 2000-present
;; ======================================================================

(defun c:AREACALC ( /
        ;; helper fns
        sqm->ha sqm->ac round-str
        ;; selections
        ssBoundary ssDispo ssCut ssWet
        ;; objects
        obj
        ;; numeric accumulators (m²)
        aTot aDispo aCut aWet
        aCombCut aNewCut aOutDisp aUpland
        ;; strings (rounded & formatted)
        hTot hDispo hCut hComb hNew hOut hWet hUpl
        aTotAc aDispoAc aCutAc aCombAc aNewAc aOutAc aWetAc aUplAc
        ;; misc
        doc st oldH pt mspace mtx
      )

  ;; ---------------------------------------------------------------
  ;; basic helpers
  ;; ---------------------------------------------------------------
  (vl-load-com)
  (defun sqm->ha (a) (/ a 10000.0))
  (defun sqm->ac (a) (/ a 4046.8564224))
  (defun round-str (v d) (rtos v 2 d))

  (prompt "\n=== Area Calculation Started ===")

  ;; ---------------------------------------------------------------
  ;; 1) Proposed boundary
  ;; ---------------------------------------------------------------
  (prompt "\nSelect Proposed Boundary Objects: ")
  (setq ssBoundary (ssget '((0 . "LWPOLYLINE,REGION,POLYLINE,CIRCLE,ELLIPSE"))))
  (if (null ssBoundary)
    (progn (prompt "\n❌  No boundary objects selected.") (princ) (exit))
  )
  (setq aTot 0.0)
  (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ssBoundary)))
    (setq obj (vlax-ename->vla-object e))
    (if (vlax-property-available-p obj 'Area)
      (setq aTot (+ aTot (vlax-get obj 'Area))))
  )

  ;; ---------------------------------------------------------------
  ;; 2) Disposition (P-EXISTINGDISPO)
  ;; ---------------------------------------------------------------
  (prompt "\nSelect Existing Disposition Objects (Layer P-EXISTINGDISPO): ")
  (setq ssDispo (ssget '((8 . "P-EXISTINGDISPO"))))
  (setq aDispo 0.0)
  (if ssDispo
    (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ssDispo)))
      (setq obj (vlax-ename->vla-object e))
      (if (vlax-property-available-p obj 'Area)
        (setq aDispo (+ aDispo (vlax-get obj 'Area))))
    )
  )

  ;; ---------------------------------------------------------------
  ;; 3) Clear-cut (P-EXISTINGCUT)
  ;; ---------------------------------------------------------------
  (prompt "\nSelect Clearcut Objects (Layer P-EXISTINGCUT): ")
  (setq ssCut (ssget '((8 . "P-EXISTINGCUT"))))
  (setq aCut 0.0)
  (if ssCut
    (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ssCut)))
      (setq obj (vlax-ename->vla-object e))
      (if (vlax-property-available-p obj 'Area)
        (setq aCut (+ aCut (vlax-get obj 'Area))))
    )
  )

  ;; interim maths
  (setq aCombCut (+ aDispo aCut)
        aNewCut  (- aTot aCombCut)
        aOutDisp (- aTot aDispo))

  ;; ---------------------------------------------------------------
  ;; 4) Wetland  (WETLAND)
  ;; ---------------------------------------------------------------
  (prompt "\nSelect Wetland Objects (Layer WETLAND): ")
  (setq ssWet (ssget '((8 . "WETLAND"))))
  (setq aWet 0.0)
  (if ssWet
    (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ssWet)))
      (setq obj (vlax-ename->vla-object e))
      (if (vlax-property-available-p obj 'Area)
        (setq aWet (+ aWet (vlax-get obj 'Area))))
    )
  )
  (setq aUpland (- aTot aWet))

  ;; ---------------------------------------------------------------
  ;; 5) Convert & round
  ;; ---------------------------------------------------------------
  (setq hTot   (round-str (sqm->ha aTot)       3)
        hDispo (round-str (sqm->ha aDispo)     3)
        hCut   (round-str (sqm->ha aCut)       3)
        hComb  (round-str (sqm->ha aCombCut)   3)
        hNew   (round-str (sqm->ha aNewCut)    3)
        hOut   (round-str (sqm->ha aOutDisp)   3)
        hWet   (round-str (sqm->ha aWet)       3)
        hUpl   (round-str (sqm->ha aUpland)    3))

  (setq aTotAc   (round-str (sqm->ac aTot)       2)
        aDispoAc (round-str (sqm->ac aDispo)     2)
        aCutAc   (round-str (sqm->ac aCut)       2)
        aCombAc  (round-str (sqm->ac aCombCut)   2)
        aNewAc   (round-str (sqm->ac aNewCut)    2)
        aOutAc   (round-str (sqm->ac aOutDisp)   2)
        aWetAc   (round-str (sqm->ac aWet)       2)
        aUplAc   (round-str (sqm->ac aUpland)    2))

  ;; ---------------------------------------------------------------
  ;; 6) MTEXT string (no \H overrides)
  ;; ---------------------------------------------------------------
  (setq mtx
    (strcat
      "{\\fArial|b0|i0|c0|p34;"
      "\\C7=== Area Calculation Results ===\\P\\C0"
      "\\P TOTAL AREA = "                hTot   " ha  " aTotAc   " Ac."
      "\\P AREA WITHIN DISPOSITION = "   hDispo " ha  " aDispoAc " Ac."
      "\\P AREA OUTSIDE DISPOSITION = "  hOut   " ha  " aOutAc   " Ac."
      "\\P CLEAR-CUT AREA (Dispo + Existing Cut) = "
                                         hComb  " ha  " aCombAc  " Ac."
      "\\P NEW CUT AREA = "              hNew   " ha  " aNewAc   " Ac."
      "\\P WETLAND AREA = "              hWet   " ha  " aWetAc   " Ac."
      "\\P UPLAND AREA = "               hUpl   " ha  " aUplAc   " Ac."
      "\\P === End ==="
    )
  )

  ;; ---------------------------------------------------------------
  ;; 7) Place MTEXT (object height = 10)
  ;; ---------------------------------------------------------------
  (setq pt (getpoint "\nPick insertion point for results MTEXT: "))
  (if (null pt) (setq pt '(0 0 0)))   ; ESC ➜ 0,0

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        mspace (vla-get-ModelSpace doc)
        st (vla-get-ActiveTextStyle doc)
        oldH (vla-get-Height st))
  ;; make sure style height = 0 so our object height is honoured
  (if (/= oldH 0.0) (vla-put-Height st 0.0))

  (setq mtx
    (vla-AddMText
      mspace
      (vlax-3d-point pt)
      0.0
      mtx))
  (vla-put-Height mtx 10.0)

  ;; restore original style height
  (if (/= oldH 0.0) (vla-put-Height st oldH))

  (prompt "\n✅  MTEXT summary created (height 10, no inline overrides).")
  (princ)
)
;; --------------------------  End of file  -----------------------------
