;;; ===================================================================
;;; COGO.LSP – 2025-06-09
;;;   • Robust DDD.MMSS parser (AZ)
;;;   • Reject negative / zero distance in AT
;;; ===================================================================

;; ---------- (mod) helper ------------------------------------------------
(defun mod (val divisor / remainder)
  (setq remainder (rem val divisor))
  (if (< remainder 0) (setq remainder (+ remainder divisor)))
  remainder)

;; ---------- chooseUnit --------------------------------------------------
(defun chooseUnit ( / unit conv unitStr )
  (initget "Meters Feet Chains")
  (setq unit (getkword "\nSelect unit of measurement [Meters/Feet/Chains]: "))
  (if (null unit) (progn (prompt "\n**Cancelled**") (exit)))
  (cond
    ((equal unit "Meters") (setq conv 1.0      unitStr "Meters"))
    ((equal unit "Feet")   (setq conv 0.3048   unitStr "Feet"))
    ((equal unit "Chains") (setq conv 20.1168  unitStr "Chains"))
    (t                     (setq conv 1.0      unitStr "Meters")))
  (list conv unitStr))

;; ---------- round -------------------------------------------------------
(defun round (val / i f)
  (setq i (atoi (rtos val 2 0))
        f (- val i))
  (if (>= f 0.5) (+ i 1) i))

;; ---------- parseBearing ------------------------------------------------
(defun parseBearing (brgStr / s f l n deg mn)
  (setq s (strcase brgStr))
  (if (< (strlen s) 3) (progn (prompt "\nInvalid bearing format.") (exit)))
  (setq f (substr s 1 1)
        l (substr s (strlen s) 1)
        n (atof (substr s 2 (- (strlen s) 1))))
  (setq deg (fix n)
        mn  (* (- n deg) 100.0)
        n   (+ deg (/ mn 60.0)))
  (cond
    ((and (equal f "N") (equal l "E")) n)
    ((and (equal f "N") (equal l "W")) (- 360 n))
    ((and (equal f "S") (equal l "E")) (- 180 n))
    ((and (equal f "S") (equal l "W")) (+ 180 n))
    (T (prompt "\nInvalid bearing quadrant.") (exit))))

;; ======================================================================
;;  AZ  – Azimuth/Distance Traverse (DDD.MMSS)
;;        <Enter> repeats last angle
;; ======================================================================
(defun C:AZ ( / conv unitStr startPt currentPt prevAng angPrompt
               angIn intDeg mmss angMin angSec angDec angRad
               distIn distM nextPt oldCMDECHO oldDYNMODE tmp )

  ;; silence command & dynamic input
  (setq oldCMDECHO (getvar "CMDECHO")
        oldDYNMODE (getvar "DYNMODE"))
  (setvar "CMDECHO" 0) (setvar "DYNMODE" 0)

  (unwind-protect
    (progn
      ;; units
      (setq tmp     (chooseUnit)
            conv    (nth 0 tmp)
            unitStr (nth 1 tmp))

      ;; starting point
      (setq startPt (getpoint "\nSpecify starting point for the traverse: "))
      (if (null startPt) (progn (prompt "\n**Cancelled**") (exit)))
      (setq currentPt startPt  prevAng nil)

      (while T
        ;; prompt (with default)
        (setq angPrompt
              (if prevAng
                (strcat "\nEnter azimuth angle (DDD.MMSS) <"
                        (rtos prevAng 2 4) ">: ")
                "\nEnter azimuth angle (DDD.MMSS): "))
        (setq angIn (getreal angPrompt))

        ;; Enter = reuse previous angle
        (if (null angIn)
          (if prevAng
              (setq angIn prevAng)           ; repeat last
              (progn (prompt "\nTraverse completed.") (exit)))) ; no angle yet

        ;; negative → equivalent positive azimuth
        (if (< angIn 0.0)
          (progn
            (setq angIn (- 180.0 angIn))
            (if (< angIn 0.0) (setq angIn (+ angIn 360.0)))))

        ;; -------- robust DDD.MMSS → deg/min/sec --------------------
        ;; convert fractional part to a 4-digit integer (MMSS) so FP
        ;; rounding can’t turn 2.26 into 2°25'60"
        (setq intDeg (fix angIn)
              mmss   (fix (+ 0.5 (* (- angIn intDeg) 10000))) ; rounded
              angMin (fix (/ mmss 100))
              angSec (rem mmss 100))

        (if (or (>= angMin 60) (>= angSec 60))
          (progn (prompt "\nInvalid DDD.MMSS.") (princ)) ; stay in loop
          (progn
            ;; decimal degrees → radians
            (setq angDec (+ intDeg (/ angMin 60.0) (/ angSec 3600.0))
                  angRad (* (- 90.0 angDec) (/ pi 180.0)))

            ;; distance (no zero / negative)
            (initget 6)
            (setq distIn (getreal (strcat "\nEnter distance in " unitStr ": ")))
            (if (null distIn) (progn (prompt "\nTraverse completed.") (exit)))
            (setq distM (* distIn conv))

            ;; compute & draw
            (setq nextPt (list
                          (+ (car currentPt) (* distM (cos angRad)))
                          (+ (cadr currentPt) (* distM (sin angRad)))
                          (caddr currentPt)))
            (command "LINE" currentPt nextPt "")
            (setq currentPt nextPt
                  prevAng   angIn))))
      )
    )
    ;; restore vars
    (setvar "CMDECHO" oldCMDECHO)
    (setvar "DYNMODE"  oldDYNMODE))
  (princ))

;; ======================================================================
;;  AT – Angle-deflection Traverse
;;       (now rejects negative / zero distance)
;; ======================================================================
(defun C:AT ( / conv unitStr setupPt backPt angleCAD baseBrg defIn
               defDeg defMin defSec defAng distIn distM finalAng
               rad lastPt nextPt oldCMDECHO oldDYNMODE tmp )
  (setq oldCMDECHO (getvar "CMDECHO")
        oldDYNMODE (getvar "DYNMODE"))
  (setvar "CMDECHO" 0) (setvar "DYNMODE" 0)

  (unwind-protect
    (progn
      (setq tmp     (chooseUnit)
            conv    (nth 0 tmp)
            unitStr (nth 1 tmp))
      (prompt "\nAngle Traverse: pick setup point: ")
      (setq setupPt (getpoint))
      (if (null setupPt) (progn (prompt "\n**Cancelled**") (exit)))

      (initget 1)
      (setq backPt (getpoint setupPt "\nPick backsight point: "))
      (if (null backPt) (progn (prompt "\n**Cancelled**") (exit)))

      (setq angleCAD (* (angle setupPt backPt) (/ 180.0 pi))
            baseBrg  (mod (- 90 angleCAD) 360)
            lastPt   setupPt)
      (prompt (strcat "\nInitial bearing: "
                      (rtos baseBrg 2 2) (chr 176)))

      (while T
        (initget 4)
        (setq defIn (getreal
                      "\nEnter deflection DD.MMSS <Enter to quit>: "))
        (if (null defIn)
          (progn (prompt "\nTraverse completed.") (exit)))

        (setq defDeg (fix defIn)
              defMin (fix (* (- defIn defDeg) 100))
              defSec (* (- (* (- defIn defDeg) 100) defMin) 100)
              defAng (+ defDeg (/ defMin 60.0) (/ defSec 3600.0))
              baseBrg (mod (+ baseBrg defAng) 360))

        ;; distance (no zero / negative)
        (initget 6)
        (setq distIn (getreal
                       (strcat "\nEnter distance in " unitStr ": ")))
        (if (null distIn)
          (progn (prompt "\nTraverse completed.") (exit)))

        (setq distM    (* distIn conv)
              finalAng (mod (- 90 baseBrg) 360)
              rad      (* finalAng (/ pi 180.0))
              nextPt   (list
                         (+ (car lastPt) (* distM (cos rad)))
                         (+ (cadr lastPt) (* distM (sin rad)))
                         (caddr lastPt)))
        (command "LINE" lastPt nextPt "")
        (setq lastPt nextPt)))
    (setvar "CMDECHO" oldCMDECHO)
    (setvar "DYNMODE"  oldDYNMODE))
  (princ))

;; ======================================================================
;;  BRG – Bearing / Distance Traverse  (unchanged)
;; ======================================================================
(defun C:BRG ( / conv unitStr startPt currentPt brgStr azDeg rad
               distIn distM nextPt oldCMDECHO oldDYNMODE tmp )
  (setq oldCMDECHO (getvar "CMDECHO")
        oldDYNMODE (getvar "DYNMODE"))
  (setvar "CMDECHO" 0) (setvar "DYNMODE" 0)

  (unwind-protect
    (progn
      (setq tmp     (chooseUnit)
            conv    (nth 0 tmp)
            unitStr (nth 1 tmp))
      (prompt "\nBRG: Bearing Traverse initiated.")
      (setq startPt (getpoint "\nPick first point: "))
      (if (null startPt) (progn (prompt "\n**Cancelled**") (exit)))
      (setq currentPt startPt)

      (while T
        (setq brgStr (getstring T
                  "\nEnter bearing (e.g. N02.4503E): "))
        (if (or (null brgStr) (= brgStr ""))
          (progn (prompt "\nTraverse completed.") (exit)))

        (setq azDeg  (parseBearing brgStr)
              rad    (* azDeg (/ pi 180.0)))

        (initget 6)
        (setq distIn (getreal
                       (strcat "\nEnter distance in " unitStr ": ")))
        (if (null distIn)
          (progn (prompt "\nTraverse completed.") (exit)))

        (setq distM  (* distIn conv)
              nextPt (list
                       (+ (car currentPt) (* distM (sin rad)))
                       (+ (cadr currentPt) (* distM (cos rad)))
                       (caddr currentPt)))
        (command "LINE" currentPt nextPt "")
        (setq currentPt nextPt)))
    (setvar "CMDECHO" oldCMDECHO)
    (setvar "DYNMODE"  oldDYNMODE))
  (princ))

;; --------------------------------------------------------------------
(princ "\nAvailable commands:  AZ   AT   BRG\n")
(princ)
