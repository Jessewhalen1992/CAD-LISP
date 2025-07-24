;;; Utils.LSP - Land Surveying Utility Functions (Updated for AutoCAD 2015-2025)
;;; This script provides various helpful commands and functions for survey drafting.
;;; 
;;; Features:
;;;   - Line breaking at a specified point
;;;   - Entity cloning (duplicate objects in place)
;;;   - Entity layer management (set current layer by object, move/copy objects to layer, freeze/lock layers by object)
;;;   - Angle conversions (bearing <-> azimuth, degrees <-> radians)
;;;   - Text manipulation (convert text to uppercase)
;;;   - User input utilities (e.g., get point with Z=0)
;;;
;;; Update Notes (March 2025):
;;;   * Removed obsolete menu loading calls (e.g., loadmenu). If a custom menu was used, load it via CUI manually.
;;;   * No Civil 3D-specific code (this version is for vanilla AutoCAD only).
;;;   * Replaced older AutoLISP techniques with modern VL/VLAX functions where appropriate (e.g., use of ActiveX for copying entities).
;;;   * Ensured compatibility with AutoCAD 2015 through 2025. Deprecated functions and system variables have been replaced with current equivalents.
;;;   * Preserved all originally useful functionality; commented out or removed only obsolete or non-functional code.
;;;
;;; ------------------------------------------------------------------------------

(vl-load-com)  ; Load ActiveX support (Visual LISP)

;; Obsolete initialization (if any) has been removed. 
;; For example, menu loading was done here in older version (no longer needed):
;; (if (not (menugroup "UTILS")) (loadmenu "UTILS"))  ; *** Removed: use CUI to load custom menus if needed ***

;; (Optional) Set or restore desired system variables for environment (using setvar instead of commands):
;; (setvar 'CMDECHO 1)
;; (setvar 'FILEDIA 1)
;; (setvar 'OSNAPZ 1)  ; e.g., ensure snapping uses current elevation (useful for 2D drafting)

;;; Conversion Utilities: Degrees/Radians and Bearing/Azimuth
(defun deg2rad (deg)
  "Convert angle in degrees to radians."
  (* pi (/ deg 180.0))
)
(defun rad2deg (rad)
  "Convert angle in radians to degrees."
  (/ (* rad 180.0) pi)
)
;; For backward compatibility, provide DTR and RTD if used in older code
(defun utils-dtr (a) (deg2rad a))
(defun utils-rtd (a) (rad2deg a))

(defun bearing->angle (bearStr / str deg min sec partAngle azDeg)
  "Convert a bearing string (e.g., \"N45d30'0\\\"E\") to an absolute angle in radians (AutoCAD's standard angle measure).
Returns the angle measured from East (0° East, counter-clockwise positive) as a radians value."
  ;; Normalize input string: remove spaces and uppercase for consistent parsing
  (setq str (strcase (vl-string-trim " " bearStr)))
  (if (not (and (> (strlen str) 2)
                (member (substr str 1 1) '("N" "S"))
                (member (substr str (strlen str) 1) '("E" "W"))))
    (progn
      (princ "\nInvalid bearing format.")
      (return nil)
    )
  )
  ;; Extract the numeric part between the first and last character (exclude N/S and E/W)
  (setq str (substr str 2 (- (strlen str) 2)))
  ;; Parse degrees, minutes, seconds
  (setq deg 0.0  min 0.0  sec 0.0  partAngle 0.0)
  (if (vl-string-search "d" str)
    (progn
      ;; Degrees (substring before 'd')
      (setq deg (atof (substr str 1 (vl-string-search "d" str))))
      ;; Minutes (if any, between 'd' and '\' or end of string)
      (if (vl-string-search "'" str)
        (setq min (atof (substr str (+ 2 (vl-string-search "d" str))
                                 (- (or (vl-string-search "'" str) (1- (strlen str)))
                                    (1+ (vl-string-search "d" str))))))
      )
      ;; Seconds (if any, after "'" up to "\"")
      (if (vl-string-search "\"" str)
        (setq sec (atof (substr str (+ 2 (vl-string-search "'" str))
                                 (- (vl-string-search "\"" str)
                                    (1+ (vl-string-search "'" str))))))
      )
      (setq partAngle (+ deg (/ min 60.0) (/ sec 3600.0)))
    )
    ;; If no 'd' in string, interpret entire numeric part as decimal degrees
    (setq partAngle (atof str))
  )
  ;; Determine azimuth (angle from North, clockwise, in degrees)
  (cond
    ((= (substr bearStr 1 1) "N")
      (if (= (substr bearStr (strlen bearStr) 1) "E")
          (setq azDeg partAngle)
          (setq azDeg (if (zerop partAngle) 360.0 (- 360.0 partAngle))))
    )
    ((= (substr bearStr 1 1) "S")
      (if (= (substr bearStr (strlen bearStr) 1) "E")
          (setq azDeg (- 180.0 partAngle))
          (setq azDeg (+ 180.0 partAngle)))
    )
  )
  (if (>= azDeg 360.0) (setq azDeg (- azDeg 360.0)))  ; normalize 360 to 0
  ;; Convert azimuth (from North CW) to AutoCAD angle (from East CCW) and return in radians
  (deg2rad (rem (- 450.0 azDeg) 360.0))
)

(defun angle->bearing (angRad / degE degN angleD firstChar secondChar bearingStr)
  "Convert an angle in radians (AutoCAD standard orientation) to a bearing string (e.g., \"N45d30'0\\\"E\").
The input angle is assumed to be measured from East (0 at East, CCW positive)."
  (setq degE (rad2deg angRad))
  ;; Calculate angle measured from North clockwise (azimuth)
  (setq degN (rem (- 450.0 degE) 360.0))
  ;; Determine quadrant and internal angleD (0 to 90 degrees within that quadrant)
  (cond
    ((or (>= degN 360.0) (< degN 1e-8))  ; close to 0 or 360 -> due North
      (setq firstChar "N" secondChar "E" angleD 0.0)  ; represent as N0°0'0"E (due North)
    )
    ((<= degN 90.0)
      (setq firstChar "N" secondChar "E" angleD degN)
    )
    ((<= degN 180.0)
      (setq firstChar "S" secondChar "E" angleD (- 180.0 degN))
    )
    ((< degN 270.0)
      (setq firstChar "S" secondChar "W" angleD (- degN 180.0))
    )
    (T
      (setq firstChar "N" secondChar "W" angleD (- 360.0 degN))
    )
  )
  ;; Format the angleD (0-90) as degrees/minutes/seconds string
  (setq bearingStr (angtos (deg2rad angleD) 1 0))  ; e.g., "45d30'0\""
  ;; Construct the full bearing string with quadrant letters
  (strcat firstChar bearingStr secondChar)
)

;;; User Input Helpers
(defun getpointXY (prompt / pt)
  "Like getpoint, but returns the picked point with Z forced to 0 (flattened to XY plane)."
  (setq pt (getpoint prompt))
  (if pt (setq pt (list (car pt) (cadr pt) 0.0)))
  pt
)

;;; Line Utility: Break a line at a specified point
(defun c:BreakAtPoint (/ ent obj pickPt breakVar breakPt sp ep newObj)
  "Breaks a LINE at a user-specified point. The original line is split into two lines at the break point."
  (setq ent (car (entsel "\nSelect a LINE to break: ")))
  (if (and ent (eq (cdr (assoc 0 (entget ent))) "LINE"))
    (progn
      (setq pickPt (getpoint "\nSpecify break point on the line: " ent))
      (if (null pickPt)
        (princ "\nPoint not specified.")
        (progn
          (setq obj (vlax-ename->vla-object ent))
          ;; Get closest point on the line segment to the picked point
          (setq breakVar (vlax-curve-getClosestPointTo obj (vlax-3D-point pickPt) nil)
                breakPt  (vlax-safearray->list (vlax-variant-value breakVar)))
          ;; Get current line's endpoints
          (setq sp (vlax-curve-getStartPoint obj)
                ep (vlax-curve-getEndPoint obj))
          ;; If break point is effectively at an endpoint, cancel the operation
          (if (or (< (distance sp breakPt) 1e-8)
                  (< (distance ep breakPt) 1e-8))
            (princ "\nBreak point is too close to an endpoint; no break made.")
            (progn
              ;; Duplicate the original line (ActiveX copy preserves all properties)
              (setq newObj (vla-copy obj))
              ;; Set new endpoints: original from SP->breakPt, new from breakPt->EP
              (vla-put-EndPoint obj (vlax-3D-point breakPt))
              (vla-put-StartPoint newObj (vlax-3D-point breakPt))
            )
          )
        )
      )
    )
    (princ "\nInvalid selection. Please select a LINE object.")
  )
  (princ)
)

;;; Clone Utility: Clone an entity in place
(defun c:CloneEntity (/ ent obj newObj)
  "Clones (duplicates) a selected entity at the same location."
  (setq ent (car (entsel "\nSelect entity to clone: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent)
            newObj (vla-copy obj))  ; copy the object (modern equivalent of entmake clone)
      (princ "\nEntity cloned.")
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

;;; Layer Utilities
(defun c:SetLayerToObject (/ ent layerName)
  "Sets the current layer (CLAYER) to the layer of a selected object."
  (setq ent (car (entsel "\nSelect object to set its layer current: ")))
  (if ent
    (progn
      (setq layerName (cdr (assoc 8 (entget ent))))
      (setvar 'CLAYER layerName)
      (princ (strcat "\nCurrent layer set to " layerName))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

(defun c:MoveToLayer (/ ss layerName ent obj)
  "Moves selected objects to a specified layer. If the layer does not exist, it will be created."
  (if (setq ss (ssget "_I"))
    (progn
      (setq layerName (getstring "\nEnter target layer name: "))
      (if (or (null layerName) (= layerName ""))
        (progn (princ "\nNo layer name provided.") (return))
      )
      (setq layerName (strcase layerName))  ; normalize layer name
      ;; Create layer if it doesn't exist
      (if (not (tblsearch "LAYER" layerName))
        (progn
          (vla-add (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) layerName)
          (princ (strcat "\n** Created new layer: " layerName " **"))
        )
      )
      ;; Move each selected entity to the target layer
      (repeat (sslength ss)
        (setq ent (ssname ss 0)
              ss  (ssdel ent ss)
              obj (vlax-ename->vla-object ent))
        (vla-put-Layer obj layerName)
      )
      (princ (strcat "\nMoved selected object(s) to layer " layerName "."))
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)

(defun c:CopyToLayer (/ ss layerName ent obj newObj)
  "Copies selected objects to a specified layer (keeps originals on their layer). Creates the layer if needed."
  (if (setq ss (ssget "_I"))
    (progn
      (setq layerName (getstring "\nEnter target layer name: "))
      (if (or (null layerName) (= layerName ""))
        (progn (princ "\nNo layer name provided.") (return))
      )
      (setq layerName (strcase layerName))
      (if (not (tblsearch "LAYER" layerName))
        (progn
          (vla-add (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) layerName)
          (princ (strcat "\n** Created new layer: " layerName " **"))
        )
      )
      ;; Copy each entity and assign it to the target layer
      (repeat (sslength ss)
        (setq ent (ssname ss 0)
              ss  (ssdel ent ss)
              obj (vlax-ename->vla-object ent)
              newObj (vla-copy obj))
        (vla-put-Layer newObj layerName)
      )
      (princ (strcat "\nCopied selected object(s) to layer " layerName "."))
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)

(defun c:FreezeLayerByObject (/ ent layName)
  "Freezes the layer of a selected object (global freeze in drawing)."
  (setq ent (car (entsel "\nSelect object to freeze its layer: ")))
  (if ent
    (progn
      (setq layName (cdr (assoc 8 (entget ent))))
      ;; Use -LAYER command to freeze (preferred over legacy loadmenu/menucmd methods)
      (command "-layer" "freeze" layName "") 
      (princ (strcat "\nLayer \"" layName "\" has been frozen."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

(defun c:LockLayerByObject (/ ent layName layerObj)
  "Locks the layer of a selected object."
  (setq ent (car (entsel "\nSelect object to lock its layer: ")))
  (if ent
    (progn
      (setq layName (cdr (assoc 8 (entget ent)))
            layerObj (vla-item (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) layName))
      (vla-put-Lock layerObj :vlax-true)
      (princ (strcat "\nLayer \"" layName "\" is now locked."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

(defun c:UnlockLayerByObject (/ ent layName layerObj)
  "Unlocks the layer of a selected object."
  (setq ent (car (entsel "\nSelect object to unlock its layer: ")))
  (if ent
    (progn
      (setq layName (cdr (assoc 8 (entget ent)))
            layerObj (vla-item (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) layName))
      (vla-put-Lock layerObj :vlax-false)
      (princ (strcat "\nLayer \"" layName "\" is now unlocked."))
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;;; Text Utility: Convert text to uppercase
(defun c:TextToUpper (/ ss ent obj type textVal)
  "Converts the contents of selected TEXT, MTEXT, or Attribute entities to uppercase.
Handles single-line text, MText, attributes, and attribute definitions."
  (if (setq ss (ssget "_I" '((0 . "TEXT,MTEXT,ATTRIB,ATTDEF"))))
    (progn
      (while (setq ent (ssname ss 0))
        (setq ss  (ssdel ent ss)
              obj (vlax-ename->vla-object ent)
              type (vla-get-ObjectName obj))
        ;; Get current text content depending on object type
        (setq textVal 
              (if (= type "AcDbMText")
                (vla-get-Contents obj)
                (vla-get-TextString obj)
              ))
        ;; Convert variant to string if necessary, then to uppercase
        (setq textVal (strcase (if (vlax-variant-p textVal) (vlax-variant-value textVal) textVal)))
        ;; Set the text content back
        (if (= type "AcDbMText")
          (vla-put-Contents obj textVal)
          (vla-put-TextString obj textVal)
        )
      )
      (princ "\nSelected text has been converted to UPPERCASE.")
      ;; Note: Uppercasing MText will also uppercase any formatting codes (e.g., \\L and \\l for underline),
      ;; which may affect text formatting.
    )
    (princ "\nNo text entities selected.")
  )
  (princ)
)

(princ)  ; End of file quietly
