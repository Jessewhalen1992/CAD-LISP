(defun c:AreaCalculation ()
  ;; Load Visual LISP functions
  (vl-load-com)
  
  ;; Define Attachment Point constants
  (setq acAttachmentPointTopLeft 1
        acAttachmentPointTopCenter 2
        acAttachmentPointTopRight 3
        acAttachmentPointMiddleLeft 4
        acAttachmentPointMiddleCenter 5
        acAttachmentPointMiddleRight 6
        acAttachmentPointBottomLeft 7
        acAttachmentPointBottomCenter 8
        acAttachmentPointBottomRight 9)
  
  ;; Define helper functions inside the main function to ensure they are always loaded
  (defun safe-atof (str)
    ;; Converts string to float, returns 0.0 if conversion fails
    (if (and (eq (type str) 'STR) (/= str ""))
      (progn
        (setq num (atof str))
        (if (numberp num)
            num
            0.0))
      0.0))

  (defun RoundTo (value decimalPlaces)
    ;; Rounds the value to the specified number of decimal places
    (if (numberp value)
      (progn
        (setq factor (expt 10 decimalPlaces))
        (/ (float (fix (+ (* value factor) 0.5))) factor))
      0.0))

  (defun getBlockAttribute (block tag)
    ;; Retrieves the value of a specified attribute tag in a block
    (if (and (vlax-property-available-p block 'HasAttributes)
             (vla-get-HasAttributes block))
      (vl-some
        '(lambda (attrib)
           (if (eq (strcase (vla-get-TagString attrib)) (strcase tag))
             (vla-get-TextString attrib)))
        (vlax-invoke block 'GetAttributes))
      nil))

  (defun parseSizeAttribute (sizeAttr)
    ;; Parses the SIZE attribute to calculate area in hectares
    (setq sizeAttrUpper (strcase sizeAttr))
    ;; Case 1: Pattern like "10.0x40.0"
    (cond
      ((and (vl-string-search "X" sizeAttrUpper)
            (not (vl-string-search "-" sizeAttrUpper)))
        (setq xPos (+ (vl-string-search "X" sizeAttrUpper) 1))
        (setq widthStr (substr sizeAttr 1 (- xPos 1)))
        (setq heightStr (substr sizeAttr (+ xPos 1)))
        (setq width (safe-atof widthStr))
        (setq height (safe-atof heightStr))
        (if (and (> width 0) (> height 0))
          (* width height 0.0001)
          0.0))
      ;; Case 2: Pattern like "2-10.0x40.0"
      ((and (vl-string-search "-" sizeAttrUpper)
            (vl-string-search "X" sizeAttrUpper))
        (setq dashPos (+ (vl-string-search "-" sizeAttrUpper) 1))
        (setq xPos (+ (vl-string-search "X" sizeAttrUpper) 1))
        ;; Extract substrings
        (setq quantityStr (substr sizeAttr 1 (- dashPos 1)))
        (setq widthStr (substr sizeAttr (+ dashPos 1) (- xPos dashPos 1)))
        (setq heightStr (substr sizeAttr (+ xPos 1)))
        ;; Convert to numbers
        (setq quantity (safe-atof quantityStr))
        (setq width (safe-atof widthStr))
        (setq height (safe-atof heightStr))
        (if (and (> quantity 0) (> width 0) (> height 0))
          (* quantity width height 0.0001)
          0.0))
      ;; Case 3: Pattern like "0.015 ha"
      ((vl-string-search "HA" sizeAttrUpper)
        (setq haPos (vl-string-search "HA" sizeAttrUpper))
        (setq numberStr (substr sizeAttr 1 (- haPos 1)))
        (setq numberStr (vl-string-trim " " numberStr))
        (setq areaHa (safe-atof numberStr))
        (if (> areaHa 0)
            areaHa
            0.0))
      ;; Unrecognized format
      (t
        0.0)))

  (defun updateAssoc (key value alist)
    ;; Updates or adds a key-value pair in an association list
    (if (assoc key alist)
      (subst (cons key value) (assoc key alist) alist)
      (cons (cons key value) alist)))

  (defun PipelineOption ()
    ;; Reset pipelineOption variable
    (setq pipelineOption (strcase (getstring "\nSelect Option [Select Object (S)/Enter Area (E)]: ")))
    (cond
      ;; Select Object for Pipeline
      ((or (= pipelineOption "S") (= pipelineOption "SELECT OBJECT"))
       (setq obj (car (entsel "\nSelect an object: ")))
       (if obj
         (progn
           (setq areaSqM (vlax-get (vlax-ename->vla-object obj) 'Area))
           (setq areaHa (/ areaSqM 10000.0))
           (setq areaHaRounded (RoundTo areaHa 3))
           (setq areaAc (/ areaHaRounded 0.4047))
           (setq areaAcRounded (RoundTo areaAc 2))
           ;; Output for Pipeline with middle-bottom justification and Y-offset south
           (setvar "CLAYER" "L-AREAREQUIRED")
           (setq outputLocation (getpoint "\nSpecify text output location: "))
           (setq adjustedLocation (list (car outputLocation) (- (cadr outputLocation) 17.5) (caddr outputLocation)))
           (setq mTextObj (vla-AddMText
                            (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                            (vlax-3d-point adjustedLocation)
                            0
                            (strcat "Area Required = " (rtos areaHaRounded 2 3) " ha  " (rtos areaAcRounded 2 2) " Ac.")))
           (vla-put-Height mTextObj 10)
           (vla-put-Width mTextObj 0)
           ;; Set attachment point to bottom center
           (vla-put-AttachmentPoint mTextObj acAttachmentPointBottomCenter)
           ;; Reset the insertion point to adjusted location
           (vla-put-InsertionPoint mTextObj (vlax-3d-point adjustedLocation))
           (vla-put-Color mTextObj 5)
         )
         (princ "\nInvalid selection.")
       )
      )
      ;; Enter Area for Pipeline
      ((or (= pipelineOption "E") (= pipelineOption "ENTER AREA"))
       (setq areaHa (getreal "\nEnter area in hectares: "))
       (setq areaHaRounded (RoundTo areaHa 3))
       (setq areaAc (/ areaHaRounded 0.4047))
       (setq areaAcRounded (RoundTo areaAc 2))
       ;; Output for entered area in Pipeline
       (setvar "CLAYER" "L-AREAREQUIRED")
       (setq outputLocation (getpoint "\nSpecify text output location: "))
       (setq adjustedLocation (list (car outputLocation) (- (cadr outputLocation) 22.5) (caddr outputLocation)))
       (setq mTextObj (vla-AddMText
                        (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                        (vlax-3d-point adjustedLocation)
                        0
                        (strcat "Area Required = " (rtos areaHaRounded 2 3) " ha  " (rtos areaAcRounded 2 2) " Ac.")))
       (vla-put-Height mTextObj 10)
       (vla-put-Width mTextObj 0)
       ;; Set attachment point to bottom center
       (vla-put-AttachmentPoint mTextObj acAttachmentPointBottomCenter)
       ;; Reset the insertion point to adjusted location
       (vla-put-InsertionPoint mTextObj (vlax-3d-point adjustedLocation))
       (vla-put-Color mTextObj 5)
      )
      (t (princ "\nInvalid pipeline option selected."))
    )
    ;; Reset variables to ensure clean state for next run
    (setq pipelineOption nil)
    (setq obj nil)
    (setq areaSqM nil)
    (setq areaHa nil)
    (setq areaHaRounded nil)
    (setq areaAc nil)
    (setq areaAcRounded nil)
    (setq outputLocation nil)
    (setq adjustedLocation nil)
    (setq mTextObj nil)
  )
  (defun WorkspaceOption ()
    ;; Now only handles "Single Activity"
    (setq workspaceOptionType (strcase (getstring "\nSelect Option [Single Activity (S)]: ")))
    (cond
      ;; Single Activity
      ((or (= workspaceOptionType "S") (= workspaceOptionType "SINGLE ACTIVITY"))
        ;; Prompt user to enter the WORKSPACE name for the activity
        (setq activityName (getstring T "\nEnter the WORKSPACE name for the activity: "))
        (princ "\nSelect blocks (use crossing window, window, or individual selection, then press Enter when done): ")
        ;; Allow multiple selection methods
        (setq selectionSet (ssget '((0 . "INSERT"))))
        ;; Check if selection is not nil
        (if selectionSet
          (progn
            (setq count (sslength selectionSet))
            (setq idx 0)
            (setq totalArea 0.0)
            ;; Loop over each selected block
            (while (< idx count)
              (setq ename (ssname selectionSet idx))
              (if ename
                (progn
                  (setq obj (vlax-ename->vla-object ename))
                  (setq sizeAttr (getBlockAttribute obj "SIZE"))
                  (setq workspaceAttr (getBlockAttribute obj "WORKSPACE"))
                  ;; Check if SIZE attribute exists and WORKSPACE matches
                  (if (and sizeAttr (equal (strcase workspaceAttr) (strcase activityName)))
                    (progn
                      (setq areaHa (parseSizeAttribute sizeAttr))
                      (setq totalArea (+ totalArea areaHa))
                    )
                    (princ "\nWarning: SIZE attribute missing or WORKSPACE does not match.")
                  )
                )
                (princ (strcat "\nError: Could not get entity at index " (itoa idx)))
              )
              (setq idx (1+ idx))
            )
            ;; Output the result with middle-bottom justification and Y-offset south
            (if (> totalArea 0.0)
              (progn
                (setq totalAreaHaRounded (RoundTo totalArea 3))
                (setq areaAc (/ totalAreaHaRounded 0.4047))
                (setq totalAreaAcRounded (RoundTo areaAc 2))
                ;; Create text
                (setq outputLocation (getpoint "\nSpecify text output location: "))
                (setq adjustedLocation (list (car outputLocation) (- (cadr outputLocation) 22.5) (caddr outputLocation)))
                (setvar "CLAYER" "L-AREAWORK")
                (setq mTextObj (vla-AddMText
                                 (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                                 (vlax-3d-point adjustedLocation)
                                 0
                                 (strcat activityName " = " (rtos totalAreaHaRounded 2 3) " ha  " (rtos totalAreaAcRounded 2 2) " Ac.")))
                (vla-put-Height mTextObj 10)
                (vla-put-Width mTextObj 0)
                ;; Set attachment point to bottom center
                (vla-put-AttachmentPoint mTextObj acAttachmentPointBottomCenter)
                ;; Reset the insertion point to adjusted location
                (vla-put-InsertionPoint mTextObj (vlax-3d-point adjustedLocation))
                (vla-put-Color mTextObj 5)
              )
              (princ "\nNo valid areas found to output.")
            )
          )
          (princ "\nNo blocks selected.")
        )
      )
      (t
        (princ "\nInvalid workspace option selected.")
      )
    )
  )

  (defun OtherOption ()
    ;; Prompt user to input custom label, allowing spaces
    (setq customLabel (getstring T "\nEnter label for Other option (e.g., WELL SITE): "))
    
    ;; Offer options similar to Pipeline
    (setq otherOption (strcase (getstring "\nSelect Option [Select Object (S)/Enter Area (E)]: ")))
    (cond
      ;; Select Object for Other
      ((or (= otherOption "S") (= otherOption "SELECT OBJECT"))
       (setq obj (car (entsel "\nSelect an object: ")))
       (if obj
         (progn
           ;; Get area in square meters and convert to hectares and acres
           (setq areaSqM (vlax-get (vlax-ename->vla-object obj) 'Area))
           (setq areaHa (/ areaSqM 10000.0))
           (setq areaHaRounded (RoundTo areaHa 3))
           (setq areaAc (/ areaHaRounded 0.4047))
           (setq areaAcRounded (RoundTo areaAc 2))
           ;; Output for Other option with custom label
           (setvar "CLAYER" "L-AREAREQUIRED")
           (setq outputLocation (getpoint "\nSpecify text output location: "))
           (setq adjustedLocation (list (car outputLocation) (- (cadr outputLocation) 22.5) (caddr outputLocation)))
           (setq mTextObj (vla-AddMText
                            (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                            (vlax-3d-point adjustedLocation)
                            0
                            (strcat customLabel " = " (rtos areaHaRounded 2 3) " ha  " (rtos areaAcRounded 2 2) " Ac.")))
           (vla-put-Height mTextObj 10)
           (vla-put-Width mTextObj 0)
           ;; Set attachment point to bottom center
           (vla-put-AttachmentPoint mTextObj acAttachmentPointBottomCenter)
           ;; Reset the insertion point to adjusted location
           (vla-put-InsertionPoint mTextObj (vlax-3d-point adjustedLocation))
           (vla-put-Color mTextObj 5)
         )
         (princ "\nInvalid selection.")
       )
      )
      ;; Enter Area for Other
      ((or (= otherOption "E") (= otherOption "ENTER AREA"))
       ;; Get entered area and convert
       (setq areaHa (getreal "\nEnter area in hectares: "))
       (setq areaHaRounded (RoundTo areaHa 3))
       (setq areaAc (/ areaHaRounded 0.4047))
       (setq areaAcRounded (RoundTo areaAc 2))
       ;; Output for entered area with custom label
       (setvar "CLAYER" "L-AREAREQUIRED")
       (setq outputLocation (getpoint "\nSpecify text output location: "))
       (setq adjustedLocation (list (car outputLocation) (- (cadr outputLocation) 22.5) (caddr outputLocation)))
       (setq mTextObj (vla-AddMText
                        (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                        (vlax-3d-point adjustedLocation)
                        0
                        (strcat customLabel " = " (rtos areaHaRounded 2 3) " ha  " (rtos areaAcRounded 2 2) " Ac.")))
       (vla-put-Height mTextObj 10)
       (vla-put-Width mTextObj 0)
       ;; Set attachment point to bottom center
       (vla-put-AttachmentPoint mTextObj acAttachmentPointBottomCenter)
       ;; Reset the insertion point to adjusted location
       (vla-put-InsertionPoint mTextObj (vlax-3d-point adjustedLocation))
       (vla-put-Color mTextObj 5)
      )
      (t (princ "\nInvalid option selected for Other."))
    )
    ;; Reset variables to ensure clean state for next run
    (setq customLabel nil)
    (setq otherOption nil)
    (setq obj nil)
    (setq areaSqM nil)
    (setq areaHa nil)
    (setq areaHaRounded nil)
    (setq areaAc nil)
    (setq areaAcRounded nil)
    (setq outputLocation nil)
    (setq adjustedLocation nil)
    (setq mTextObj nil)
  )

  ;; Main function to calculate areas based on user selection
  (setq userChoice (strcase (getstring "\nChoose Option [Pipeline (P)/Workspace (W)/Other (O)]: ")))
  (cond
    ((or (= userChoice "P") (= userChoice "PIPELINE"))
      (PipelineOption)
    )
    ((or (= userChoice "W") (= userChoice "WORKSPACE"))
      (WorkspaceOption)
    )
    ((or (= userChoice "O") (= userChoice "OTHER"))
      (OtherOption)
    )
    (t
      (princ "\nInvalid option selected.")
    )
  )
  (princ)
)
