;; Global variables to store the last used crossing number and section label
(setq last-crossing-number 1)
(setq last-section-label nil)
(setq last-location nil)

;; Custom function to split a string by a delimiter
(defun my-string-split (delimiter str)
  (if (vl-string-search delimiter str)
    (cons (substr str 1 (vl-string-search delimiter str))
          (my-string-split delimiter (substr str (+ (strlen delimiter) (vl-string-search delimiter str)))))
    (list str)
  )
)

(defun clean-description (desc keyword)
  ;; Remove the keyword from the description
  (setq desc (vl-string-subst "" keyword desc))

  ;; Handle \pxqc and \P characters
  (setq desc (vl-string-subst "" "\\pxqc;" desc))  ;; Remove \pxqc;
  (while (or (vl-string-search "\\P" desc) (vl-string-search "\n" desc))
    (setq desc (vl-string-subst " " "\\P" desc))  ;; Replace \P with spaces
    (setq desc (vl-string-subst " " "\n" desc))  ;; Replace \n with spaces
  )

  ;; Remove any formatting codes before the first semicolon
  (if (vl-string-search ";" desc)
    (setq desc (substr desc (+ 2 (vl-string-search ";" desc))))  ;; Remove everything before and including the semicolon
  )

  ;; Remove the final '}'
  (if (vl-string-search "}" desc)
    (setq desc (vl-string-subst "" "}" desc))
  )

  ;; Remove unwanted formatting characters like pipes '|'
  (setq desc (vl-string-subst "" "|" desc))

  ;; Remove the font formatting "{\\f" if present
  (setq desc (vl-string-subst "" "{\\f" desc))

  ;; Remove multiple spaces
  (while (vl-string-search "  " desc)
    (setq desc (vl-string-subst " " "  " desc))
  )

  ;; Return the cleaned description
  (vl-string-trim " " desc)
)

(defun c:cbg ( / dispLabel owner desc secLabel loc dispObj secObj objType insPt blockObj attList attObj attTag dispLayer keywords crossingNumber useLastSection)
  ;; Block name for insertion
  (setq blockName "xing2")
  ;; Keywords for specified owners
  (setq keywords '("Telus" "Fortis" "Government" "ATCO"))
  ;; Set the current layer to "P-CROSSINGS"
  (setvar "CLAYER" "P-CROSSINGS")
  ;; Step 1: Ask for Crossing #
  (setq crossingNumber (getstring (strcat "\nCROSSING #? <" (itoa last-crossing-number) ">: ")))
  (if (= crossingNumber "")
    (setq crossingNumber (itoa last-crossing-number))  ;; Default to last-crossing-number if none entered
  )
  (setq crossingNumber (strcat "X" crossingNumber))  ;; Format it as X[number]
  
  ;; Store the next crossing number for future use
  (setq last-crossing-number (+ 1 (atoi (substr crossingNumber 2))))  ;; Increment the number for the next run

  ;; Step 2: Select a Linear Dimension or MText
  (setq dispObj (entsel "\nSelect a Linear Dimension or MText: "))
  
  ;; If a valid object is selected
  (if dispObj
    (progn
      (setq dispEntity (car dispObj))
      ;; Determine object type and layer
      (setq objType (cdr (assoc 0 (entget dispEntity))))
      (setq dispLayer (cdr (assoc 8 (entget dispEntity))))

      ;; Step 3: Handle Linear Dimension
      (if (equal objType "DIMENSION")
        (progn
          (setq dispLabel (cdr (assoc 1 (entget dispEntity))))
          (princ (strcat "\nRaw Dimension Text: " dispLabel))

          (setq owner (car (my-string-split "\\P" dispLabel)))
          (setq desc (vl-string-subst "" (strcat owner "\\P") dispLabel))
          (while (or (vl-string-search "\\P" desc) (vl-string-search "\n" desc))
            (setq desc (vl-string-subst " " "\\P" desc))
            (setq desc (vl-string-subst " " "\n" desc))
          )
          (setq desc (vl-string-trim " " desc))

          (princ (strcat "\nOwner Detected: " owner))
          (princ (strcat "\nDescription Detected: " desc))
        )
        ;; Step 4: Handle MText
        (if (equal objType "MTEXT")
          (progn
            ;; Case 1: Check if it's on the 'T-WATER-T' or 'WETLAND-T' layer
            (if (or (wcmatch (strcase dispLayer) "T-WATER-T")
                    (wcmatch (strcase dispLayer) "WETLAND-T"))
              (progn
                (setq owner "-")  ;; Owner is set to '-'
                ;; Clean the MText for the description: remove everything between '{' and ';' and the final '}'
                (setq desc (clean-description (cdr (assoc 1 (entget dispEntity))) ""))
                (princ (strcat "\nOwner Set to '-': Description Detected: " desc))
              )
              ;; Case 2: Check for specified keywords
              (if (apply 'or (mapcar (function (lambda (word) (wcmatch (cdr (assoc 1 (entget dispEntity))) (strcat "*" word "*")))) keywords))
                (progn
                  (setq owner (car (vl-remove-if-not '(lambda (word) (wcmatch (cdr (assoc 1 (entget dispEntity))) (strcat "*" word "*"))) keywords)))
                  ;; Remove the keyword from the description entirely
                  (setq desc (clean-description (cdr (assoc 1 (entget dispEntity))) owner))
                  (princ (strcat "\nOwner Set to Keyword: " owner))
                  (princ (strcat "\nDescription Detected: " desc))
                )
                ;; Case 3: Default handling
                (progn
                  (setq dispLabel (cdr (assoc 1 (entget dispEntity))))
                  ;; First line is the owner
                  (setq owner (car (my-string-split "\\P" dispLabel)))
                  ;; Remove \pxqc; from the owner if it's there
                  (setq owner (vl-string-subst "" "\\pxqc;" owner))
                  ;; Remaining lines are the description
                  (setq desc (vl-string-subst "" owner dispLabel))
                  (setq desc (clean-description desc ""))
                  (princ (strcat "\nOwner Detected: " owner))
                  (princ (strcat "\nDescription Detected: " desc))
                )
              )
            )
          )
          ;; If the object type is neither DIMENSION nor MTEXT
          (princ "\nSelected object is neither a Linear Dimension nor MTEXT.")
        )
      )

      ;; Step 5: Ask to use last section or a new one
      (if last-section-label
        (progn
          (setq useLastSection (getstring (strcat "\nDo you want to use the last section label? [Y/N] <Y>: ")))
          (if (= useLastSection "")
            (setq useLastSection "Y")
          )
        )
      )

      ;; If the user selects 'Y', use the last stored section label and location
      (if (and last-section-label (wcmatch (strcase useLastSection) "Y"))
        (progn
          (setq secLabel last-section-label)
          (setq loc last-location)
          (princ (strcat "\nUsing last section label: " secLabel))
        )
        ;; Otherwise, select a new section label and calculate location
        (progn
          (setq secObj (entsel "\nSelect the Section Label (MText/Text): "))
          (if secObj
            (progn
              (setq secLabel (cdr (assoc 1 (entget (car secObj)))))
              (while (vl-string-search "\\P" secLabel)
                (setq secLabel (vl-string-subst " " "\\P" secLabel)))

              ;; Step 5.1: Remove "Theor. " and "Sec. " from the section label
              (setq loc (vl-string-subst "" "Theor. " secLabel))
              (setq loc (vl-string-subst "" "Sec. " loc))

              ;; Step 5.2: Replace "\P" with a space
              (setq loc (vl-string-subst " " "\\P" loc))

              ;; Step 5.3: Replace "-W.4M." with "-4" and "-W.5M." with "-5"
              (setq loc (vl-string-subst "-4" "-W.4M." loc))
              (setq loc (vl-string-subst "-5" "-W.5M." loc))
              (setq loc (vl-string-subst "-6" "-W.6M." loc))

              ;; Step 5.4: Remove "1/4" if present
              (setq loc (vl-string-subst "" "1/4" loc))

              ;; Step 5.5: Replace multiple spaces with single space
              (while (vl-string-search "  " loc)
                (setq loc (vl-string-subst " " "  " loc))
              )

              ;; Step 5.6: Replace multiple dashes with single dash
              (while (vl-string-search "--" loc)
                (setq loc (vl-string-subst "-" "--" loc))
              )

              ;; Step 5.7: Remove only trailing periods, not internal ones
              (if (and (> (strlen loc) 0) (equal (substr loc (strlen loc) 1) "."))
                (setq loc (substr loc 1 (- (strlen loc) 1)))
              )

              ;; Step 5.8: Final cleanup to ensure format like "S.E. 13-62-20-5"
              ;; Ensure no leading or trailing spaces
              (setq loc (vl-string-trim " " loc))

              ;; Store the current section label and location for future use
              (setq last-section-label secLabel)
              (setq last-location loc)
              (princ (strcat "\nLocation Cleaned: " loc))
            )
            (princ "\nError: No Section Label selected.")
          )
        )
      )

      ;; Step 6: Insert the block
      (setq insPt (getpoint "\nSpecify insertion point for the block: "))
      (if insPt
        (progn
          (princ "\nInserting the block...")
          (setq blockObj (vla-InsertBlock
                          (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                          (vlax-3D-point (car insPt) (cadr insPt) 0.0)  ;; Fixed the insertion point with 3 coordinates
                          blockName
                          1.0 1.0 1.0 0.0))  ;; Scale and rotation parameters set
          (setq attList (vlax-invoke blockObj 'GetAttributes))
          (princ "\nAttributes fetched.")

          ;; Set the block's attributes (Owner, Description, Location, Crossing)
          (foreach attObj attList
            (setq attTag (strcase (vla-get-tagstring attObj)))
            (cond
              ((= attTag "OWNER")
                (vlax-put-property attObj 'TextString owner)) ;; Set the Owner
              ((= attTag "DESCRIPTION")
                (vlax-put-property attObj 'TextString desc)) ;; Set the Description
              ((= attTag "LOCATION")
                (vlax-put-property attObj 'TextString loc)) ;; Set Location
              ((= attTag "CROSSING")
                (vlax-put-property attObj 'TextString crossingNumber)) ;; Set Crossing Number
              ((= attTag "DWG_REF")
                (vlax-put-property attObj 'TextString "")) ;; Leave DWG_REF blank
            )
          )
          (princ "\nBlock attributes filled successfully.")
        )
        (princ "\nError: No insertion point selected.")
      )
    )
    (princ "\nError: No valid object selected.")
  )
)

;; Exit gracefully with a success message upon loading
(princ "\nCommand CBG loaded successfully.")
(princ)
