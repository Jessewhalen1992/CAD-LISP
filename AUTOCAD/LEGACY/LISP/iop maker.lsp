(vl-load-com)  ;; Ensure VLA functions are available

(defun digitp (c)
  ;; Helper function to check if a character is a digit
  (and (>= (ascii c) 48) (<= (ascii c) 57)))  ;; ASCII codes for '0' to '9'

(defun replace-p (str)
  ;; Replace "\P" with "-" if preceded by a digit, else remove "\P"
  (setq i 1)
  (setq len (strlen str))
  (setq newstr "")
  (while (<= i len)
    (if (and (< i len) (= (substr str i 2) "\\P"))
      (progn
        ;; Check if the preceding character is a digit
        (setq prev (if (> i 1) (substr str (- i 1) 1) ""))
        (if (and (> (strlen prev) 0) (digitp prev))
          (progn
            ;; Replace "\P" with "-"
            (setq newstr (strcat newstr "-"))
            (setq i (+ i 2))  ;; Skip past "\P"
          )
          (progn
            ;; Remove "\P" by not appending anything
            (setq i (+ i 2))  ;; Skip past "\P"
          )
        )
      )
      (progn
        ;; Add current character to new string
        (setq newstr (strcat newstr (substr str i 1)))
        (setq i (1+ i))
      )
    )
  )
  newstr)

(defun remove-theor-prefix (str)
  ;; Remove "Theor. " prefix if it exists at the beginning of the string
  (if (and (>= (strlen str) 7) (equal (substr str 1 7) "Theor. "))
    (substr str 8)  ;; Return the string without the first 7 characters
    str)            ;; Return the original string if prefix doesn't exist
)

(defun parse-section-label (secLabel)
  ;; This function parses the section label to the desired format
  (setq loc secLabel)

  ;; Step 0: Remove "Theor. " prefix if present
  (setq loc (remove-theor-prefix loc))

  ;; Step 1: Replace "\P" appropriately
  (setq loc (replace-p loc))

  ;; Step 2: Final cleanup to ensure no extra spaces or trailing periods
  ;; Remove multiple spaces
  (while (vl-string-search "  " loc)
    (setq loc (vl-string-subst " " "  " loc))
  )

  ;; Remove trailing period
  (if (and (> (strlen loc) 0) (equal (substr loc (strlen loc) 1) "."))
    (setq loc (substr loc 1 (- (strlen loc) 1)))
  )

  ;; Trim any leading/trailing spaces
  (setq loc (vl-string-trim " " loc))

  loc)  ;; Return the parsed location

(defun get-attribute (block tag)
  ;; This function retrieves the full value of a specified attribute in a block
  (setq blk (vlax-ename->vla-object block))  ;; Convert entity name to VLA object
  (setq attrs (vlax-invoke blk 'GetAttributes))  ;; Get all attributes in the block
  (setq result nil)
  (foreach att attrs
    (setq attTag (vla-get-tagstring att))
    (setq attValue (vla-get-textstring att))
    (if (equal (strcase attTag) (strcase tag))
      (setq result attValue)
    )
  )
  result)  ;; Return the attribute value or nil if not found

(defun update-text-in-layout (old-text new-text)
  ;; This function updates all instances of old-text with new-text in the current layout
  (setq ss (ssget "X" (list (cons 1 old-text))))
  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq ent-data (entget ent))
        (setq ent-type (cdr (assoc 0 ent-data)))
        (cond
          ((= ent-type "TEXT")
            ;; For TEXT entities, replace \P with space (since TEXT cannot handle line breaks)
            (setq new-text-for-text (vl-string-subst " " "\\P" new-text))
            ;; Update TEXT entity
            (setq ent-data (subst (cons 1 new-text-for-text) (assoc 1 ent-data) ent-data))
            (entmod ent-data)
          )
          ((= ent-type "MTEXT")
            ;; Update MTEXT entity
            (setq mtextObj (vlax-ename->vla-object ent))
            ;; Set the text content, including \P sequences for line breaks
            (vla-put-TextString mtextObj new-text)
          )
        )
        (setq i (1+ i))
      )
    )
  )
)

(defun delete-text-in-layout (text)
  ;; This function deletes all instances of specified text in the current layout
  (setq ss (ssget "X" (list (cons 1 text))))
  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (entdel ent)
        (setq i (1+ i))
      )
    )
  )
)

(defun get-highest-layout-number ()
  ;; This function retrieves the highest IOP layout number currently in use
  (setq highest 0)
  (setq layout-list (layoutlist))
  (foreach layout layout-list
    (if (wcmatch (strcase layout) "IOP #*")
      (progn
        (setq num (atoi (substr layout 6)))  ;; Extract number after "IOP #"
        (if (> num highest)
          (setq highest num))
      )
    )
  )
  highest)

(defun layout-exists (name)
  ;; This function checks if a layout with the given name already exists
  (if (member (strcase name) (mapcar 'strcase (layoutlist))) T nil))

(defun c:CreateIOP ()
  ;; Main function to create a new IOP layout

  ;; Step 1: Select Section Label
  (setq sectionLabel (car (entsel "\nSelect Section Label: ")))
  (if sectionLabel
    (setq sectionLabelText (cdr (assoc 1 (entget sectionLabel))))
    (progn
      (princ "\nError: Section Label not selected correctly.")
      (princ)  ;; Gracefully exit the function
      (exit)    ;; Exit the function without errors
    )
  )
  (princ (strcat "\nSection Label: " sectionLabelText))

  ;; Step 2: Select Land Owner Block
  (setq ownerBlock (car (entsel "\nSelect Land Owner Block: ")))
  (if ownerBlock
    (progn
      ;; Attempt to retrieve the ENTER_TEXT attribute
      (setq ownerName (get-attribute ownerBlock "ENTER_TEXT"))  ;; Correct tag

      ;; Debugging: Print retrieved attribute
      (princ (strcat "\nRetrieved Owner Name: " (if ownerName ownerName "nil")))

      ;; Check if attribute was retrieved correctly
      (if ownerName
        (progn
          (princ "\nLand Owner attribute retrieved successfully.")
          ;; Remove multiple spaces
          (while (vl-string-search "  " ownerName)
            (setq ownerName (vl-string-subst " " "  " ownerName))
          )
          ;; Trim any extra spaces
          (setq ownerName (vl-string-trim " " ownerName))
          (princ (strcat "\nProcessed Owner Name: " ownerName))
        )
        (progn
          (princ "\nError: Land Owner attribute not retrieved correctly.")
          (princ)  ;; Gracefully exit the function
          (exit)    ;; Exit the function without errors
        )
      )
    )
    (progn
      (princ "\nError: Land Owner Block not selected correctly.")
      (princ)  ;; Gracefully exit the function
      (exit)    ;; Exit the function without errors
    )
  )

  ;; Step 3: Select Permanent Area Labels
  (setq areaLabels '())
  (prompt "\nSelect Permanent Area Labels (Press Enter to finish):")
  (while (setq areaLabel (car (entsel "\nSelect Area Label (or press Enter to finish): ")))
    (setq areaLabelText (cdr (assoc 1 (entget areaLabel))))
    (setq areaLabels (append areaLabels (list areaLabelText)))
  )

  (if (null areaLabels)
    (progn
      (princ "\nError: No Permanent Area Labels selected.")
      (princ)  ;; Gracefully exit the function
      (exit)    ;; Exit the function without errors
    )
    (princ (strcat "\nPermanent Area Labels selected: " (itoa (length areaLabels))))
  )

  ;; Step 4: Create or Use Existing "IOP" Layout
  (if (layout-exists "IOP")
    (progn
      (princ "\nIOP layout already exists. Using the existing layout.")
      (command "_.layout" "set" "IOP")
    )
    (progn
      (princ "\nCreating new IOP layout from template.")
      (command "_.layout" "template" "M:\\Drafting\\_CURRENT TEMPLATES\\Compass_Main.dwt" "IOP")
      ;; Pause to ensure layout creation completes
      (command "_.regen")
      ;; Switch to the newly created layout
      (command "_.layout" "set" "IOP")
    )
  )

  ;; Step 5: Automatically update text in new layout
  ;; Parse the section label to handle different formats
  (setq parsedSectionLabel (parse-section-label sectionLabelText))
  (update-text-in-layout "LOCATION" parsedSectionLabel)
  (update-text-in-layout "LANDOWNER" ownerName)

  ;; Update AREA1 to AREA4 based on selected areaLabels
  (setq max-areas 4)
  (setq i 1)  ;; Start counter at 1
  (foreach area areaLabels
    (setq tag (strcat "AREA" (itoa i)))
    (update-text-in-layout tag area)
    (setq i (1+ i))
  )

  ;; Delete any remaining AREA tags if necessary
  (while (<= i max-areas)
    (setq tag (strcat "AREA" (itoa i)))
    (delete-text-in-layout tag)
    (setq i (1+ i))
  )

  ;; Step 6: Rename the layout
  (setq highest-layout-number (get-highest-layout-number))
  (setq new-layout-number (1+ highest-layout-number))
  (setq new-layout-name (strcat "IOP #" (itoa new-layout-number)))

  ;; Ensure the new layout name is unique
  (while (layout-exists new-layout-name)
    (setq new-layout-number (1+ new-layout-number))
    (setq new-layout-name (strcat "IOP #" (itoa new-layout-number)))
  )

  (command "_.layout" "rename" "IOP" new-layout-name)

  (princ (strcat "\n" new-layout-name " creation and text update completed successfully."))
  (princ)
)

(defun c:ListBlockAttributes ( / blockObj attrs att)
  ;; This function lists all attributes and their values in a selected block
  (vl-load-com)  ;; Ensure VLA functions are available

  ;; Prompt user to select a block
  (setq blk (car (entsel "\nSelect a Block Reference: ")))

  (if blk
    (progn
      (setq blockObj (vlax-ename->vla-object blk))  ;; Convert entity name to VLA object
      (setq attrs (vlax-invoke blockObj 'GetAttributes))  ;; Get all attributes in the block

      (princ "\nAttributes in the selected block:")

      (foreach att attrs
        (setq attTag (vla-get-tagstring att))
        (setq attValue (vla-get-textstring att))
        (princ (strcat "\nTag: " attTag " | Value: " attValue))
      )
    )
    (princ "\nError: No block selected.")
  )
  (princ)
)

(princ "\nLISP routine loaded. Type CREATEIOP to run.")
(princ)
