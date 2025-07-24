(defun c:RenameLayouts (/ numLayouts prefix option i layoutNames layoutObjs layoutName layoutObj newLayoutName ly)

  ;; Function to get the list of layout names in the correct tab order, excluding "Model"
  (defun getLayoutNames (/ layoutList)
    (setq layoutList '())
    (vlax-for layout (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object)))
      (if (/= (vla-get-Name layout) "Model")
        (setq layoutList (append layoutList (list (vla-get-Name layout))))
      )
    )
    layoutList
  )

  ;; Function to get the layout objects in the correct tab order, excluding "Model"
  (defun getLayoutObjs (layoutNames / layouts)
    (setq layouts '())
    (foreach name layoutNames
      (setq layoutObj (vla-item (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object))) name))
      (setq layouts (append layouts (list layoutObj)))
    )
    layouts
  )

  ;; Function to get the layout names sorted by TabOrder
  (defun getSortedLayoutNames (/ layoutList layoutObj sortedLayoutList)
    (setq layoutList (getLayoutNames))
    (setq sortedLayoutList '())
    (setq ly (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object))))
    (foreach layoutName layoutList
      (setq layoutObj (vla-item ly layoutName))
      (setq sortedLayoutList
            (append sortedLayoutList
                    (list (list (vla-get-TabOrder layoutObj) layoutName))))
    )
    (setq sortedLayoutList (vl-sort sortedLayoutList '(lambda (a b) (< (car a) (car b)))))
    (mapcar 'cadr sortedLayoutList)
  )

  ;; Function to activate a layout by name
  (defun activateLayoutByName (layoutName)
    (setvar "CTAB" layoutName)
  )

  ;; Ask the user for the prefix option they want to use
  (initget "XING IOP Number CNRLXING")
  (setq option (getkword "\nEnter the prefix option [XING/IOP/Number/CNRLXING]: "))

  ;; Set the prefix based on the user's choice
  (cond
    ((= option "XING") (setq prefix "XING #"))
    ((= option "IOP") (setq prefix "IOP #"))
    ((= option "Number") (setq prefix "Number"))
    ((= option "CNRLXING") (setq prefix "XING DWG #"))
    (t (prompt "\nInvalid option. Defaulting to Number.") (setq prefix "Number"))
  )

  ;; Ask the user for the number of layouts they want to rename
  (setq numLayouts (getint "\nEnter the number of layouts to rename: "))

  ;; Get a list of all layout names sorted by TabOrder, excluding "Model"
  (setq layoutNames (getSortedLayoutNames))

  ;; Ensure we have the correct number of layouts to rename
  (if (> numLayouts (length layoutNames))
    (progn
      (setq numLayouts (length layoutNames))
      (prompt "\nNumber of layouts to rename exceeds available layouts. Adjusting to rename all available layouts.")
    )
  )

  ;; Get layout objects in the correct order
  (setq layoutObjs (getLayoutObjs layoutNames))

  ;; Loop through the specified number of layouts and rename them
  (setq i 1)
  (foreach layoutObj layoutObjs
    (if (<= i numLayouts)
      (progn
        (if (= prefix "Number")
          (setq newLayoutName (strcat (itoa i) " of " (itoa numLayouts)))
          (setq newLayoutName (strcat prefix (itoa i)))
        )
        ;; Activate the layout before renaming
        (activateLayoutByName (vla-get-Name layoutObj))
        ;; Use VL commands to rename the layout
        (vla-put-Name layoutObj newLayoutName)
        (setq i (1+ i))
      )
    )
  )

  (princ)
)
