(defun c:CleanDwg ()
  ;; Prompt user to remove layouts
  (if (eq (strcase (getstring "\nDo you want to delete all layouts? (Y/N): ")) "Y")
    (progn
      ;; Delete all layouts except Model space
      (vlax-for layout (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object)))
        (if (not (equal (vla-get-Name layout) "Model"))
          (vla-delete layout)
        )
      )
      (princ "\nAll layouts deleted.")
    )
  )

  ;; Turn on and thaw all layers
  (command "_.LAYER" "T" "*" "")
  (command "_.LAYER" "ON" "*" "")

  ;; Delete the block "NEZD Point"
  (setq blockName "NEZD Point")
  (if (tblsearch "BLOCK" blockName)
    (progn
      (command "_.-PURGE" "B" blockName "N")
      (command "_.ERASE" (ssget "_X" (list (cons 2 blockName) (cons 0 "INSERT"))) "")
      (princ (strcat "\nBlock " blockName " deleted."))
    )
    (princ (strcat "\nBlock " blockName " not found."))
  )

  ;; Delete all Data Links
  (vl-load-com)
  (setq *acad* (vlax-get-acad-object))
  (setq *doc* (vla-get-ActiveDocument *acad*))
  ;; Get the Named Object Dictionary (NOD)
  (setq nod (vla-get-Dictionaries *doc*))
  ;; Attempt to get the DataLink dictionary within the NOD
  (setq datalinkDict (vl-catch-all-apply 'vla-Item (list nod "ACAD_DATALINK")))
  ;; Check if the DataLinkDictionary exists
  (if (and datalinkDict (not (vl-catch-all-error-p datalinkDict)))
    (progn
      ;; Iterate through the DataLinkDictionary and delete each entry
      (vlax-for item datalinkDict
        (vla-delete item)
      )
      (princ "\nAll DataLinks have been deleted.")
    )
    (princ "\nNo DataLinks found.")
  )

  ;; Delete all objects on the "Parcel Mapping" layer
  (if (tblsearch "LAYER" "Parcel Mapping")
    (progn
      (command "_.ERASE" (ssget "_X" (list (cons 8 "Parcel Mapping"))) "")
      (princ "\nAll objects on the 'Parcel Mapping' layer deleted.")
    )
  )

  ;; Define the layers to delete
  (setq layers-to-delete
        '("Z-description"
          "Z-Elevation"
          "Z-Pointnumber"
          "Z-Mspoint"
          "Z-DRILL-POINT"
          "P--DRILLOFFSET"
          "L-SEC-HB"))

  ;; Delete all entities on the specified layers
  (foreach lay layers-to-delete
    (if (tblsearch "LAYER" lay)
      (command "_.ERASE" (ssget "X" (list (cons 8 lay))) "")
    )
  )

  ;; Delete all images
  (command "_.ERASE" (ssget "_X" '((0 . "IMAGE"))) "")

  ;; Purge the drawing more efficiently
  (repeat 3
    (command "_.PURGE" "A" "" "N")
  )

  ;; Delete everything on the Defpoints layer
  (if (tblsearch "LAYER" "Defpoints")
    (progn
      (command "_.ERASE" (ssget "_X" (list (cons 8 "Defpoints"))) "")
      (princ "\nAll objects on the 'Defpoints' layer deleted.")
    )
  )

  ;; Zoom extents
  (command "_.ZOOM" "E")

  ;; Insert the block "Disclaimer_Legal" from the specified path with scale 1 and 360-degree rotation
  (setq blockPath "C:/AUTOCAD-SETUP/BLOCKS/_CG BLOCKS/Disclaimer_Legal.dwg")
  (if (findfile blockPath)
    (progn
      (setq insPt (getpoint "\nSpecify insertion point: "))
      (command "_.INSERT" blockPath insPt 1 1 360)
      (princ "\nDisclaimer_Legal block inserted from file at scale 1 with 360-degree rotation.")
    )
    (princ "\nBlock file not found at the specified path.")
  )

  ;; End message
  (princ "\nDrawing cleaned and purged.")
  (princ)
)
