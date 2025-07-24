;;; crown.LSP ver 1.0 (Final Revision with Layer 40 Protection and P-PROPOSED Keep)
;;; Command: CROWN
;;;
;;; Original by Jimmy Bergmark, 2013
;;; Modified by Jesse Whalen, 2025 (Assisted by AI)
;;;
;;; This routine:
;;;   1. Ensures layer "40" exists with color Yellow.
;;;   2. Prompts user to select objects on layer "P-PROPOSED" to keep;
;;;      deletes all other objects on that layer.
;;;   3. Merges various old layers into target layers (including merging
;;;      "P-PROPOSED" into "28").
;;;   4. Deletes all layouts except MODEL and detaches images.
;;;   5. Runs multiple purge passes, keeping only layers "28", "35", and "40".
;;;   6. Erases objects on layers 9, 10, and 17 and force-deletes these layers.
;;;   7. Erases objects on any layer not in the allowed list.
;;;   8. Force-deletes any leftover layers (except "0" and "DEFPOINTS").
;;;   9. Final purge.
;;;  10. Explicitly erases objects on layer "0" and "DEFPOINTS".
;;;  11. Ensures layer "40" exists (with color Yellow).
;;;  12. Prompts for disposition label text, which will be placed on layer 40.
;;;
;;; Requires Express Tools for LAYDEL.
;;; Use on a backup drawing.
;;;
;;; ------------------------------------------------------------------------------

(vl-load-com)

;; --- Utility: Convert selection set to list
(defun ss->list (ss / lst i)
  (setq lst '() i 0)
  (if ss
    (repeat (sslength ss)
      (setq lst (cons (ssname ss i) lst))
      (setq i (1+ i))
    )
  )
  lst
)

;; --- Utility: Subtract lst2 from lst1
(defun list-subtract (lst1 lst2 / result)
  (setq result '())
  (foreach x lst1
    (if (not (member x lst2))
      (setq result (cons x result))
    )
  )
  result
)

(defun c:crown (/ doc allowedLayers str prevstr ins texto InsertionPoint
                  ssAll ssKeep lstAll lstKeep lstDelete
                  Layout ss lname)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ============================================================
  ;; 0) Ensure layer "40" exists and is set to Yellow (color index 2).
  ;;     (We use the -LAYER Make command with a Color option.)
  ;; ============================================================
  (if (not (tblsearch "layer" "40"))
    (command "._-LAYER" "M" "40" "Color" "2" "40" "")
  )
  ;; Also, if layer 40 exists, set its color to Yellow.
  (command "._-LAYER" "S" "40" "")
  (command "._-LAYER" "C" "2" "40" "")

  ;; ============================================================
  ;; 1) On layer "P-PROPOSED": let the user select objects to KEEP,
  ;;    then delete all others on that layer.
  ;; ============================================================
  (if (tblsearch "layer" "P-PROPOSED")
    (progn
      (prompt "\nSelect object(s) on layer 'P-PROPOSED' to KEEP; objects NOT selected will be deleted: ")
      (setq ssKeep (ssget))  ;; user selection
      (setq ssAll (ssget "X" '((8 . "P-PROPOSED"))))
      (if ssAll
        (progn
          (setq lstAll (ss->list ssAll))
          (setq lstKeep (if ssKeep (ss->list ssKeep) '()))
          (setq lstDelete (list-subtract lstAll lstKeep))
          (foreach e lstDelete (entdel e))
        )
      )
    )
  )

  ;; ============================================================
  ;; 2) Merge layers into target layers.
  ;;     Merge "P-PROPOSED" into "28", and merge other layers into "9" or "10".
  ;; ============================================================
  (defun layermerge (oldlay newlay)
    (if (tblsearch "layer" oldlay)
      (progn
        (command "._-layer" "_u" oldlay "")
        (if (tblsearch "layer" newlay)
          (progn
            (command "._-layer" "_u" newlay "")
            (command "._laymrg" "_n" oldlay "" "_n" newlay)
            (if (wcmatch (getvar "cmdnames") "*LAYMRG*")
              (command "_yes")
            )
          )
          (command "._rename" "_la" oldlay newlay)
        )
      )
    )
  )

  (layermerge "P-PROPOSED" "28")
  (layermerge "GEO-RPOINTS" "35")

  (layermerge "C-AR" "9")
  (layermerge "C-FLOWLINE" "9")
  (layermerge "C-RW" "9")
  (layermerge "C-RW-DASHED" "9")
  (layermerge "C-WS" "9")
  (layermerge "C-EZE" "9")
  (layermerge "C-VCE" "9")
  (layermerge "F-AR" "9")
  (layermerge "F-EZE" "9")
  (layermerge "F-WS" "9")
  (layermerge "F-EZE_VCE" "9")
  (layermerge "F-FLOWLINE" "9")
  (layermerge "F-VCE" "9")
  (layermerge "F-RW" "9")
  (layermerge "F-RW-DASHED" "9")
  (layermerge "L-TITLEDB" "9")
  (layermerge "F-WELLS" "9")
  (layermerge "C-WELLS" "9")
  (layermerge "F-WELLS_AR" "9")
  (layermerge "F-FLOWLINE_EZE" "9")

  (layermerge "C-AR-T" "10")
  (layermerge "F-WELLS_AR_TXT" "10")
  (layermerge "C-AR_T" "10")
  (layermerge "C-FLOWLINE-T" "10")
  (layermerge "C-FLOWLINE_EZE-TXT" "10")
  (layermerge "C-RW-T" "10")
  (layermerge "C-RW-TXT" "10")
  (layermerge "C-WS-T" "10")
  (layermerge "C-EZE-T" "10")
  (layermerge "C-VCE-T" "10")
  (layermerge "F-AR-T" "10")
  (layermerge "F-EZE-T" "10")
  (layermerge "F-WS-T" "10")
  (layermerge "F-EZE_VCE-T" "10")
  (layermerge "F-FLOWLINE-T" "10")
  (layermerge "F-VCE-T" "10")
  (layermerge "F-RW-T" "10")
  (layermerge "F-RW-TXT" "10")
  (layermerge "L-TITLEDB-T" "10")
  (layermerge "C-WELLS_TXT" "10")
  (layermerge "F-WELLS_TXT" "10")

  ;; ============================================================
  ;; 3) Delete all layouts except MODEL.
  ;; ============================================================
  (vlax-for Layout (vla-get-Layouts doc)
    (if (/= (strcase (vla-get-name Layout)) "MODEL")
      (vla-delete Layout)
    )
  )

  ;; ============================================================
  ;; 4) Detach all images.
  ;; ============================================================
  (command "-IMAGE" "D")
  (if (wcmatch (getvar "cmdnames") "*IMAGE*")
    (command "*")
  )

  ;; ============================================================
  ;; 5) Repeatedly purge unused items,
  ;;     keeping only allowed layers: "28", "35", and "40".
  ;; (We want to delete objects on layers "0" and "DEFPOINTS" later.)
  ;; ============================================================
  (defun ax:purge-layer (doc name)
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-delete
            (list (vl-catch-all-apply 'vla-item (list (vla-get-layers doc) name)))))
      nil
      T
    )
  )
  (defun ax:purge-layers (doc except)
    (vlax-for L (vla-get-layers doc)
      (if (not (member (strcase (vla-get-name L)) except))
        (ax:purge-layer doc (vla-get-name L))
      )
    )
  )
  (setq allowedLayers '("28" "35" "40"))
  (repeat 4
    (command "-PURGE" "Blocks" "*" "N")
    (command "-PURGE" "Dimstyles" "*" "N")
    (command "-PURGE" "LTypes" "*" "N")
    (command "-PURGE" "MAterials" "*" "N")
    (command "-PURGE" "MUltileaderstyles" "*" "N")
    (command "-PURGE" "Plotstyles" "*" "N")
    (command "-PURGE" "SHapes" "*" "N")
    (command "-PURGE" "textSTyles" "*" "N")
    (command "-PURGE" "Mlinestyles" "*" "N")
    (command "-PURGE" "Tablestyles" "*" "N")
    (command "-PURGE" "Visualstyles" "*" "N")
    (command "-PURGE" "Regapps" "*" "N")
    (command "-PURGE" "Zero")
    (command "-PURGE" "Empty")
    (ax:purge-layers doc allowedLayers)
  )

  ;; ============================================================
  ;; 6) Erase objects on layers 9, 10, and 17; then force-delete those layers.
  ;; ============================================================
  (setq ss (ssget "X" '((8 . "9"))))   (if ss (command "._ERASE" ss ""))
  (setq ss (ssget "X" '((8 . "10"))))  (if ss (command "._ERASE" ss ""))
  (setq ss (ssget "X" '((8 . "17"))))  (if ss (command "._ERASE" ss ""))
  (setvar "CLAYER" "0")
  (if (tblsearch "layer" "9")
    (progn
      (command "._LAYDEL" "_Name" "9" "")
      (if (wcmatch (getvar "cmdnames") "*LAYDEL*")
        (command "_Yes")
      )
    )
  )
  (if (tblsearch "layer" "10")
    (progn
      (command "._LAYDEL" "_Name" "10" "")
      (if (wcmatch (getvar "cmdnames") "*LAYDEL*")
        (command "_Yes")
      )
    )
  )
  (if (tblsearch "layer" "17")
    (progn
      (command "._LAYDEL" "_Name" "17" "")
      (if (wcmatch (getvar "cmdnames") "*LAYDEL*")
        (command "_Yes")
      )
    )
  )

  ;; ============================================================
  ;; 7) Erase objects on any layer not in allowed list.
  ;; Allowed layers: "28", "35", "40"
  ;; ============================================================
  (setq allowedLayers '("28" "35" "40"))
  (vlax-for L (vla-get-layers doc)
    (if (not (member (strcase (vla-get-name L)) allowedLayers))
      (progn
        (setq ss (ssget "X" (list (cons 8 (vla-get-name L)))))
        (if ss (command "._ERASE" ss ""))
      )
    )
  )

  ;; ============================================================
  ;; 8) Force-delete leftover layers not in allowed list.
  ;; Do not delete layers "0" and "DEFPOINTS" (they are mandatory).
  ;; ============================================================
  (vlax-for L (vla-get-layers doc)
    (setq lname (strcase (vla-get-name L)))
    (if (and (not (member lname '("0" "DEFPOINTS" "28" "35" "40")))
             (tblsearch "layer" lname))
      (progn
        (command "._LAYDEL" "_Name" lname "")
        (if (wcmatch (getvar "cmdnames") "*LAYDEL*")
          (command "_Yes")
        )
      )
    )
  )

  ;; ============================================================
  ;; 9) Final purge pass
  ;; ============================================================
  (command "-PURGE" "All" "*" "N")
  (command "-PURGE" "Zero")
  (command "-PURGE" "Empty")

  ;; ============================================================
  ;; 10) Erase all objects on layers "0" and "DEFPOINTS"
  ;; ============================================================
  (setq ss (ssget "X" '((8 . "0"))))   (if ss (command "._ERASE" ss ""))
  (setq ss (ssget "X" '((8 . "DEFPOINTS"))))   (if ss (command "._ERASE" ss ""))

  ;; ============================================================
  ;; 11) Ensure layer "40" exists with color Yellow (2).
  ;; ============================================================
  (if (not (tblsearch "layer" "40"))
    (command "._-LAYER" "M" "40" "Color" "2" "40" "")
  )
  (command "._-LAYER" "S" "40" "")
  (command "._-LAYER" "C" "2" "40" "")

  ;; ============================================================
  ;; 12) Prompt user for disposition text on layer 40.
  ;; ============================================================
  (setq str "")
  (while T
    (setq prevstr str)
    (setq str (getstring (strcat "\nAdd disposition label <" str ">: ")))
    (if (= str "") (setq str prevstr))
    (if (/= str "")
      (progn
        (setq ins (getpoint "Specify start point of text: "))
        (if (not ins) (return))
        (setq InsertionPoint (vlax-3D-Point ins))
        (setq texto (vla-addMText (vla-get-modelspace doc) InsertionPoint 0.0 str))
        (vla-put-layer texto "40")
        (vla-put-AttachmentPoint texto acAttachmentPointMiddleCenter)
        (vla-put-height texto 4.0)
        (vla-put-InsertionPoint texto InsertionPoint)
      )
    )
  )
  (princ)
)
(princ)
