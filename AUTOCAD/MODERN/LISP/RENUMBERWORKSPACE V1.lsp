(defun c:RNW ( / pline ss ssFiltered blkList sortedBlks cnt mapping)
  (vl-load-com)

  ;; Step 1: Select the polyline
  (setq pline (car (entsel "\nSelect the polyline: ")))
  (if (and pline (member (cdr (assoc 0 (entget pline))) '("LWPOLYLINE" "POLYLINE")))
    (progn
      ;; Get the VLA object of the polyline
      (setq plineObj (vlax-ename->vla-object pline))

      ;; Step 2: Select all blocks and filter by specific names in the code
      (princ "\nSelect the blocks to include (only selected blocks will be processed): ")
      ;; Select all INSERT entities
      (setq ss (ssget '((0 . "INSERT"))))

      ;; Filter selection to include only blocks with specified names
      (if ss
        (progn
          (setq ssFiltered (ssadd))
          (repeat (setq i (sslength ss))
            (setq ent (ssname ss (setq i (1- i))))
            (setq entData (entget ent))
            ;; Add to filtered selection if the block name matches the specified ones
            (if (member (cdr (assoc 2 entData)) '("Temporary Area" "Temp_Area5" "Temp_Area4" "Temp_Area3" "Temp_Area_Small_Pink"))
              (ssadd ent ssFiltered)
            )
          )

          ;; Check if any specified blocks were selected
          (if (> (sslength ssFiltered) 0)
            (progn
              ;; Step 3: Calculate the sequence order along the polyline
              (setq blkList '())
              (repeat (setq i (sslength ssFiltered))
                (setq ent (ssname ssFiltered (setq i (1- i))))
                (setq blkObj (vlax-ename->vla-object ent))
                ;; Find the closest point on the polyline
                (setq blkPt (vlax-get blkObj 'InsertionPoint))
                (setq closestPt (vlax-curve-getClosestPointTo plineObj blkPt))
                ;; Get the parameter at that point
                (setq param (vlax-curve-getParamAtPoint plineObj closestPt))
                ;; Store in a list: (blockObj . param)
                (setq blkList (cons (cons blkObj param) blkList))
              )
              ;; Sort the blocks based on the parameter
              (setq sortedBlks (vl-sort blkList (function (lambda (a b) (< (cdr a) (cdr b))))))

              ;; Initialize mapping list
              (setq mapping '())

              ;; Step 4: Renumber the blocks using W# format and store the mappings
              (setq cnt 1)
              (foreach blkParamPair sortedBlks
                (setq blkObj (car blkParamPair))
                ;; Get the attributes
                (setq attCol (vlax-invoke blkObj 'GetAttributes))
                (foreach att attCol
                  ;; Look for the "TEMPORARY" attribute in these blocks
                  (if (= (strcase (vla-get-TagString att)) "TEMPORARY")
                    (progn
                      (setq origValue (vla-get-TextString att))
                      (setq newValue (strcat "W" (itoa cnt)))
                      ;; Update the attribute
                      (vla-put-TextString att newValue)
                      ;; Store the mapping of old and new values
                      (setq mapping (cons (cons origValue newValue) mapping))
                    )
                  )
                )
                (setq cnt (1+ cnt))
              )

              ;; Step 5: Look for any blocks with matching old values and update them
              (setq allBlocks (ssget "X" '((0 . "INSERT")))) ; Get all blocks
              (if allBlocks
                (progn
                  (repeat (setq i (sslength allBlocks))
                    (setq ent (ssname allBlocks (setq i (1- i))))
                    (setq blkObj (vlax-ename->vla-object ent))
                    ;; Get the attributes
                    (setq attCol (vlax-invoke blkObj 'GetAttributes))
                    (foreach att attCol
                      ;; Check for attributes matching old values (both TEMPORARY and TEMP_AREA_W1)
                      (if (or (= (strcase (vla-get-TagString att)) "TEMPORARY") (= (strcase (vla-get-TagString att)) "TEMP_AREA_W1"))
                        (progn
                          (setq attValue (vla-get-TextString att))
                          ;; Check if the attribute value matches any original value in the mapping
                          (setq mapPair (assoc attValue mapping))
                          (if mapPair
                            ;; Only update if the block is not one of the originally renumbered ones
                            (if (not (vl-position blkObj (mapcar 'car sortedBlks)))
                              ;; Update the attribute to the new value
                              (vla-put-TextString att (cdr mapPair))
                            )
                          )
                        )
                      )
                    )
                  )
                  (princ "\nAll matching attributes updated successfully.")
                )
                (princ "\nNo matching blocks found.")
              )
            )
            (princ "\nNo specified blocks were selected.")
          )
        )
        (princ "\nNo blocks selected.")
      )
    )
    (princ "\nSelected entity is not a polyline.")
  )
  (princ)
)
