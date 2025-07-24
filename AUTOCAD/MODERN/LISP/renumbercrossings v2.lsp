(defun c:RNC ( / pline ss ssFiltered blkList sortedBlks cnt mapping)
  (vl-load-com)

  ;; Step 1: Select the polyline
  (setq pline (car (entsel "\nSelect the polyline: ")))
  (if (and pline (member (cdr (assoc 0 (entget pline))) '("LWPOLYLINE" "POLYLINE")))
    (progn
      ;; Get the VLA object of the polyline
      (setq plineObj (vlax-ename->vla-object pline))

      ;; Step 2: Select blocks to include
      (princ "\nSelect the blocks to include (only 'xing2' blocks will be processed): ")
      (setq ss (ssget '((0 . "INSERT")))) ; Allow user to select blocks

      ;; Filter the selection set to only include blocks named "xing2"
      (if ss
        (progn
          (setq ssFiltered (ssadd))
          (repeat (setq i (sslength ss))
            (setq ent (ssname ss (setq i (1- i))))
            (setq entData (entget ent))
            (if (= (cdr (assoc 2 entData)) "xing2")
              (ssadd ent ssFiltered)
            )
          )
          ;; Check if any "xing2" blocks were selected
          (if (> (sslength ssFiltered) 0)
            (progn
              ;; Step 3: Calculate the sequence order
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

              ;; Initialize the mapping list
              (setq mapping '())

              ;; Step 4: Update the 'CROSSING' attribute and store mappings
              (setq cnt 1)
              (foreach blkParamPair sortedBlks
                (setq blkObj (car blkParamPair))
                ;; Get the attributes
                (setq attCol (vlax-invoke blkObj 'GetAttributes))
                (foreach att attCol
                  (if (= (strcase (vla-get-TagString att)) "CROSSING")
                    (progn
                      (setq origValue (vla-get-TextString att))
                      (setq newValue (strcat "X" (itoa cnt)))
                      ;; Update the attribute
                      (vla-put-TextString att newValue)
                      ;; Store the mapping if not already stored
                      (if (not (assoc origValue mapping))
                        (setq mapping (cons (cons origValue newValue) mapping))
                      )
                    )
                  )
                )
                (setq cnt (1+ cnt))
              )
              ;; Step 5: Update other blocks with matching 'CROSSING' values
              ;; Get all blocks with 'CROSSING' attributes
              (setq allBlocks (ssget "X" '((0 . "INSERT"))))
              (if allBlocks
                (progn
                  (repeat (setq i (sslength allBlocks))
                    (setq ent (ssname allBlocks (setq i (1- i))))
                    (setq blkObj (vlax-ename->vla-object ent))
                    ;; Skip the blocks we already processed
                    (if (not (vl-position blkObj (mapcar 'car blkList)))
                      (progn
                        ;; Get the attributes
                        (setq attCol (vlax-invoke blkObj 'GetAttributes))
                        (foreach att attCol
                          (if (= (strcase (vla-get-TagString att)) "CROSSING")
                            (progn
                              (setq attValue (vla-get-TextString att))
                              ;; Check if the attribute value matches any original value
                              (setq mapPair (assoc attValue mapping))
                              (if mapPair
                                (vla-put-TextString att (cdr mapPair))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (princ "\nAll matching attributes updated successfully.")
                )
                (princ "\nNo other blocks with 'CROSSING' attributes found.")
              )
            )
            (princ "\nNo blocks named 'xing2' were selected.")
          )
        )
        (princ "\nNo blocks selected.")
      )
    )
    (princ "\nSelected entity is not a polyline.")
  )
  (princ)
)
