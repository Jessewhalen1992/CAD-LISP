(vl-load-com)

(defun c:SetRev (/ blocks-to-update new-visibility block dyn-props prop)

  ;; Select all block instances
  (setq blocks-to-update (ssget "_X" '((0 . "INSERT")))) 

  ;; Prompt user for the revision number to apply as visibility state
  (setq new-visibility (getint "\nEnter the revision number (1-7) for visibility state: "))

  ;; Ensure valid input
  (if (and blocks-to-update (>= new-visibility 1) (<= new-visibility 7))
    (progn
      ;; Loop through each block and set the visibility property if available
      (foreach block (mapcar 'vlax-ename->vla-object (vl-remove-if 'null (mapcar 'cadr (ssnamex blocks-to-update))))
        (princ (strcat "\nProcessing block: " (vla-get-Name block)))
        
        ;; Get dynamic properties of the block
        (setq dyn-props (vlax-invoke block 'GetDynamicBlockProperties))
        
        ;; Check if the block has the visibility property
        (if dyn-props
          (foreach prop dyn-props
            (if (and (eq (vlax-get-property prop 'PropertyName) "Visibility1")
                     (vlax-property-available-p prop 'Value))
              (progn
                ;; Set visibility state based on user input
                (vlax-put-property prop 'Value (strcat "Revision " (itoa new-visibility)))
                (princ "\nVisibility property set successfully."))))))
      (princ "\nAll relevant blocks processed."))
    (princ "\nInvalid input or no blocks found with the expected properties."))
  (princ)
)
