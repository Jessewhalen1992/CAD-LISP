(defun C:SV (/ selSet total num obj objText i)
  ;; Check if UTM is enabled and trigger UTM logic if necessary
  (if (l+getUTMstatus nil)
    (C:UTM)
  )
  
  ;; Initialize total to zero
  (setq total 0)
  
  ;; Prompt the user to select text or mtext objects
  (setq selSet (ssget '((0 . "TEXT,MTEXT"))))  ; Only select TEXT and MTEXT objects
  
  ;; If selection is not empty
  (if selSet
    (progn
      ;; Loop through each selected object
      (repeat (setq i (sslength selSet))
        (setq obj (ssname selSet (setq i (1- i))))
        (setq objText (cdr (assoc 1 (entget obj))))  ; Get the text content

        ;; Try to convert text to a number and add to total
        (setq num (distof objText))  ; Converts string to a real number
        (if num
          (setq total (+ total num))  ; Add to total if it's a valid number
        )
      )
      ;; Display the total
      (princ (strcat "\nTotal sum: " (rtos total 2 2)))  ; 2 decimal places
    )
    (princ "\nNo valid text selected.")  ; Error message for empty selection
  )
  (princ)  ; Exit quietly
)
