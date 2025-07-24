(defun c:Tie ( / sel i ent obj oldStr newStr )
  ;; Prompt user to select TEXT or MTEXT objects
  (setq sel (ssget '((0 . "TEXT,MTEXT"))))
  
  (if sel
    (progn
      ;; Process each selected object
      (repeat (setq i (sslength sel))
        (setq i    (1- i)
              ent  (ssname sel i)
              obj  (vlax-ename->vla-object ent)
              oldStr (vla-get-TextString obj)
              ;; Append " (Tie)" to the end of the existing text
              newStr (strcat oldStr " (Tie)")
        )
        ;; Apply the new text value
        (vla-put-TextString obj newStr)
      )
      (princ "\n(Tie) has been added to all selected text objects.")
    )
    (prompt "\nNo TEXT or MTEXT objects selected.")
  )
  (princ)
)
