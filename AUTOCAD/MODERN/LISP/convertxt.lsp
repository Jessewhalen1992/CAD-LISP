(defun c:Cts (/ oldFont newFont ss ent convertedCount)
  (setq oldFont "Book Antiqua") ; Update this with the actual font of your MTEXT
  (setq newFont "Arial Narrow")

  ;; Prompt user to select MTEXT objects
  (setq ss (ssget '((0 . "MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (setq convertedCount 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq textContent (cdr (assoc 1 (entget ent)))) ; Get the text content of MTEXT
        (setq oldFontFound (vl-string-search oldFont textContent)) ; Check if old font is in text content
        (if oldFontFound
          (progn
            (setq newTextContent (vl-string-subst newFont oldFont textContent)) ; Replace old font with new font
            (entmod (subst (cons 1 newTextContent) (assoc 1 (entget ent)) (entget ent))) ; Update MTEXT with new text content
            (setq convertedCount (1+ convertedCount))
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nConverted " (itoa convertedCount) " MTEXT entities to " newFont " font."))
    )
    (princ "\nNo MTEXT entities found.")
  )
  (princ)
)
