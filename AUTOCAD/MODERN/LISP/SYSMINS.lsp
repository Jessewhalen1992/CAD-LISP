(defun InsertBlock5 (blkName / pt)
  ;; This function repeatedly inserts "blkName" at scale = 5, rotation = 0
  ;; until the user presses Enter or Esc.
  (while
    (setq pt (getpoint (strcat "\nSpecify insertion point for " blkName " (Enter or Esc to exit): ")))
    (command "._INSERT" blkName pt 5 5 0)
  )
  (princ)
)
(defun InsertBlock1 (blkName / pt)
  ;; This function repeatedly inserts "blkName" at scale = 5, rotation = 0
  ;; until the user presses Enter or Esc.
  (while
    (setq pt (getpoint (strcat "\nSpecify insertion point for " blkName " (Enter or Esc to exit): ")))
    (command "._INSERT" blkName pt 1 1 0)
  )
  (princ)
)
