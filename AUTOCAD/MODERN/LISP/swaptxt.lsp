w;;; Purpose:  SWAPS TEXT
;;;************************************************************************

(defun c:swap ()
  (setq sl1 (entget (car (entsel "\nPick first text to switch :"))))
  (setq first (cdr (assoc 1 sl1)))
  (setq sl2 (entget (car (entsel "\nPick second text to switch : "))))
  (setq second (cdr (assoc 1 sl2)))
  (setq sl1 (subst (cons 1 second)(cons 1 first) sl1))
  (entmod sl1)
  (setq sl2 (subst (cons 1 first)(cons 1 second) sl2))
  (entmod sl2)
  (princ)
)

