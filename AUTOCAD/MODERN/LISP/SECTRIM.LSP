(vl-load-com)

(defun c:SECTRIM ( / ent data ins xScale rad cir moveVec ss )
  (setvar "cmdecho" 0)
  (prompt "\nSelect block to trim around: ")
  (if (and (setq ent (car (entsel)))
           (setq data (entget ent))
           (= (cdr (assoc 0 data)) "INSERT"))
    (progn
      (setq ins    (cdr (assoc 10 data))
            xScale (cond ((cdr (assoc 41 data))) (1.0))
            rad    (* 40 xScale)
      )
      ;; Draw circle at insertion point
      (command "_.CIRCLE" ins rad)
      (setq cir (entlast))
      ;; Move circle down (negative Y) 10 units * X scale
      (setq moveVec (list 0.0 (* -10 xScale) 0.0))
      (command "_.MOVE" cir "" '(0 0 0) moveVec)
      ;; Trim all geometry crossing inside the circle
      (setq ss (ssget "_X"))
      (setq ss (ssdel cir ss))
      (if ss
        (vl-cmdf "_.TRIM" cir "" ss "")
      )
    )
    (prompt "\nBlock selection required.")
  )
  (setvar "cmdecho" 1)
  (princ)
)

(princ)
