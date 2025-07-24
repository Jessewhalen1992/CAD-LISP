(defun c:rdtxt (/ ss i ent val degrees minutes seconds newBearing numVal roundedVal currentNum numList char idx)
  (setq ss (ssget ":L" '((0 . "TEXT")))) ; Prompt user to select text entities
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq val (cdr (assoc 1 (entget ent))))
        (princ (strcat "\nProcessing text: " val))
        ;; Initialize variables
        (setq numList '()
              currentNum ""
              idx 1
        )
        ;; Check if the text contains any of the symbols
        (if (or (wcmatch val "*'*") (wcmatch val "*\"*") (wcmatch val "*°*") (wcmatch val "*%%d*"))
          (progn
            ;; Bearing text
            ;; Loop over each character
            (while (<= idx (strlen val))
              (setq char (substr val idx 1))
              (if (wcmatch char "[0-9]")
                (setq currentNum (strcat currentNum char))
                (progn
                  ;; Non-digit character, check if currentNum has accumulated digits
                  (if (not (equal currentNum ""))
                    (progn
                      (setq numList (append numList (list (atoi currentNum))))
                      (setq currentNum "")
                    )
                  )
                )
              )
              (setq idx (1+ idx))
            )
            ;; Check if any number left in currentNum
            (if (not (equal currentNum ""))
              (setq numList (append numList (list (atoi currentNum))))
            )
            ;; Now numList contains degrees, minutes, seconds
            ;; For bearings, we need at least degrees and minutes
            (if (>= (length numList) 2)
              (progn
                (setq degrees (nth 0 numList))
                (setq minutes (nth 1 numList))
                (setq seconds (if (> (length numList) 2) (nth 2 numList) 0))
                ;; Apply the rounding rules
                (if (member seconds '(30 35 40 45 50 55))
                  (setq minutes (1+ minutes))
                )
                ;; Seconds are removed in any case
                ;; Adjust degrees if minutes >= 60
                (if (>= minutes 60)
                  (progn
                    (setq minutes (- minutes 60))
                    (setq degrees (1+ degrees))
                  )
                )
                ;; Reconstruct the bearing without seconds
                (setq newBearing (strcat (itoa degrees) "%%d"
                                         (if (< minutes 10)
                                           (strcat "0" (itoa minutes))
                                           (itoa minutes))
                                         "'"
                                  ))
                ;; Update the text entity
                (entmod (subst (cons 1 newBearing) (assoc 1 (entget ent)) (entget ent)))
                (princ (strcat "\nUpdated bearing: " newBearing))
              )
              (prompt (strcat "\nInvalid bearing format in text: " val))
            )
          )
          ;; Else, treat as distance
          (progn
            (if (numberp (setq numVal (atof val)))
              (progn
                ;; Round numeric value to one decimal place (0.0 accuracy)
                (setq roundedVal (rtos numVal 2 1))
                ;; Update the text entity
                (entmod (subst (cons 1 roundedVal) (assoc 1 (entget ent)) (entget ent)))
                (princ (strcat "\nUpdated numeric value: " roundedVal))
              )
              (prompt (strcat "\nSkipped non-numeric text: " val))
            )
          )
        )
        (setq i (1+ i))
      )
      (princ "\nText values rounded successfully.")
    )
    (princ "\nNo text entities selected.")
  )
  (princ)
)
