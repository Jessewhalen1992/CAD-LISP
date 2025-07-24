(defun c:M2FT ()
  (vl-load-com)
  ;; Prompt the user to select multiple TEXT and MTEXT entities
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (setq count 0)  ;; Counter for updated entities
      (while (< i n)
        (setq obj (ssname ss i))
        (setq entData (entget obj))
        (setq textString (cdr (assoc 1 entData)))
        ;; Check if the text is purely numeric
        (if (purely-numeric-p textString)
          (progn
            (setq metersValue (atof textString))
            (setq feetValue (* metersValue 3.2808357))
            (setq roundedFeetValue (rtos feetValue 2 2))
            (setq newString (strcat roundedFeetValue " ft"))
            ;; Update the entity with the new text
            (setq entData (subst (cons 1 newString) (assoc 1 entData) entData))
            (entmod entData)
            (setq count (1+ count))  ;; Increment counter
          )
          ;; Else, ignore entities with non-numeric characters
          (princ (strcat "\nSkipped non-numeric text: " textString))
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nConversion complete. " (itoa count) " text entities updated."))
    )
    (princ "\nNo TEXT or MTEXT entities selected.")
  )
  (princ)
)

(defun not-digit-or-dot-p (c)
  ;; Returns T if c is not a digit (0-9) or a decimal point '.'
  (not (or (and (>= c 48) (<= c 57))  ;; ASCII codes for '0' to '9'
           (= c 46))))                ;; ASCII code for '.'

(defun purely-numeric-p (str)
  ;; Returns T if str contains only digits and at most one decimal point
  (and
    (> (strlen str) 0)
    ;; Check that all characters are digits or a decimal point
    (not (vl-some 'not-digit-or-dot-p (vl-string->list str)))
    ;; Check that there is at most one decimal point
    (<= (count-char str ".") 1)
  )
)

(defun count-char (str char)
  ;; Counts occurrences of char in str
  (setq lst (vl-string->list str))
  (setq count 0)
  (foreach c lst
    (if (= c (ascii char))
      (setq count (1+ count))
    )
  )
  count
)
