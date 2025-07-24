(defun c:DDL ()
  (vl-load-com)
  (setq *acad* (vlax-get-acad-object))
  (setq *doc* (vla-get-ActiveDocument *acad*))
  
  ;; Get the Named Object Dictionary (NOD)
  (setq nod (vla-get-Dictionaries *doc*))
  
  ;; Attempt to get the DataLink dictionary within the NOD
  (setq datalinkDict (vl-catch-all-apply 'vla-Item (list nod "ACAD_DATALINK")))
  
  ;; Check if the DataLinkDictionary exists
  (if (and datalinkDict (not (vl-catch-all-error-p datalinkDict)))
    (progn
      ;; Iterate through the DataLinkDictionary and delete each entry
      (vlax-for item datalinkDict
        (vla-delete item)
      )
      (princ "\nAll DataLinks have been deleted.")
    )
    (princ "\nNo DataLinks found.")
  )
  (princ)
)
