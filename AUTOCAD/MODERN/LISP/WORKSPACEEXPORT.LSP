(defun pair-exists-p (pair data-list)
  "Check if a pair exists in the data-list.
   pair: A list containing two elements (temp_area_w1 enter_text).
   data-list: A list of such pairs.
   Returns T if the pair exists, NIL otherwise."
  (setq exists NIL)
  (foreach existing-pair data-list
    (if (and (equal (car existing-pair) (car pair))
             (equal (cadr existing-pair) (cadr pair)))
      (setq exists T)
    )
  )
  exists
)

(defun ExtractLastNumber (text)
  "Extract the last numerical value from a string."
  (vl-load-com) ; Ensure Visual LISP COM functions are loaded
  (setq regex (vlax-create-object "VBScript.RegExp"))
  (vlax-put-property regex 'Pattern "\\d+\\.\\d+")
  (vlax-put-property regex 'Global :vlax-true)
  (vlax-put-property regex 'IgnoreCase :vlax-true)
  
  (setq matches (vlax-invoke regex 'Execute text))
  (if (> (vlax-get-property matches 'Count) 0)
    (vlax-get-property (last matches) 'Value)
    ""
  )
)

(defun ExtractAllNumbers (text)
  "Extract all numerical values from a string and concatenate them separated by spaces."
  (vl-load-com) ; Ensure Visual LISP COM functions are loaded
  (setq regex (vlax-create-object "VBScript.RegExp"))
  (vlax-put-property regex 'Pattern "\\d+\\.\\d+")
  (vlax-put-property regex 'Global :vlax-true)
  (vlax-put-property regex 'IgnoreCase :vlax-true)
  
  (setq matches (vlax-invoke regex 'Execute text))
  (setq result "")
  (if (> (vlax-get-property matches 'Count) 0)
    (progn
      (foreach match matches
        (setq result (strcat result (vlax-get-property match 'Value) " "))
      )
      (strcase (vl-string-trim " " result)) ; Remove trailing space and convert to uppercase if needed
    )
    ""
  )
)

(defun c:WSE ()
  (vl-load-com)
  
  ;; Define the list of block names to process
  (setq block-names '("Dyn_temp_area" "TempArea-Blue" "TempArea_County" "Work_Area_Stretchy" "Temp_Area_Pink"))
  
  ;; Prompt user to select blocks
  (prompt "\nSelect blocks to export attributes: ")
  (setq sel (ssget '((0 . "INSERT"))))
  
  (if (not sel)
    (progn
      (prompt "\nNo blocks selected.")
      (exit)
    )
  )
  
  ;; Initialize a list to hold unique attribute data
  (setq data-list '())
  
  ;; Iterate through each selected block
  (setq i 0)
  (repeat (sslength sel)
    (setq ent (ssname sel i))
    (setq obj (vlax-ename->vla-object ent))
    
    ;; Retrieve the Effective Name of the block
    (setq block-name (vla-get-EffectiveName obj))
    
    (prompt (strcat "\nProcessing Block: " block-name))
    
    ;; Check if the block name is in the block-names list (case-sensitive)
    (if (member block-name block-names)
      (progn
        ;; Initialize variables for attributes
        (setq temp_area_w1 "")
        (setq enter_text "")
        
        ;; Retrieve attributes
        (setq attribs (vlax-invoke obj 'GetAttributes))
        
        ;; Iterate through the block's attributes
        (foreach att attribs
          (setq tag (vla-get-tagstring att)) ; Attribute tag as is
          (setq value (vla-get-textstring att)) ; Attribute value
          
          (prompt (strcat "\nFound Attribute: " tag " = " value))
          
          (cond
            ((equal tag "TEMP_AREA_W1")
             (setq temp_area_w1 value)
            )
            ((equal tag "ENTER_TEXT")
             (setq enter_text value)
            )
          )
        )
        
        ;; Debugging: Display the final values after assignment
        (prompt (strcat "\ntemp_area_w1: '" temp_area_w1 "'"))
        (prompt (strcat "\nenter_text: '" enter_text "'"))
        
        ;; Check if both attributes were found and have non-empty values
        (if (and (not (equal temp_area_w1 "")) (not (equal enter_text "")))
          (progn
            ;; Create a pair of the current attributes
            (setq current-pair (list temp_area_w1 enter_text))
            
            ;; Check if the pair already exists in data-list
            (if (not (pair-exists-p current-pair data-list))
              (progn
                ;; Append the unique pair to the data list
                (setq data-list (append data-list (list current-pair)))
                (prompt "\nAttributes added to export list.")
              )
              (prompt "\nDuplicate attribute pair found. Skipping.")
            )
          )
          (prompt "\nOne or more attributes not found in this block.")
        )
      )
      (prompt "\nBlock name does not match any in the processing list. Skipping.")
    )
    (setq i (1+ i))
  )
  
  ;; Check if data was collected
  (if (= (length data-list) 0)
    (progn
      (prompt "\nNo blocks with the specified names and attributes found.")
      (exit)
    )
  )
  
  ;; Prepare CSV lines as a list
  (setq csv-lines '("TEMP_AREA_W1,ENTER_TEXT"))
  
  ;; Add each unique pair as a new line
  (foreach row data-list
    (setq csv-lines (append csv-lines (list (strcat "\"" (car row) "\",\"" (cadr row) "\""))))
  )
  
  ;; Define the output file path
  ;; Change from TEMP directory to the drawing's directory
  (setq drawing-path (getvar "DWGPREFIX"))
  
  ;; Handle cases where the drawing hasn't been saved yet
  (if (or (equal drawing-path "") (null drawing-path))
    (progn
      ;; Prompt user to select a save location
      (prompt "\nDrawing has not been saved yet. Please select a location to save the CSV file.")
      (setq user-path (getfiled "Select Output CSV File" "" "csv" 1))
      (if user-path
        (setq csv-path user-path)
        (progn
          (prompt "\nNo valid path selected. Exiting.")
          (exit)
        )
      )
    )
    (progn
      ;; If drawing path exists, set CSV path to drawing directory
      (setq csv-path (strcat drawing-path "BlockAttributesExport.csv"))
    )
  )
  
  ;; Debugging: Show the CSV path
  (prompt (strcat "\nCSV Path: " csv-path))
  
  ;; Attempt to open the file for writing
  (setq file (open csv-path "w"))
  
  (if file
    (progn
      ;; Write each line to the CSV file
      (foreach line csv-lines
        (write-line line file)
      )
      (close file)
      (princ (strcat "\nData successfully exported to " csv-path))
    )
    (princ "\nFailed to create the CSV file. Please ensure the file is not open in another program and that you have write permissions.")
  )
  
  (princ)
)

;; Provide a message that the command is loaded
(princ "\nType 'WSE' to export block attributes to Excel.")
(princ)
