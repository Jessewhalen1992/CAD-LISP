;;inserts NW LSD block
(defun C:NW ()
(command "-insert" "*label_lsd_nw"pause "" "")
)

;;inserts SW LSD block
(defun C:SW ()
(command "-insert" "*label_lsd_sw"pause "" "")
)

;;inserts SE LSD block
(defun C:SE ()
(command "-insert" "*label_lsd_se"pause "" "")
)

;;inserts NE LSD block
(defun C:NE ()
(command "-insert" "*label_lsd_ne"pause "" "")
)

;;inserts power pole
(defun C:PP ()
(command "-insert" "powerpole"pause "5" "" "")
)

;;inserts Fd.I.
(defun C:FDI ()
(command "-insert" "FDI"pause "5" "" "")
)

;;inserts Dynamic Scale Bar
(defun C:SB ()
(command "-insert" "TBK_SCALEBAR_STC_STR"pause "" "")
)

;;inserts Well Elevation Bubble
(defun C:WE ()
(command "-insert" "we"pause "" "")
)

;;inserts Well Approximate Elevation Bubble
(defun C:WA ()
(command "-insert" "wa"pause "" "")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;COMMANDS WORK WITH POINT CREATE;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Inserts NEZD block
(defun C:PIN()
(command "pinsert")
)

;;Setup NEZD layers
(defun C:PSET()
(command "psetting")
)

;;Converting existing NEZD into NEZD blocks for export
(defun C:PC()
(command "pconvert")
)

;;Point Export
(defun C:PX()
(command "pexport")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----- draw line from x to y using snaps
(defun c:lee  ()
    (command "_line" "_endp" pause "_endp" pause "")
    (princ)
    )
(defun c:lei  ()
    (command "_line" "_endp" pause "_int" pause "")
    (princ)
    )
(defun c:lie  ()
    (command "_line" "_int" pause "_endp" pause "")
    (princ)
    )




