(defun c:tb (/ savAttdia savAttreq blkName ss i ent e p10 text)
(vl-load-com)
(setvar "cmdecho" 0); disable command echo
(command ".undo" "begin")
(setq savAttdia (getvar "attdia")); save attdia
(setq savAttreq (getvar "attreq")); save attreq
(setvar "attdia" 0); disable attribute dialog
(setvar "attreq" 1); enable attribute request
(if (/= (setq blkName (getstring t "\nBlock name: ")) "")
(if (tblsearch "block" blkName)
(if (setq ss (ssget (list '(0 . "TEXT")))); select only TEXT objects
(progn
(setq i -1)
(repeat (sslength ss)
(setq i (1+ i))
(setq e (entget (setq ent (ssname ss i))))
(setq p10 (cdr (assoc '10 e)))
(setq text (cdr (assoc '1 e)))
(entdel ent)
(command ".insert" blkName p10 "" "" "" text)
); repeat
); progn
); if
); if
(progn
(vlr-beep-reaction)
(prompt (strcat "\nBlock " blkName " is not exist."))
); progn
)
(setvar "attdia" savAttdia); restore attdia
(setvar "attreq" savAttreq); restore attreq
(command ".undo" "end")
(setvar "cmdecho" 1); restore command echo
(princ)
)
