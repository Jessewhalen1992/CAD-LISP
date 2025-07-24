;;--------------------=={ Delete Blocks }==-------------------;;
;;                                                            ;;
;;  Displays a dialog interface prompting the user to select  ;;
;;  blocks to be deleted and proceeds to remove all traces of ;;
;;  selected blocks from the drawing.                         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    14-07-2012                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;

(defun c:de ( / *error* del lst )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (null (setq lst (LM:GetBlockNames)))
            (princ "\nNo Blocks found in Drawing.")
        )
        (   (null (setq lst (LM:ListBox "Select Blocks to Delete" lst t)))
            (princ "\n*Cancel*")
        )
        (   t
            (LM:startundo (LM:acdoc))
            (setq del  (LM:DeleteBlocks (LM:acdoc) lst))
            (vla-regen (LM:acdoc) acallviewports)
            (foreach block lst
                (if (member (strcase block) del)
                    (princ (strcat "\nDeleted block " block "."))
                    (princ (strcat "\nUnable to delete block " block "."))
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )    
    (princ)
)
        
;;--------------------=={ Delete Blocks }==-------------------;;
;;                                                            ;;
;;  Deletes all references of a list of blocks from a drawing ;;
;;  (including nested references, nested to any level).       ;;
;;  Proceeds to delete the associated block definitions from  ;;
;;  the drawing, if possible.                                 ;;
;;                                                            ;;
;;  This function is compatible with ObjectDBX.               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  docobj - VLA Document Object                              ;;
;;  blocks - List of blocks to be deleted, (case insensitive) ;;
;;------------------------------------------------------------;;
;;  Returns:  List of blocks that were successfully deleted.  ;;
;;------------------------------------------------------------;;

(defun LM:DeleteBlocks ( docobj blocks / blk lst out )
    (setq blk (vla-get-blocks docobj))
    (if (setq blocks
            (mapcar 'strcase
                (vl-remove-if
                    (function
                        (lambda ( name )
                            (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list blk name)))
                        )
                    )
                    blocks
                )
            )
        )
        (progn
            (vlax-for layer (vla-get-layers docobj)
                (if (eq :vlax-true (vla-get-lock layer))
                    (progn
                        (setq lst (cons layer lst))
                        (vla-put-lock layer :vlax-false)
                    )
                )
            )
            (vlax-for def blk
                (vlax-for obj def
                    (if
                        (and
                            (eq "AcDbBlockReference" (vla-get-objectname obj))
                            (or
                                (and
                                    (vlax-property-available-p obj 'effectivename)
                                    (member (strcase (vla-get-effectivename obj)) blocks)
                                )
                                (member (strcase (vla-get-name obj)) blocks)
                            )
                        )
                        (vl-catch-all-apply 'vla-delete (list obj))
                    )
                )
            )
            (foreach block blocks
                (if (null (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list (vla-item blk block)))))
                    (setq out (cons block out))
                )
            )
            (foreach layer lst (vla-put-lock layer :vlax-true))
            (reverse out)
        )
    )
)

;;-----------------------=={ List Box }==---------------------;;
;;                                                            ;;
;;  Displays a List Box allowing the user to make a selection ;;
;;  from the supplied data.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  title    - List Box Dialog title                          ;;
;;  lst      - List of Strings to display in the List Box     ;;
;;  multiple - Boolean flag to determine whether the user     ;;
;;             may select multiple items (T=Allow Multiple)   ;;
;;------------------------------------------------------------;;
;;  Returns:  List of selected items, else nil.               ;;
;;------------------------------------------------------------;;

(defun LM:ListBox ( title lst multiple / dch des tmp res )
    (cond
        (   (not
                (and
                    (setq tmp (vl-filename-mktemp nil nil ".dcl"))
                    (setq des (open tmp "w"))
                    (write-line
                        (strcat
                            "listbox : dialog { label = \""
                            title
                            "\"; spacer; : list_box { key = \"list\"; multiple_select = "
                            (if multiple "true" "false")
                            "; } spacer; ok_cancel;}"
                        )
                        des
                    )
                    (not (close des))
                    (< 0 (setq dch (load_dialog tmp)))
                    (new_dialog "listbox" dch)
                )
            )
            (prompt "\nError Loading List Box Dialog.")
        )
        (   t     
            (start_list "list")
            (foreach item lst (add_list item))
            (end_list)
            (setq res (set_tile "list" "0"))
            (action_tile "list" "(setq res $value)")
            (setq res
                (if (= 1 (start_dialog))
                    (mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" res ")")))
                )
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    (if (and tmp (setq tmp (findfile tmp)))
        (vl-file-delete tmp)
    )
    res
)

;; Get Block Names  -  Lee Mac
;; Returns an alphabetically sorted list of block names,
;; excluding anonymous and xref-dependent blocks.

(defun LM:GetBlockNames ( / bd bl )
    (while (setq bd (tblnext "BLOCK" (null bd)))
        (if (zerop (logand 125 (cdr (assoc 70 bd))))
            (setq bl (cons (cdr (assoc 2 bd)) bl))
        )
    )
    (vl-sort bl '<)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'UNDOCTL)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns a global pointer to the VLA Active Document Object

(defun LM:acdoc nil
    (cond ( acdoc ) ((setq acdoc (vla-get-activedocument (vlax-get-acad-object)))))
)

;;------------------------------------------------------------;;

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;