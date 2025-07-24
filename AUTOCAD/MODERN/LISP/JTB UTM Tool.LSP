;JTB WORLD UTM LISP			  
;Versions:  
;03-Dec-2015
;12-Sept-2015				  
;-----------------------------------------
(vl-load-com)
(prompt "\n-----------------------------------")
(prompt "\nUTM Lisp | Dec 3rd 2015 | JTB World")
(prompt "\n-----------------------------------\n")

;To turn UTM on or off without scale drawing (at setup process):
;Command: (l+getUTMstatus 0) >>> turn off
;Command: (l+getUTMstatus 1) >>> turn on
;----------------
(defun C:UTM (/	*error* l*sshighlight l*savevars
	      doc layers layer
	     )
  ;select texts and mtexts then put to pickfirst selectionset
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  
  (if (not(equal (l+curspace)(vla-get-modelspace doc)))
  (prompt "*This command is not avaiable in paperspace*")
  (progn
  
  
  (if (> (l+getUTMScale nil) 0);(tblsearch "UCS" "UTM")
    (progn
      (l+begin)
      (l+runUTM)
      (l+end)
    )
    (progn
      (alert "Can not run UTM because UTM scale has not been set up. UTMSETTINGS will run instead.")
      (C:UTMSETTINGS)
    )
  )
  
  ))

  (princ)
)
;---------------------
(defun C:EDIT-NonUTM-Commands (/ *error* folderpath filepath fileid
			    text)
  (defun *error* (msg /)
    (princ)
  )

  (setq folderpath (strcat (getenv "APPDATA")
			   "/Autodesk/ApplicationPlugins/"
			   "JTB UTM Tool.bundle/Contents/Windows"
			   )
	filepath (strcat folderpath "/Non UTM Commands.csv")
  )

  (if (not(findfile filepath))
    (progn
      (setq fileid (open filepath "w"))
      (write-line "(Command Alias),(OrgCommand),(Lisp file name to be loaded)" fileid)
      (close fileid)
    )
  )

  (l+openFile filepath)

  (alert (strcat "Please edit your Non-UTM command list in opening Non UTM Commands.csv file."
		 (chr 13)
		 "Note: All command alias should be registered."
		 (chr 13)
		 "Once done, return to AutoCAD and click OK"))

  (l+initNonUTMCommand)
  
)
;----------------------------------------------------------
(defun l+initNonUTMCommand (/ *error* folderpath filepath lisppath fileid lispID
			    text lisp alias orgcommand)

  (defun *error* (msg /)
    (princ msg)
  )

  (setq folderpath (strcat (getenv "APPDATA")
			   "/Autodesk/ApplicationPlugins/"
			   "JTB UTM Tool.bundle/Contents/Windows"
	           )
	filepath (strcat folderpath "/Non UTM Commands.csv")
	lisppath (strcat folderpath "/Non UTM Command.lsp")
	fileID   (open filepath "r")
  )

  (while (and fileid (setq text (read-line fileid)))
    ;command alias - orgcommand
    (if (wcmatch text "(*)")
      (setq alias nil)
      (setq ar (l+split text ",")
	    alias (car ar)
	    orgcommand (cadr ar)
	    lispfile (caddr ar)
      )
    )
    
    (if (and alias orgcommand
	     (/= (setq alias (strcase (l+trim alias))) "")
	     (/= (setq orgcommand (strcase (l+trim orgcommand))) "")
	)
      
      (progn
	
	(prompt (strcat "\nRegister Ground Only Command: " alias " >>> " orgcommand " >>> "))
	
	(setq lisp (strcat (if lispfile (strcat "(load \"" lispfile  "\")") "")
			   (chr 10)

			   (if (= orgcommand (strcat "C:" alias))
			     (strcat "(setq org_" alias " " orgcommand ")" (chr 10));will this set org_alias to a reference, or a new value ?
			     ""
			   )

			   "(defun C:" alias " ()(if (l+getUTMstatus nil)(C:UTM))"

			   (if (wcmatch (strcase orgcommand) "C:*")
			     (if (= orgcommand (strcat "C:" alias))
			       (strcat "(org_" alias ")")
			       (strcat "(" orgcommand ")")
			     )			     
			     
			     (strcat "(command \"._" orgcommand "\")")  
			   )
			   
			   ")"

			   (chr 10)

			   (if (wcmatch (strcase orgcommand) "C:*")
			   ""
			   (strcat
			   "(defun C:" orgcommand " ()(if (l+getUTMstatus nil)(C:UTM))"			   
			   (strcat "(command \"._" orgcommand "\")")			   
			   ")")
			   )
			   
		   )
	      lispID   (open lisppath "w")
	)
	
	(write-line lisp lispID)
	(close lispID)
	;there can be error in lisp file
	(if (vl-catch-all-error-p (vl-catch-all-apply 'load (list lisppath)))
	  (prompt " ... Error in command name, or loading lisp.")
	  (prompt " ... Success.")
	)
      )
    )
  )
  
  (princ)
)

;---------------------
(defun l+runUTM (/	
	        doc layers layer
	        ss lockeds freezes i ent scalef el ename
	     )

  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
	layers (vla-get-layers doc)	
  )
  
  (command "undefine" "U");this can not put to at initialization process
  ;unlock, thaw all layers
  

  (vlax-for layer layers
    (if (= (vla-get-lock layer) :vlax-true)
      (setq lockeds (cons layer lockeds))
    )
    (if (= (vla-get-freeze layer) :vlax-true)
      (setq freezes (cons layer freezes))
    )
  )

  (foreach layer lockeds
    (vla-put-lock layer :vlax-false)
  )
  (foreach layer freezes
    (vla-put-freeze layer :vlax-false)
  )
  ;				
  (defun l+DimScale ()
    (if (= 1 (getvar "DimAnno"))
      (/ 1 (getvar "cannoscalevalue"))
      (getvar "DimScale")
    )
  )
	
  ;				
  
  (if (setq ss (ssget "_ALL" (list (cons -4 "<NOT")(cons 8 "UTM")(cons -4 "NOT>"))))
    (progn
      ;(if (vl-catch-all-error-p (vl-catch-all-apply  'vla-put-layer (list obj newlayer)))
      (setq scalef (l+getUTMScale nil)
	    i -1
	    base (l+getUTMbase nil)
      )            

      (if (l+getutmstatus nil)
	(progn
	  (setq scaleF (/ 1. scalef))
	  (prompt "Drawing is in GROUND. ")
	)
	(progn
	  ;save normal color:	  
	  (setq normalcolor (getenv "XhairPickboxEtc"))
	  (l+GetVar "NormalColor" "JTB UTM Tool" normalcolor "0");current
	  (prompt "Drawing is in UTM. ")
	)	
      )

      (command "._scale" ss "" (utm-transw2c base nil) scalef);ucs wcs

      ;dealing with Dimensions and Leaders and Annotative:
      ;(repeat (sslength ss)
	;(setq i (1+ i)
	      ;ent (ssname ss i)
	      ;el  (entget ent)
	      ;ename (cdr(assoc 0 el))
	;)

	
      ;);repeat
      
    );progn
  );if ss
  
  (if (l+getutmstatus nil)(l+getutmstatus 0)(l+getutmstatus 1))
  ;restore locked, freeze layers
  (foreach layer lockeds
    (vla-put-lock layer :vlax-true)
  )
  (foreach layer freezes
    (vla-put-freeze layer :vlax-true)
  )
)


;--------------
(defun l+getUTMbase (newbase /)
  (l+getUCSprop "utm" 10 (list 0. 0. 0.) newbase)
)
;-------------------
(defun l+getUTMscale (newscale / oldscale curstatus defaultScale)
  (setq defaultScale 0;0.99953886
	oldscale (l+getUCSprop "utm" 146 defaultScale nil)
  )  

  (if newscale (if (<= oldscale 0) (setq newscale (- (abs newscale)))(setq newscale (abs newscale))))
  (setq oldscale (abs oldscale))

  ;(if (setq curstatus (l+getUTMstatus nil));it is scaled, got to scale it up
    ;(if newscale (l+runUTM)); too smart is not good sometime
  ;)

  (l+getUCSprop "utm" 146 defaultScale newscale)

  ;then scale them down ?
  ;(if curstatus (if newscale (l+runUTM))); too smart is not good sometime
  
  ;return:
  ;(if (= 0 oldscale) defaultScale oldscale)
  oldscale
)
;---------------

;---------------

(defun C:U ()
  ;undo can not be override ?
  (command "._U")
  (l+updUTM)
  (princ)
)
;-----------------
(defun l+updUTM (/ doc layers color)  
  (if (l+getUTMstatus nil)
    (l+getUTMstatus 1)
    (l+getUTMstatus 0)
  )

  (if (l+getUTMstatus nil)
    (prompt "\nDrawing is in UTM.\n")    
    (if (/= 0 (l+getUTMscale nil))
      (prompt "\nDrawing is in GROUND.\n")
    )
  )
)
;-------------------------------------------
(defun l+getUTMColor (newcolorIndex / doc layers red green blue newcolor color)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
	layers (vla-get-layers doc)	
  )

  (setq color (l+GetVar "UtmColor" "JTB UTM Tool" nil "255"))
  
  (if newcolorIndex
    (progn
      (l+initUTMlayer)
      (setq layer (vla-item layers "UTM")
	    newcolor (vla-get-truecolor layer)	    
      )
      (vla-put-colorindex newcolor newcolorindex)
      (vla-put-truecolor layer newcolor)

      (setq color (vla-get-truecolor layer)
	    red (vla-get-red color)
	    green (vla-get-green color)
	    blue (vla-get-blue color)
	    color(+ red (* green 256)(* blue 256 256))
	    color (itoa color);(l+dec2hex color)
      )
      ;turn it to hexa: no need !
      (l+GetVar "UtmColor" "JTB UTM Tool" color nil);save this to settings init      
      (l+updUTM)
    )
  )

  color
)
;-------------
(defun l+Dec2Hex (n / i a res)
  (setq i 0
	res "")
  (while (> n 0)
    (setq i (1+ i))
    (setq a (rem n 16)
	  n (fix (/ n 16))
	  a (nth a (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))
	  res (strcat a res)
    )
        
  );while
)
;-----------------------------------
(defun l+initUTMLayer (/ doc layers)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
	layers (vla-get-layers doc)	
  )

  (if (not(tblsearch "layer" "UTM"))
    (entmake (list (cons 0 "LAYER")
		   (cons 100 "AcDbSymbolTableRecord")
		   (cons 100 "AcDbLayerTableRecord")
		   (cons 2 "UTM")
		   (cons 70 2)
		   (cons 62 1)
		   (cons 6 "Continuous")
		   (cons 290 0)
		   ;(cons 370 -3)		   
	     )
    )
  )
)
;--------------
(defun l+getUTMstatus (newstatus / curstatus utmcolor normalcolor
		       doc layers layer color red green blue
		       scale
		       );new status should be nil, 0 or 1, where nil = change nothing
  ;(if newstatus (setq newstatus (if (= newstatus 0) 0 16)))
  (setq scale (l+getUCSprop "utm" 146 0 nil)
	;scale (if (= scale 0) -0.99953886 scale);default status is T
	curstatus (< scale 0)
	;curstatus (l+getUCSprop "utm" 70 0 newstatus)
	utmcolor (l+getUTMcolor nil)
	normalcolor (l+GetVar "NormalColor" "JTB UTM Tool" nil "0")
	scale (abs scale)
  )

  ;switch color:
  ;(if (> scale 0); even UTM is not set, got to switch color back to normal
  (if (= 0 newstatus)
    (progn
      (if (> scale 0)(if curstatus (l+getUCSprop "utm" 146 nil scale)))
      (setenv "LayoutXhairPickboxEtc" normalcolor)
      (setenv "XhairPickboxEtc"       normalcolor)
      (l+GetVar "NormalColor" "JTB UTM Tool" normalcolor nil)
    )
    (if (= 1 newstatus)
      (progn
	(l+getUCSprop "utm" 146 nil (- scale))
	(setenv "LayoutXhairPickboxEtc" utmcolor)
	(setenv "XhairPickboxEtc"       utmcolor)

	(l+initUTMlayer)
	;set utm layer's color:
	(setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
	      layers (vla-get-layers doc)
	      layer (vla-item layers "UTM")
	      color (vla-get-truecolor layer)
	      
	      utmcolor (atoi utmcolor)
	      red (rem utmcolor 256)
	      utmcolor (fix (/ utmcolor 256))
	      green (rem utmcolor 256)
	      blue (fix (/ utmcolor 256))
	)

	(vla-setRGB color red green blue)	
	(vla-put-truecolor layer color)
      )
    )
  );)
  
  ;(> curstatus 0)
  curstatus
)
;-------------------

(defun l+getUCSprop (UCSName dxf defvalue newvalue / oldvalue)
  
  (if (setq utm (tblsearch "UCS" UCSNAME))
    (progn
      (setq oldvalue (cdr(assoc dxf utm)))
      (if newvalue
	(progn
	  (setq utm (tblobjname "UCS" ucsName)
		utm (entget utm)
		utm (subst (cons dxf newvalue)(assoc dxf utm) utm)
	  )
	  (entmod utm)	  	  
	)
      )
    )

    (progn
      (setq oldvalue defvalue)

      (if newvalue
	(progn
	  (setq utm (list (cons 0 "UCS")
			  (cons 100 "AcDbSymbolTableRecord")
			  (cons 100 "AcDbUCSTableRecord")
			  (cons 2 ucsName)
			  (cons 70 0)
			  (cons 10 (list 0 0 0))
			  (cons 11 (list 1.0 0.0 0.0))
			  (cons 12 (list 0.0 1.0 0.0))
			  (cons 79 0)
			  (cons 146 0.0);this can be used to save scale factor
		    )
		utm (subst (cons dxf newvalue)(assoc dxf utm) utm)
	  )
	  
	  (entmake utm)
	)
      )
    )
  )

  oldvalue
)


;------------------------------------
; 19 JULY - AUDIT FOR BRICSCAD USAGE:
;------------------------------------
(defun l+highlight (ent flag / entx newent i_ent i el)
  (setq i -1)
  (if (null l*sshighlight)(setq l*sshighlight (ssadd)))
  
  (cond
    ((null ent) nil)
    ((= 'ENAME (type ent))
    (progn
      (setq el (entget ent)
	    etype (cdr(assoc 0 el))
	    )
      
      (if (= "ATTRIB" etype)
	(setq ent (cdr(assoc 330 el)))
      )
	
      (setq entx (vlax-ename->vla-object ent))
      (if entx (vlax-invoke-method entx 'Highlight flag))
      (if flag
	(ssadd ent l*sshighlight)
      )
    ))    
    
    ((repeat (sslength ent)
      (setq i (1+ i)
	    i_ent (ssname ent i)	    
	    )
      (l+highlight i_ent flag)
    ))
  )
)


;ERROR HANDLE FUNCTIONS ------------------------------------
(defun l+begin (/ l+undo_init undo_ctl)
  (defun *error* (msg /)
    (if command-s;(> (atof (getvar 'acadver)) 19.0)
      (progn
	;it depends: sometime it is good to undo all thing. sometime, just undo 1 step back.
	(command-s "_.undo" "_back")
	(command-s "_.undo" "_end")
	;(command-s "_.U")
      )
      (command "_.undo" "_back" "_.undo" "_end")
    )    

    ;(prompt (strcat "\n" msg "\n"));debug purpose
    
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nError: " msg))    )
    
    (l+highlight l*sshighlight 0)
    (setq l*sshighlight nil)
    (mapcar 'setvar (list "cmdecho" "osnapcoord") l*savevars)
    (princ)
  );defun *error*
  
  (defun l+undo_init (/ undo_ctl)
  ;(l+b_set_sysvars (list "cmdecho" 0))
  (setq undo_ctl (getvar "undoctl"))

  ;1st. Make sure undo is fully enabled:
  (if (equal 0 (getvar "UNDOCTL")) 
    (command "_.undo" "_all")  )

  (if (or (not (equal 1 (logand 1 (getvar "UNDOCTL"))))
          (equal 2 (logand 2 (getvar "UNDOCTL")))      )
    (command "_.undo" "_control" "_all")  )

  ;2nd. Ensure undo auto is off:
  (if (equal 4 (logand 4 (getvar "undoctl")))
    (command "_.undo" "_Auto" "_off")  )

  ;3rd. Place an end mark down if needed:
  (while (equal 8 (logand 8 (getvar "undoctl")))
    (command "_.undo" "_end")  )

  ;4th. Start new undo group:
  (while (not (equal 8 (logand 8 (getvar "undoctl"))))
    (command "_.undo" "_begin")  )

  ;(l+b_restore_sysvars) 
  ;return original value of undoctl
  undo_ctl
  );defun l+undo_init

  ;MAIN:
  (setq l*sshighlight nil)
  (setq l*savevars (mapcar 'getvar (list "cmdecho" "osnapcoord")))

  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)

  (setq undo_ctl (l+undo_init)) ;= setting up undoctl + (command "_.undo" "_begin")
)
;------------------------------------
(defun l+end ()
  (l+highlight l*sshighlight 0)
  (setq l*sshighlight nil)
  (command "_.undo" "_end")
  (mapcar 'setvar (list "cmdecho" "osnapcoord") l*savevars)
  (princ)
)
;--------------
(defun l+curspace (/ acadapp thisdrawing space curspace)
  (setq acadapp     (vlax-get-acad-object)
	thisdrawing (vlax-get-property acadapp 'ActiveDocument)
	curspace    (vlax-get-property
		      (vlax-get-property thisdrawing 'ActiveLayout)
		      'Block
		    )
	space   (vla-get-activespace thisdrawing)
	curpace (if (and (= space 0)(= :vlax-false (vla-get-mspace thisdrawing)))
		    curspace
		    (vla-get-modelspace thisdrawing)		  
		)
  )
)
;---------------------------------
(defun l+transC2W (point vector /)
  (trans point 1 0 vector)
)
;----------------
(defun utm-transW2C (point vector /)
  (trans point 0 1 vector)
)
;-------------------------------------
(defun l+DimScale ()
  (if (= 1 (getvar "DimAnno"))
    (/ 1 (getvar "cannoscalevalue"))
    (getvar "DimScale")
  )
)
;-------------------------------------
(defun l+DimTextHeight (/ dimscale txtstyle dimtxt)
  (setq txtStyle (tblsearch "STYLE" (getvar "DIMTXSTY"))
	dimtxt (cdr(assoc 40 txtstyle))
  )

  (if (= dimtxt 0)
    (setq dimtxt (getvar "DIMTXT")
	  dimscale (l+DimScale)
    )
    (setq dimscale 1)
  )

  (if (= 0 dimscale) dimtxt (* dimscale dimtxt))
)





;----------------------
(defun C:UTMSETTINGS (/ cmds cmd
		     *error* l*sshighlight l*savevars
		     doc layers layer
		     L i
		     pathfunc

		     ;DCL:
		     DCL_id whatnext _accept _Show _save _switch _pickcolor _pickbase

		     newcolor newscale newbase newstatus newrun
		     ccolor cscale cbase cstatus
		    )

  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
	layers (vla-get-layers doc)	
  )
  
  (if (not(equal (l+curspace)(vla-get-modelspace doc)))
  (prompt "*This command is not avaiable in paperspace*")
  (progn

  (L+initUTMlayer)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object))
	layers (vla-get-layers doc)
	layer (vla-item layers "UTM")
  )
  ;------------------------------
  (defun _accept (/)
    (_save)
    (done_dialog 1)
  )

  ;--
  (setq ccolor (l+getutmcolor  nil)
	cscale (l+getutmscale  nil)
	cbase  (utm-transw2c (l+getutmbase   nil) nil)
	cstatus(l+getutmstatus nil)
	newstatus cstatus
  )

  (if (not cstatus)
	(progn	  
	  (setq normalcolor (getenv "XhairPickboxEtc"))
	  (l+GetVar "NormalColor" "JTB UTM Tool" normalcolor "0")
	)
  )

  ;------------------
  (defun _Show (/ base)

  (set_tile "cstatus" (if cstatus "CURRENT MODE: UTM" "CURRENT MODE: GROUND"))

  (setq base (if newbase newbase cbase))
  (set_tile "scale" (rtos (if newscale newscale cscale) 2 9))

  

  (set_tile "baseXYZ" (strcat "X = " (rtos (car   base) 2) " "
			      "Y = " (rtos (cadr  base) 2) ;"  "
			      ;(rtos (caddr base) 2)
		      )
  )

  nil
  )
  ;							
  (defun _pickcolor (/ ccolorindex new)

    (if newcolor
      (setq ccolorindex newcolor)
      (setq ccolorindex (vla-get-colorindex (vla-get-truecolor layer)))
    )

    (setq new (acad_colordlg ccolorindex nil))

    (if new
      (progn
	(setq newcolor new);index
	;change tile color:
	;can not change it !
      )
    )
  )
  ;							
  (defun _pickbase (/ pp)
    (setq pp (getpoint (if newbase newbase cbase)))
    (if pp
      (progn
	(setq newbase pp)
      )
    );if
  )
  ;				
  
  (defun _save (/)
    (setq newscale (atof(get_tile "scale")))
  )
  
  ;MAIN:

  (l+begin)
  
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  
  (setq i 0)
  
  
  ;;; DIALOG
  (setq dcl_id (load_dialog "UTM.dcl")
	whatnext 2
  )  

  
  

    (while (progn
	     (if (and (>= whatnext 2)
		      (or (<= dcl_id 0) (not (new_dialog "UTM_Setting" dcl_id))) ; Initialize the dialog.
		      )
	       (progn
		 (alert "Error loading UTM SETTING dialog.")
		 (setq whatnext -1)
	       )
	     )
	     
	     (>= whatnext 2)
	   )
      
      (_show)
      (action_tile "accept" "(_accept)")
      ;(action_tile "cmdRun" "(setq newrun T)(_accept)")
      ;(action_tile "cmdSwitch" "(_save)(done_dialog 5)")
      ;(action_tile "scale" "(setq newscale (get_tile \"scale\"))")
      (action_tile "cmdPick"  "(_save)(done_dialog 3)")
      (action_tile "cmdColor" "(_save)(done_dialog 4)")
      ;(action_tile "cmdDefScale" "(set_tile \"scale\" \"0.99953886\")")

      
      (setq whatnext (start_dialog))

      ;(setq newscale (atof (get_tile "scale"))); can not do it here if dialog is done !
      
      (if (= whatnext 3)(_pickbase))
      (if (= whatnext 4)(_pickcolor))
      ;(if (= whatnext 5)(setq newstatus (not newstatus)))
      (if (< whatnext 2)(unload_dialog dcl_id))
    );while

  ;;; DIALOG

  (if (= whatnext 1)
    (progn
      (if newcolor (l+getutmcolor newcolor))      
      (if newbase (l+getUTMbase (l+transc2w newbase nil)))

      (if (/= cscale newscale)
	(if (or ( = cscale 0)( = newscale 0))
	  (progn
	    (l+getUTMscale newscale);turn UTM on or off
	    
	    (if ( = cscale 0)
	      (prompt "UTM is enabled.")
	      (prompt "UTM is disabled.")
	    )	    
	  )
	  
	  (progn
	    (if (not cstatus)(l+runUTM));scale it back to UTM
	    (l+getUTMscale newscale)
	    (if (not cstatus)(l+runUTM));scale it back to Ground	    
	  )
	)
       )

      (if (/= cscale newscale) (setq cscale newscale))
      
      ;(if (/= cstatus newstatus) (if (= cscale 0)
				   ;(prompt "UTM is not set up. UTM Scale = 0. Can not change UTM mode.")
				   ;(l+getUTMstatus (if newstatus 1 0))
				 ;)
      ;)      
      (l+updUTM)
    )      
  )
  
  (l+end)
  ))
  ;(if (= whatnext 1)(if newrun (l+runUTM)))
  (princ)
)

;July 2015 - Audit - Transform to BricsCAD***
; 1 : OK !!!
;
;
;JTB World Application for AutoCAD* and BricsCAD
(defun l+isBricsCAD ()
  (wcmatch (getvar 'acadver) "*BricsCAD")
)
;--------------------------------------------------
(defun l+AddIn-Bundle-Contents-Path (appname /)
  ;just use Autodesk/ApplicationPlugins/ folder for All-In-One solution ?
  ;other thing like Lisp des, start up routines, setup cuix will be made by separate setup application.
  (if (l+isBricsCAD)
    (progn
      (vl-mkdir (strcat (getenv "APPDATA")
	     "/Autodesk"))
      (vl-mkdir (strcat (getenv "APPDATA")
	     "/Autodesk/ApplicationPlugins"))
    )
  )
  ;return:
  (strcat
	   (getenv "APPDATA")
	   ;(if (l+isBricsCAD)
	     ;will change this to BricsCAD store requirement
	     ;"/Bricsys/BricsCAD/"
	     "/Autodesk/ApplicationPlugins/"
	   ;)
	   appname ".bundle/Contents"
  )
)
;---------------------------------------------
(defun l+GetVar (varname Appname newvalue default_If_nil / path filepath flag varL file
		 text artexts value var strtype)
  
  (setq path (l+AddIn-Bundle-Contents-Path appname)
	filepath (strcat path "/Windows/Settings.ini")
  )

  (if newvalue
    (cond
      ;((= (type newvalue) 'STR)
      ;)
      ((= (type newvalue) 'INT)
       (setq newvalue (strcat "%" (itoa newvalue)))
      )
      ((= (type newvalue) 'REAL)
       (setq newvalue (strcat "#"(rtos newvalue)))
      )
      (T
       (setq newvalue (strcat "$" newvalue))
      )
    )
  )

  
  (setq file (open filepath "r") flag T varname (strcat (strcase varname) " ") varL (strlen varname))

  (if newvalue (setq artexts (list (strcat varname newvalue))));no file found

  (if file
    (while (and flag (setq text (read-line file)))
      (setq ;ar (l+split text " ")
	    var (substr text 1 varL)
	    var (strcase var)
      )

      (if (= varname var)
	(progn
	  (setq flag (if newvalue T nil)
		value (l+cutleft text varL);it is integer, real or other depend on varname type
		strtype (substr value 1 1)
		value (l+cutleft value 1)  ;it is integer, real or other depend on varname type
	  )

	  (cond
	    ((= strtype "%")(setq value (atoi value)))
	    ((= strtype "#")(setq value (atof value)))
	  )	  
	)
	
	(if newvalue
	  (setq artexts (append artexts (list text)))
	)
      )
    )    
  )

  (if file (close file))

  (if newvalue
    (progn      
      (setq file (open filepath "w"))
      (if file
	(foreach text artexts
	  (write-line text file)
	)
      )

      (if file (close file))	
    )
  )

  ;it will return the old value:
  (if value value default_If_nil);nil = not found
)
;--------- 1 dependency:
(defun l+CutLeft (str n /)
  (substr str (1+ n) (strlen str))
)

;---------------------------

(defun l+UpdUTMHandler (reactor datalist / doc newpath)  
  (setq doc (car datalist)
	;newpath (vla-get-path doc)
  )
  (l+updUTM)  
)

;----------------------------
(defun l+Split (txt delimiter / artexts text i txtlength ch);from Ycad
  (if (null delimiter)(setq delimiter (chr 9)))
  
  (setq txtlength (strlen txt) i 0 text "" artexts (list))
  
  (setq dlength (strlen delimiter))
  
  (repeat txtlength
    (setq i (1+ i)
	  ch (substr txt i dlength)
    )
    
    (if (/= ch delimiter)
      (setq text (strcat text ch))
      (progn
	;format text
	(setq text (l+Trim text))
	;(if (or(= text "")(= text "-"))(setq text nil)(setq text (l+formatTextData text)))
	(setq artexts (cons text artexts)
	      text ""
	)
      )
    )    
  )
  
  (if (/= text "") (setq artexts (cons text artexts)));the last field in the record
  
(reverse artexts)
)

;----------------------
(defun l+Trim (str / i l m1 m2 f ch)
  (cond
    ((listp str)
     (mapcar 'l+Trim str)
    )
    
    (T
     (setq l (strlen str)
	   i 0
	   f T
	   m1 1 m2 l
     )


     (while f (setq i (1+ i)  ch nil)
       (if (> i l) (setq f nil) (setq ch (substr str i 1)))
       (if (/= ch " ") (setq m1 i f nil))
     )

     (setq i (1+ l) f T)

     (while f (setq i (1- i)  ch nil)
       (if (= i 0) (setq f nil) (setq ch (substr str i 1)))
       (if (/= ch " ") (setq m2 i f nil))
     )

     (substr str m1 (- m2 m1 -1))
     )
  );cond
)


;-------------------------
(defun l+OpenFile (filepath / shlobj)
  
   (setq
     ShlObj (vla-getInterfaceObject
             (vlax-get-acad-object)
             "Shell.Application"
           )
   )
   (vlax-invoke-method ShlObj 'ShellExecute filepath)
   (vlax-release-object ShlObj)
)



;-------------------------
(vlr-docmanager-reactor "JTB UTM"
      '((:vlr-documentToBeActivated . l+updUTMHandler))
    )
;--------------------------------
(l+updUTM)  ;at load, then the cursor is update its color
;---------------------
(l+initNonUTMCommand)
(princ)