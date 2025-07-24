(setq mn_class '
      ((1 . "<Text><IMPORTANT><Plan Showing Survey of...>") 
       (2 . "<Text/Line><IMPORTANT><Scale Bar & Text>") 
       (3 . "<Text/Line><IMPORTANT><Legend, North Arrow, Geo-Ref Box>") 
       (4 . "<Text><IMPORTANT><Surveyor(ALS) Information, Permit Stamp & Box>") 
       (5 . "<Text/Line><IMPORTANT><Sheet Border, Client Name & Box, Compass Info & Box>") 
       (6 . "<Text/Line><IMPORTANT><Land Titles Information & Box (Excluding PLAN NO. text)>") 
       (7 . "<Text><Area Required 0.000 ha (In Body of Plan)>") 
       (8 . "<Line><IMPORTANT><Proposed Polygon (Closed)>") 
       (9 . "<Line><Property and RW Linework Outside Area to be Registered>") 
       (10 . "<Text><Text for Linework Outside Area to be Registered (bearings, distances, lot and block numbers, etc.)>") 
       (11 . "<Text/Line><Phantomized Linework for Superceded Plans and Phantomized Lot, Block and Plan Numbers>") 
       (12 . "<Text/Line><MASCOT Number and Symbol and ASCM tie linework and text; lines may be broken for plot purposes, not to scale>") 
       (13 . "<Text/Line><IMPORTANT><Symbols (Fd.I., Pl.I., c.s.,etc...) and associated text>") 
       (14 . "<Text/Line><Dimension Arrows and Lines>") 
       (15 . "<Text><IMPORTANT><Street Name, Lane, Road Allowance and WalkWay Text>") 
       (16 . "<Text/Line><Condominium Unit Factors Table, Individual Lot and Parcel Areas, Certificate of Title Number in Lot/Parcel>") 
       (17 . "<Line><IMPORTANT><Section, Block lines, Unbroken Within Area to be Registered>") 
       (18 . "<Line><Section, Lot, Block Lines up to Survey Post Within Area to be Registered (trimmed linework)>") 
       (19 . "<Text><IMPORTANT><Lot/Condo Unit Numbers Within Area to be Registered>") 
       (20 . "<Text><IMPORTANT><Block Numbers Within Area to be Registered>") 
       (21 . "<Text><Registered Plan Number, (generally blank, number added after registration)>")
       (22 . "<Text/Line><IMPORTANT><Line Bearings, Distances, Lot Dimensions, Arc, Radius, Chord Info B and E of Curve, Delta (Text) Within Area to be Registered and Associated Lead Lines, Arrowheads and Dimension Linework>") 
       (23 . "<Text><IMPORTANT><Street and Lane Widths Within Area to be Registered>") 
       (24 . "<Line><IMPORTANT><Lot Lines Unbroken Within Area to be Registered; including Bareland Condiminium Unit Boundary>") 
       (25 . "<Text/Line><IMPORTANT><ASCM Symbols and Text Identifier at True Coordinates or To Scale>") 
       (26 . "<Text/Line><IMPORTANT><Right-Of-Way Plan - Dimensions and Distances include assoc. lead lines, arrowheads and dimension linework within area to be registered>") 
       (27 . "<Line><IMPORTANT><Right-Of-Way Dashed Line - for new R/W plans within new subdivision plans>") 
       (28 . "<Line><IMPORTANT><Right-Of-Way Linework, for new R/W plans>") 
       (29 . "<Text><IMPORTANT><Right-Of-Way Descriptor (e.g. Utility R/W)>") 
       (30 . "<Text/Line><Lot Line and Text in Hydrology - Within Hydrology>") 
       (31 . "<Text/Line><Section, Block Line in Hydrology, Block Nos., Dimension Within Hydrology>") 
       (32 . "<Text/Line><Hydrographic Features Linework and Feature Names>") 
       (33 . "<Text><IMPORTANT><Location/Place Name (City, Town, County, Municipal District)>") 
       (34 . "<See Detail Text & Entire Details>") 
       (35 . "<Reference Point Symbols & Text>")
       (36 . "<Tie Lines & Text>")
       (37 . "<Monument Plan (Existing) Linework & Text>")
       (38 . "<Dashed Line Across Road>")
      ))
;;; 
;;; 0 = LINE, 1 = TEXT, 2 = LINE/TEXT
;;;
(setq mn_obj '
      ((1 . 1) 
       (2 . 2) 
       (3 . 2) 
       (4 . 1) 
       (5 . 2) 
       (6 . 2) 
       (8 . 0) 
       (13 . 2) 
       (15 . 1) 
       (17 . 0) 
       (19 . 1) 
       (20 . 1) 
       (22 . 2) 
       (23 . 1) 
       (24 . 0) 
       (25 . 2) 
       (26 . 2) 
       (27 . 0) 
       (28 . 0) 
       (29 . 1) 
       (33 . 1)
      ))
;;; 
;;; 
(defun count_obj (layer_name obj_type) 
  (setq all_obj (ssget "x" (list (cons 8 layer_name))))
  (cond
    ((= obj_type 0) (setq obj (ssget "x" (list (cons 0 "LINE") (cons 8 layer_name))))) 
    ((= obj_type 1) (setq obj (ssget "x" (list (cons 0 "TEXT") (cons 8 layer_name))))) 
    ((= obj_type 2) (setq obj (ssget "x" (list (cons -4 "<OR") (cons -4 "<AND") 
                                               (cons 0 "LINE") 
                                               (cons 8 layer_name) 
                                               (cons -4 "AND>") 
                                               (cons -4 "<AND") 
                                               (cons 0 "TEXT") 
                                               (cons 8 layer_name) 
                                               (cons -4 "AND>") 
                                               (cons -4 "OR>")
                                         )
                              ))
    )
  )
) 
;;;
;;;
;;;
(defun c:check ()
  (princ "Please wait...")
  (setvar "cmdecho" 0)
  (command "_.UNDO" "_M") 
  (setvar "REGENMODE" 1)
  (setvar "ANGBASE" 0)
  (command "_.LAYER" "_T" "*" "_ON" "*" "")
  (command "_.CHPROP" "_ALL" "" "_C" "7" "")
  (command "_.LAYER" "_S" "5" "")
  (command "_.LAYER" "_OFF" "*" "" "")
  (command "_.ZOOM" "_E")
  (initget 1) 
  (setq mn_pt1 (getpoint "\rSelect lower left corner: "))
  (initget 1) 
  (setq mn_pt2 (getcorner mn_pt1 "\nSelect upper right corner: "))
  (command "_.ZOOM" "_W" mn_pt1 mn_pt2)
  (command "_.LAYER" "_ON" "*" "_S" "0" "")
  (setq nxt_layer 1)
  (initget "Screen Printer") 
  (setq mn_qt1 (getkword "\nEnter the preferred output (Screen/Printer): <S> "))
  (if (= mn_qt1 "Printer") 
    (progn
      (setq mn_hgt (- (cadr mn_pt2) (cadr mn_pt1)))
      (setq txt_size (* 2.0 (/ mn_hgt 190)))
      (command "_.STYLE" "MN_LABEL" "ROMANS" "" "" "" "" "" "")

      (command "_.PLOT" "_D" "0")



      (initget "Yes No")
      (setq mn_nxt (getkword "Do you wish to print the individual layers? (Yes/No): <Yes> "))
      (if (/= mn_nxt "No")
        (progn
      

          (while (< nxt_layer 39) 
            (command "_.LAYER" "_M" (itoa nxt_layer) "_OFF" "*" "" "") 
            (command "_.TEXT" "_S" "MN_LABEL" (polar mn_pt1 0.785398 txt_size) 
                 txt_size "0" 
                 (strcat " LAYER " (itoa nxt_layer) " - " (cdr (assoc nxt_layer 
                                                                      mn_class
                                                               )
                                                          )
                 )
            ) 
            (command "_.CHPROP" "_L" "" "_C" "7" "") 
            (command "_.PLOT" "_D" "0") 
            (setq nxt_layer (1+ nxt_layer))
          ) 
          (command "_.LAYER" "_M" "39" "_ON" "*" "_OFF" 
"1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,MN_SUMMARY"
             ""
          ) 
          (command "_.TEXT" "_S" "MN_LABEL" (polar mn_pt1 0.785398 txt_size) txt_size 
             "0" 
               (strcat " LAYER " (itoa nxt_layer) " - " (cdr (assoc nxt_layer 
                                                                    mn_class
                                                             )
                                                        )
               )
          ) 
          (command "_.CHPROP" "_L" "" "_C" "7" "") 
          (command "_.PLOT" "_D" "0") 
        )
      )
      (command "_.UNDO" "_B")
    )
    (progn
;      (textscr) 
;      (prompt "\n\tSummary Report for Important Layers Only\n") 
;      (setq cnt 0)
;      (while (setq item (nth cnt mn_obj))
;        (count_obj (itoa (car item)) (cdr item)) 
;        (if all_obj 
;          (setq all_obj (sslength all_obj))
;          (setq all_obj 0)
;        ) 
;        (if obj 
;          (setq obj (sslength obj))
;          (setq obj 0)
;        ) 
;        (prompt (strcat "\n\tLayer " (itoa (car item)) ": "))
;        (if (/= all_obj obj)
;          (prompt "WARNING!")
;        )
;        (setq cnt (1+ cnt))
;      ) 
;      (prompt "\n\n\tWhere WARNING indicated, please review!")
;      (getstring "\nPress Enter to continue...") 
      (graphscr) 
      (while (< nxt_layer 39)        ; set the current layer
        (command "_.LAYER" "_M" (itoa nxt_layer) "_OFF" "*" "" "") ; prompt for-
                                     ; user action.
        (initget "Next Exit") 
        (setq mn_nxt (getkword (strcat "\r[<" (itoa nxt_layer) ">" (cdr 
                                                                        (assoc nxt_layer mn_class)
                                                                   ) 
                                     "]\n Proceed (Next/Exit): <Next> "
                               )
                     ))
        (if (= mn_nxt "Exit") 
          (setq nxt_layer 39)
          (setq nxt_layer (1+ nxt_layer))
        )
      ) 
      (while (< nxt_layer 39)
        (command "_.LAYER" "_M" "39" "_ON" "*" "_OFF" 
"1,2,3,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38"
               ""
        ) 
        (initget "Exit") 
        (getkword (strcat "\r[<" (itoa nxt_layer) ">" (cdr (assoc nxt_layer 
                                                                  mn_class
                                                           )
                                                      ) 
                        "]\n Proceed (Exit): <Exit> "
                  )
        ) 
        (setq nxt_layer 39)
      )
    )
  )
  (command "_.UNDO" "_B") 
  (setvar "cmdecho" 1)
  (princ)
) 

