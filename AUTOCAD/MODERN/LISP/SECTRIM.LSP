(vl-load-com)

(defun c:SECTRIM  (/ sel ent insPt cx cy cz center circle ss i ln ed p1 p2
                     d1 d2 dx dy Xd Yd A B C D sqrtD t1 t2 inter1 inter2 props)
  ;; ─── PARAMETERS ───
  (setq R   35.0     ; circle radius
        OFF 5.0)     ; shift south by 5 units in Y

  ;; ─── 1) Pick a block INSERT ───
  (prompt "\nSelect block to trim around: ")
  (setq sel (entsel))
  (if (not
        (and sel
             (= "INSERT" (cdr (assoc 0 (entget (car sel)))))))
      (progn
        (princ "\n** Cancelled or not a block INSERT **")
        (exit)))

  ;; ─── 2) Compute circle center 5 m south of block insertion ───
  (setq ent   (car sel)
        insPt (cdr (assoc 10 (entget ent)))
        cx    (car insPt)
        cy    (cadr insPt)
        cz    (caddr insPt)
        center (list cx (- cy OFF) cz))

  ;; ─── 3) Draw helper circle on current layer ───
  (setq circle
        (entmakex
          (list
            (cons 0  "CIRCLE")
            (cons 10 center)
            (cons 40 R)
            (cons 8  (getvar "CLAYER")))))

  ;; ─── 4) Collect all LINEs in the circle's bounding box ───
  (setq ss
        (ssget "C"
               (list (- (car center) R) (- (cadr center) R) cz)
               (list (+ (car center) R) (+ (cadr center) R) cz)
               '((0 . "LINE"))))

  ;; ─── 5) Delete or trim each LINE ───
  (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ln  (ssname ss i)
                ed  (entget ln)
                p1  (cdr (assoc 10 ed))
                p2  (cdr (assoc 11 ed))
                d1  (distance (list (car p1) (cadr p1)) (list (car center) (cadr center)))
                d2  (distance (list (car p2) (cadr p2)) (list (car center) (cadr center))))

          (cond
            ;; 5a) Both endpoints inside → delete the line
            ((and (<= d1 R) (<= d2 R))
             (entdel ln))

            ;; 5b) One inside, one outside → trim at single intersection
            ((or (< d1 R) (< d2 R))
             (setq dx     (- (car p2) (car p1))
                   dy     (- (cadr p2) (cadr p1))
                   Xd     (- (car p1) (car center))
                   Yd     (- (cadr p1) (cadr center))
                   A      (+ (* dx dx) (* dy dy))
                   B      (* 2 (+ (* Xd dx) (* Yd dy)))
                   C      (- (+ (* Xd Xd) (* Yd Yd)) (* R R))
                   D      (- (* B B) (* 4 A C))
                   sqrtD  (if (> D 0) (sqrt D) 0)
                   ;; root between 0 and 1
                   t1     (/ (+ (- B) (- sqrtD)) (* 2 A))
                   t1     (cond ((< t1 0) 0) ((> t1 1) 1) (t1))
                   inter1 (list (+ (car p1) (* t1 dx))
                                (+ (cadr p1) (* t1 dy))
                                cz))
             (if (< d1 R)
                 (entmod (subst (cons 10 inter1) (assoc 10 ed) ed))
                 (entmod (subst (cons 11 inter1) (assoc 11 ed) ed))))

            ;; 5c) Both outside but crossing → split at two intersections
            ((and (> d1 R) (> d2 R))
             (setq dx     (- (car p2) (car p1))
                   dy     (- (cadr p2) (cadr p1))
                   Xd     (- (car p1) (car center))
                   Yd     (- (cadr p1) (cadr center))
                   A      (+ (* dx dx) (* dy dy))
                   B      (* 2 (+ (* Xd dx) (* Yd dy)))
                   C      (- (+ (* Xd Xd) (* Yd Yd)) (* R R))
                   D      (- (* B B) (* 4 A C)))
             (if (> D 0)
               (progn
                 (setq sqrtD (sqrt D)
                       t1     (/ (+ (- B)   (- sqrtD)) (* 2 A))
                       t2     (/ (+ (- B)     sqrtD) (* 2 A))
                       t1     (cond ((< t1 0) 0) ((> t1 1) 1) (t1))
                       t2     (cond ((< t2 0) 0) ((> t2 1) 1) (t2))
                       inter1 (list (+ (car p1) (* t1 dx))
                                    (+ (cadr p1) (* t1 dy))
                                    cz)
                       inter2 (list (+ (car p1) (* t2 dx))
                                    (+ (cadr p1) (* t2 dy))
                                    cz)
                       props  (vl-remove-if 'null
                                 (mapcar
                                   (function (lambda (k) (assoc k ed)))
                                   '(8 6 62 370 48))))
                 (entdel ln)
                 (entmakex (append (list
                                      (cons 0 "LINE")
                                      (cons 10 p1)
                                      (cons 11 inter1))
                                    props))
                 (entmakex (append (list
                                      (cons 0 "LINE")
                                      (cons 10 inter2)
                                      (cons 11 p2))
                                    props))))))

          (setq i (1+ i)))
        ))  ; end if ss

  ;; ─── 6) Erase helper circle ───
  (entdel circle)

  (princ "\n→ Trim complete.")
  (princ)
)  ; end defun