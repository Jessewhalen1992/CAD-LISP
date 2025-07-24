(defun c:dimmask (/ ent entdata hnd i newentdata ss xdata)
  (if (setq ss (ssget '((0 . "Dimension"))))
    (repeat (setq i (sslength ss))
      (setq hnd	(ssname ss (setq i (1- i)))
	    ent	(entget hnd '("ACAD"))
      )
      (if (or (not (setq xdata (assoc -3 ent)))
	      (and (setq xdata (assoc -3 ent))
		   (member '(1000 . "DSTYLE") (last xdata))
		   (/= (cdr (assoc 1070 (reverse (last xdata)))) 1)
		   (setq ent (vl-remove-if '(lambda (x) (= -3 (car x))) ent))
	      );; and
	  );; or
	(setq entdata '((-3 ("ACAD" (1000 . "DSTYLE")(1002 . "{")(1070 . 69)(1070 . 1)(1002 . "}"))))
	      newentdata (append ent entdata))
	(setq ent (vl-remove-if '(lambda (x) (= -3 (car x))) ent)
	      entdata '((-3 ("ACAD"(1000 . "DSTYLE")(1002 . "{")(1070 . 69)(1070 . 0)(1002 . "}"))))
	      newentdata (append ent entdata))
	);; if
      (entmod newentdata)
    );; repeat
  );; if
  (princ)
);; dimmask