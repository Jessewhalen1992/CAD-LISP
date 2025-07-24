
(setq org_BDP C:BDP)
(defun C:BDP ()(if (l+getUTMstatus nil)(C:UTM))(org_BDP))

