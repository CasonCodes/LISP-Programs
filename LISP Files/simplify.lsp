; Simplify Program  |
; COMP - 4300       |
; ==================|

(defun simplify (term R)
	(let ((newTerm (applyAllRules term R)))
		(if (equal newTerm term)
			term
			(simplify newTerm R))
		)
)

(defun applyAllRules (term allRules)
	( if (null allRules)
		term
		(applyAllRules (applyOneRule term (car allRules)) (cdr allRules))
	)
)

(defun applyOneRule (term rule)
	(if (null term) 
		term
		(let ((sigma (match term (car rule))))
			(cond
				(sigma (applysub sigma (cadr rule)))
				((atom term) term)
				(t (cons (applyOneRule (car term) rule) 
						 (applyOneRule (cdr term) rule)))
			)
		)
	)
)
