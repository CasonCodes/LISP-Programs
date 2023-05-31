; Cason Kirschner

(defun lookup (item lst)
	(if (null lst)
		nil
		(if (equal (caar lst) item)
			(cadar lst)
			(lookup item (cdr lst))
		)
	)
)