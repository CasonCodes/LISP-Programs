; Cason Kirschner

(defun lat? (x)
	(if (null x)
		t
		(if (and (listp x) (atom (car x)))
			(lat? (cdr x))
			nil
		)
	)
)