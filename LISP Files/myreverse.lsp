; Cason Kirschner

(defun myreverse (x)
	(cond
	; if empty list
		((null x) '())
	; if head of list is atom
		((atom(car x))  (append (myreverse(cdr x)) (list (car x))))
	; if head of list is a list
		((listp(car x)) (append (myreverse(cdr x)) (list(myreverse(car x)))))
	)
)