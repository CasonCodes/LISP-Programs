; Cason Kirschner

(defun calc (x)
	(cond
		((atom x)    
			x
		)
		(t (eval(list(cadr x) 		 ; operator
					 (calc(car x))	 ; operand1
					 (calc(caddr x)) ; operand2
				)
			)
		)
	)
	
	
	
	
	;(eval(list(cadr x)(car x)(caddr x)))
	;			  +		 2		 

)