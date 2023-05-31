; Cason Kirschner
; COMP 4300
;;;;;;;;;;;;;;;;;;;

(defun unify (term1 term2)
	(unify1 term1 term2 '(nil))	
)


(defun unify1 (term1 term2 sigma) 

	(cond 
		((and (null term1) (null term2)) 
			sigma)
			
		((and (isvar term1)	(doesntoccur term1 term2)) 
			(combine (list (cons term1 term2)) sigma))
			
		((and (isvar term2)	(doesntoccur term2 term1)) 
			(combine (list (cons term2 term1)) sigma))
			
		((and (atom term1) (atom term2))
			(if (eq term1 term2) 
				sigma 
				nil))
			
		((or (atom term1) (atom term2)) 
			nil)
		
		(t (let* ((sub (unify (car term1) (car term2))))
			(cond
				((null sub) 
					nil)
					
				((equal sub '(nil)) 
					(unify1 (cdr term1) (cdr term2) sigma))
					
				(t 
					(unify1 (applysub sub (cdr term1))
						    (applysub sub (cdr term2))
							(combine sub (applysub sub sigma)))
				)				
			)
			)			
		)	
	)
) 


(defun doesntoccur (var term)
	(cond 
		((null term) t)
		((if (and (equal var term) (atom term)) nil t))
		(t (and (doesntoccur var (car term) (doesntoccur var (cdr term)))))
	)
)


;(defun isvar (x)
;     (member x '(u v w x y z))
;)