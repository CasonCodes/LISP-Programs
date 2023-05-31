; Cason Kirschner

; replaces all occurrences of 
; old within target with new

(defun myreplace (old new target)
	(cond 	
		((null target) 
			nil) 
			
		((and(atom target)(eq old target)) 
			new) 
			
		((and(atom target)(not(eq old target))) 	
			target) 
			
		((listp (car target)) 
			(cons(myreplace old new (car target)) (myreplace old new (cdr target)))) 
			
		((eq old (car target))  
			(cons new (myreplace old new (cdr target)))) 
			
		(t 		
			(cons (car target) (myreplace old new (cdr target)))) 
	)	
)