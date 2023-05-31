; Cason Kirschner   |
; Match Function    |
; COMP - 4300       |
; ==================|

; Test Input:
; ---------------------------------------------------------------------
; (MATCH '(+ a b) '(+ a x)) => ((x . b))					; pass

; (MATCH '(* a b) '(* a b)) => (nil)						; pass

; (MATCH '(f a b) '(f a a)) => nil							; pass

; (MATCH '(+ a b) '(- a b)) => nil							; pass

; (MATCH '(+ (- b c) a) '(+ x y)) => ((x - b c) (y . a))		; pass

; (MATCH '(loves a b) '(loves x x)) => nil						; pass

; (MATCH '(loves joe pie) '(loves x pie)) => ((x . joe))		; pass
 
; (MATCH '(+ a (+ b a)) '(+ x (+ y x))) => ((x . a) (y . b))	; fail (does not consider outer substitution)



(defun match1 (term pattern sub)
	(cond				
		; if both term and pattern are null, return sub
		((and (null term) (null pattern)) sub)
		
		; if either term or pattern are null, return nil
		((or (null term) (null pattern)) nil)
		
		; if nested lists found, start new list and append to existing sub, then continue on
		((and (listp (car term)) (listp (car pattern))) 
			(match1 (cdr term) (cdr pattern) (append sub (match (car term) (car pattern)))))			
		
		; if the pattern is a variable and can be matched
		((isVar (car pattern)) 
			
			(cond 
				; if pattern has already been matched, ignore
				((alreadyMatched (car pattern) sub) nil)
			
				; if sub is an empty list, don't append matched term
				((equal sub '(nil)) (match1 (cdr term) (cdr pattern) (list (cons (car pattern) (car term)))))
				
				; if sub is not an empty list, append matched term to existing sub
				(t (match1 (cdr term) (cdr pattern) (append sub (list (cons (car pattern) (car term))))))
			)
		)
		
		; if heads of term and pettern are equal, ignore
		((eq (car term) (car pattern)) (match1 (cdr term) (cdr pattern) sub))
		
		; if either term or pattern is an atom, return nil
		((or (atom term) (atom pattern)) nil)
	)
)

(defun match (term pattern)
	(match1 term pattern '(nil))
	;(deleteDuplicates (match1 term pattern '(nil))) ; not working
)

; deletes all duplicate matched variables
;(defun deleteDuplicates (lst)
;	(cond 
;		((not (null lst))
;			(setq target (caar lst))
;			(lookFor target (cdr lst))
;			
;		)
;	)
;)

;(defun lookFor (target lst)
;	(if (eq target (caar lst))
;		(cdr lst)
;	)
;)

; returns true if argument is a specified variable character
(defun isVar (input)
	(cond
		((eq input 'u) t)
		((eq input 'v) t)
		((eq input 'w) t)
		((eq input 'x) t)
		((eq input 'y) t)
		((eq input 'z) t)
		(t 'nil)
	)
)

(defun alreadyMatched (item lst)
	(cond 
		((null lst) nil)
		((eq item (caar lst)) t)
		(t (alreadyMatched item (cdr lst)))
	)
	
)

; replaces all occurrences of old within target with new
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