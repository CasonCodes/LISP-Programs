


; *******************************************************

; NAME: Cason Kirschner
; PROGRAM: Deductive Database
; COURSE: COMP 4300 - A.I.

; Description:
; -------------------------------------------
; My program is able to lookup facts within 
; the DB and "deduce" whether a certain 
; query is true or not. As it is written,
; (and I'm writing this description early),
; this program can handle queries such as:

; 	(? '(dog lassie)) 
; 	(? '(dog fido)) 
; 	(? '(man socrates)) 
; 	(? '(man plato)) 
; 	(? '(cat felix)) 
; 	(? '(student john)) 
; 	(? '(student mary))

; The basic approach is to detect whether a
; a query can be satisfied with a simple
; fact lookup first. It does this by testing
; whether the caar of the DB is an atom.

; If it is an atom, and also if the query is 
; equal to the cadar of the DB, the fact is 
; then added to a global list of facts for 
; later use and the program returns true for the query.

; The other possibility  while traversing the db
; is that, instead of coming across an 
; ['atom and a '(list)]
; the program comes across a 
; ['(list) and a '(list)]
; and whether either of those lists contain variables.

; *******************************************************


; --------- [EXAMPLE TARGET] -------------------

;		(CAR) (CDR)
;        ---  ------
; 	(? '(dog  lassie))

; ---------- [EXAMPLE DB] ----------------------

;			  (CAR)
;        ------------------
;	    (CAAR)     (CADAR)	
;        ----	   --------
; 	'(  (T        (DOG FIDO)),
;		(T        (DOG LASSIE)), 
;		((dog x1) (mammal x1)), 
;		(...),              	  )
	
; ----------------------------------------------

; '(   nil    ) empty
	
; '(  *   ()  ) atom and list
	
; '(  ()  ()  ) list and list
	
; ----------------------------------------------

; GLOBAL VARIABLES
(setq db '())
(setq allFacts '())

(defun ? (query)	
	(if (deduce query db) t 'nil)
)

(defun deduce (target lst)

	; may not need - no use yet
	(setq predicate (car target))
	(setq entity (cdr target))
	
	(cond	
		; if lst is empty, not found
		((null lst) 
			'nil
		)	
	
		; if head of lst is ATOM & tail of lst is LIST -> fact found
		((and (atom (caar lst)) (listp (cdar lst)))
		
			; if its the fact were looking for
			(if (equal target (cadar lst)) 
				t 
				
				; otherwise, if the predicate matches, but entity does not
				;(if (equal (caadar lst) predicate)
				
					; then that entity (not the target entity) satisfies the predicate
					; and we can collect all the entities together by their predicate
					; for faster searches.
					
					; (addFact predicate (caadar lst)) ; not done <--
					
					(deduce target (cdr lst)) ; continue search
				;)
			)
		)		
		
		; if head of lst is LIST & tail of lst is LIST
		((and (listp (caar lst)) (listp (cdar lst)))
			; check for variables ; not done <--
			(deduce target (cdr lst)) ; continue search
		)	
	)
)










(defun addFact (predicate entity)
	; not done <--
)

(defun ISVAR (x)
     (member x '(u v w x y z x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
					x11 x12 x13 x14 x15 x16 x17 x18 x19 x20))
)

(defun MYREPLACE (old new target)
	(cond 	
		((null target) 
			nil) 
			
		((and(atom target)(eq old target)) 
			new) 
			
		((and(atom target)(not(eq old target))) 	
			target) 
			
		((listp (car target)) 
			(cons(MYREPLACE old new (car target)) (MYREPLACE old new (cdr target)))) 
			
		((eq old (car target))  
			(cons new (MYREPLACE old new (cdr target)))) 
			
		(t 		
			(cons (car target) (MYREPLACE old new (cdr target)))) 
	)	
)

