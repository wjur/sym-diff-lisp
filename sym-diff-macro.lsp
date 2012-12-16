; calculates diverative of expression expr
; example: (diff '(+ (* a x) b) 'x)
; OUT: a
; (diff '(sqrt (sqrt x)) 'x)
; (diff '(expt a x) 'x)
; (diff '(expt x a) 'x)
; (diff '(exp x) 'x)

; changes a function to its derivative  
; f.i. cos(x) na -sin(x)
(defmacro change_fun(fun x)
    (cond ((eq fun 'sin) `(list 'cos (quote ,x)))
        ((eq fun 'cos) `(list '- ('sin (quote ,x))))
        ((eq fun 'tan) `(list '+ '1.0 (list '* ('tan (quote ,x)) ('tan (quote ,x)))))
        ((eq fun 'log) `(list '/ '1.0  (quote ,x)))
		((eq fun 'exp) `(list 'exp (quote ,x)))
     (T
        (error "Not a function!" fun)))
)
         
(defmacro diff(expr var)
		(pprint "diff")
		(pprint  expr)
		(pprint var)
		;(pprint (first expr))
		;(pprint (second expr))
		;(pprint (third expr))
		(cond
			; a' = 0
			((numberp expr) 0)
			
			; x' = 1, y' = 0
			((symbolp expr)
				(if (eq expr var) 1 0))
			
			; sum
			;((equal (first expr) '+) `(+ `(diff ,(second expr) var) `(diff ,(third ,expr) ,var)))
			((equal (first expr) '+) `(list '+ (diff ,(second expr) ,var) (diff ,(third expr) ,var)))
			
			; (f - g)' = f' - g'
			((equal (first expr) '-) `(list '- (diff ,(second expr) ,var) (diff ,(third expr) ,var)))
        
			; (fg)' = f'g + fg'	
			((equal (first expr) '*) `(list '+ (list '* (diff ,(second expr) ,var) (quote ,(third expr))) (list '* (diff ,(third expr) ,var) (quote ,(second expr)))))
        
			; (f/g)' = (f'g - fg') / (g*g)
			((equal (first expr) '/) `(list '/  (list '- (list '* (diff ,(second expr) ,var) (quote ,(third expr)) ) (list '* (diff ,(third expr) ,var) (quote ,(second expr))) ) (list '* (quote ,(third expr)) (quote ,(third expr)))))
			
			((equal (first expr) 'sin) `(list '* (list 'cos (quote ,(second expr))) (diff ,(second expr) ,var) ))
			((equal (first expr) 'cos) `(list '* (list '- 'sin (quote ,(second expr))) (diff ,(second expr) ,var) ))
			((equal (first expr) 'log) `(list '* (list '/ '1.0 (quote ,(second expr))) (diff ,(second expr) ,var) ))
			
			(T
			 (error "Unknown expression type - DERIV" expr)))
)
