; calculates diverative of expression expr
; example: (diff '(+ (* a x) b) 'x)
; OUT: a
; (diff '(sqrt (sqrt x)) 'x)
; (diff '(expt a x) 'x)
; (diff '(expt x a) 'x)
; (diff '(exp x) 'x)

(defun diff(expr var)
		(cond
			; a' = 0
			((numberp expr) 0)
			
			; x' = 1, y' = 0
			((symbolp expr)
				(if (eq expr var) 1 0))
				
			((equal (first expr) '+) `(+ ,(diff (second expr) var) ,(diff (third expr) var)))
			(T
			 (error "Unknown expression type - DERIV" expr)))
)




(defmacro ifnotnull (listToTest valueToReturnOtherwise &body body)
  `(if(null ,listToTest)
       ,valueToReturnOtherwise
       ,@body
       )
)


(defmacro calcValue (W x)
    `(
        ifnotnull ,W
            0
            (
                + (first ,W) (* ,x (calcValue (rest ,W) ,x))
            )
    )
)


(defmacro avg (realCount &body body)
  `( / (+ ,@body) ,realCount )
)


(defmacro BisectionMethod (W a b N)
    `(
        if( eq ,N 0)
            (list ,a ,b)

        (
            if(> ( *  ( calcValue ,W ,a)  ( calcValue ,W ,b) ) 0)
                        nil
        (
        if(< ( abs ( calcValue ,W ( avg 2.0 ,a ,b )) ) 0.0001)
            ( / (+ ,a ,b) 2.0 )
            (
                if( = (signum ( calcValue ,W ,a)) (signum ( calcValue ,W ( avg 2.0 ,a ,b ))))
                    (BisectionMethod ,W ( avg 2.0 ,a ,b ) ,b (- ,N 1))
                    (BisectionMethod ,W ,a ( avg 2.0 ,a ,b ) (- ,N 1))
            )
            )

        )

    )
)
