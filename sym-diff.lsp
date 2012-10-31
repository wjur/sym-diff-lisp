(defun diff(expr var)
    (cond
        ; a' = 0
        ((numberp expr) 0)
        
        ; x' = 1, y' = 0
        ((symbolp expr)
            (if (eq expr var) 1 0))
            
        ; (f + g)' = f' + g'
        ((equal (first expr) '+) (d_sum expr var))
        
        ; (f - g)' = f' - g'
        ((equal (first expr) '-) (d_sub expr var))
        
        ; (fg)' = f'g + fg'
        ((equal (first expr) '*) (d_mul expr var))
        
        ; (f/g)' = (f'g - fg') / (g*g)
        ((equal (first expr) '/) (d_div expr var))
        
        ; (h(g))' = h'(g) * g'
        ((is_fun (first expr))
            (make_mul  (change_fun (first expr) (nth 1 expr)) (diff (nth 1 expr) var)))
        
        ; (f^g)' = f^(g-1)*(gf' + g'f logf) 
        ((equal (first expr) 'expt)
            (make_comp expr var))
        
        ; (sqrt f)' = (f^(1/2))'
        ((equal (first expr) 'sqrt)
            (diff (list 'expt (nth 1 expr) (make_div 1 2)) var))
        (T
         (error "Unknown expression type - DERIV" expr)))
)

; simplifies a sum
(defun make_sum(left right)
    (cond 
            ((and (equalp left '0) (equalp right '0)) 
                '0
            )
            ((and (equalp left '0) (not (equalp right '0)))
                right
            )
            ((and (equalp right '0) (not (equalp left '0)))
                left
            )
            ((and (numberp left) (numberp right))
                (+ left right)
            )
            ((equalp left right)
                (list '* 2 left)
            )
            (T
             (list '+ left right)))
)

; simplifies a subtraction
(defun make_sub(left right)
    (cond 
            ((and (equalp left '0) (equalp right '0)) 
                '0
            )
            ((and (equalp left '0) (not (equalp right '0)))
                (- right)
            )
            ((and (equalp right '0) (not (equalp left '0)))
                left
            )
            ((and (numberp left) (numberp right))
                (- left right)
            )
            (T
             (list '- left right)))
)

; simplifies a multiplication
(defun make_mul(left right)
    (cond 
            ((or (eq left '0) (eq right '0)) 
                '0
            )
            ((eq left '1)
                right
            )
            ((eq right '1)
                left
            )
            ((and (numberp left) (numberp right))
                (* left right)
            )
            (T
             (list '* left right)))
)

; simplifies a division
(defun make_div(up down)
    (cond 
            ((eq up '0) 
                '0
            )
            ((eq up '1) 
                (/ up down)
            )
            ((eq down '1)
                up
            )
            ((and (numberp up) (numberp down))
                (/ up down)
            )
            ((equalp up down)
                '1
            )
            (T
             (list '/ up down)))
)


; (f + g)' = f' + g'
(defun d_sum(expr var)
    (make_sum (diff (nth 1 expr) var) (diff (nth 2 expr) var))
)

; (f - g)' = f' - g'
(defun d_sub(expr var)
    (make_sub (diff (nth 1 expr) var) (diff (nth 2 expr) var))
)

; (fg)' = f'g + fg'
(defun d_mul(expr var)
    (make_sum (make_mul (nth 2 expr) (diff (nth 1 expr) var))
                (make_mul (nth 1 expr) (diff (nth 2 expr) var)))
)

; (f/g)' = (f'g - fg') / (g*g)
(defun d_div (expr var)
    (make_div (make_sub  (make_mul (nth 2 expr) (diff (nth 1 expr) var))
                (make_mul (nth 1 expr) (diff (nth 2 expr) var)))
                (make_mul (nth 2 expr) (nth 2 expr)))
)

; (f^g)' = f^(g-1)*(gf' + g'f logf)  
(defun make_comp (expr var)
            (make_mul 
                (list 'expt (nth 1 expr) (make_sub (nth 2 expr) 1))
                (make_sum 
                    (make_mul  
                        (nth 2 expr)
                        (diff (nth 1 expr) var)
                    )
                    (make_mul 
                        (make_mul 
                            (diff (nth 2 expr) var)
                            (nth 1 expr)
                        )
                        (list 'log (nth 1 expr))
                    )
                )
            )  
  )     

; changes a function to its derivative  
;  cos(x) na -sin(x)
(defun change_fun(fun x)
    (cond ((eq fun 'sin) (list 'cos x))
        ((eq fun 'cos) (list '- (list 'sin x)))
        ((eq fun 'tan) (list '+ 1.0 (list '* (list 'tan x) (list 'tan x))))
        ((eq fun 'log) (list '/ 1.0  x))
     (T
        (error "Not a function!" fun)))
)
         
; checks if derivative for the function is known         
(defun is_fun(fun)
    (cond ((eq fun 'sin) T)
        ((eq fun 'cos) T)
        ((eq fun 'tan) T)
        ((eq fun 'log) T)
     (T 
        NIL))
)
