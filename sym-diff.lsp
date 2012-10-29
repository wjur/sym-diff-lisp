(defun diff(expr var)
    (cond ((numberp expr) 0)
        ((symbolp expr)
            (if (eq expr var) 
                1
                0
            )
        ) ; tutaj sprawdzic czy to ta sama zmienna
        ; suma
        ((equal (first expr) '+)
            (list '+ (diff (nth 1 expr) var) (diff (nth 2 expr) var))) 
        ; roznica
        ((equal (first expr) '-)
            (list '- (diff (nth 1 expr) var) (diff (nth 2 expr) var)))
        ; iloczyn (fg)' = f'g + fg'
        ((equal (first expr) '*)
            (list '+  (list '* (nth 2 expr) (diff (nth 1 expr) var))
                (list '* (nth 1 expr) (diff (nth 2 expr) var)))
        )
        ; iloraz (f/g)' = (f'g - fg') / (g*g)
        ((equal (first expr) '/)
            (list '/ (list '-  (list '* (nth 2 expr) (diff (nth 1 expr) var))
                (list '* (nth 1 expr) (diff (nth 2 expr) var)))
                (list '* (nth 2 expr) (nth 2 expr)))
        )
        ; zlozenie funkcji (h(g))' = h'(g) * g'
        ((is_fun (first expr))
            (list '*  (change_fun (first expr) (nth 1 expr)) (diff (nth 1 expr) var) )
        )
        

         
 ; zamienia f(x) na odpowiednie g(x)
 ; np. cos(x) na -sin(x)
(defun change_fun(fun x)
    (cond ((eq fun 'sin) (list 'cos x))
        ((eq fun 'cos) (list '- 0 (list 'sin x)))
     (else
        (error "Not a function!" fun)))
)
         
         
(defun is_fun(fun)
    (cond ((eq fun 'sin) T)
        ((eq fun 'cos) T)
     (else
        (error "Not a function!" fun)))
)
       
