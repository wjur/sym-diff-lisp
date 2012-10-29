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
        
        (else
         (error "Unknown expression type - DERIV" expr))))


       
