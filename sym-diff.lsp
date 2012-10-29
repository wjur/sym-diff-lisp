(defun diff(expr var)
    (cond ((numberp expr) 0)
        ((symbolp expr) 1) ; tutaj sprawdzic czy to ta sama zmienna
        ; suma
        ((equal (first expr) '+)
            (+ (diff (nth 1 expr) var) (diff (nth 2 expr) var))) 
        ; roznica
        ((equal (first expr) '-)
            (- (diff (nth 1 expr) var) (diff (nth 2 expr) var)))
        ; iloczyn (fg)' = f'g + fg'
        ((equal (first expr) '*)
            (list '+  (list '* (nth 2 expr) (diff (nth 1 expr) var))
                (list '* (nth 1 expr) (diff (nth 2 expr) var)))
        )
        (else
         (error "Unknown expression type - DERIV" expr))))


       
