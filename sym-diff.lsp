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
            (d_sum expr var)
        )
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
        
        ; (f^g)' = f^(g-1)*(gf' + g'f logf) 
        ((equal (first expr) 'expt)
            (list '* 
                (list 'expt (nth 1 expr) (list '- (nth 2 expr) 1))
                (list '+ 
                    (list '* 
                        (nth 2 expr)
                        (diff (nth 1 expr) var)
                    )
                    (list '* 
                        (list '* 
                            (diff (nth 2 expr) var)
                            (nth 1 expr)
                        )
                        (list 'log (nth 1 expr))
                    )
                )
            )
        )
        ; pierwiatek, z wykorzystaniem expr
        ((equal (first expr) 'sqrt)
            (diff (list 'expt (nth 1 expr) (list '/ 1 2)) var)
        )
        (T
         (error "Unknown expression type - DERIV" expr)))
)

(defun make_sum(left right)
    (cond 
            ((and (eq left '0) (eq right '0)) 
                '0
            )
            ((and (eq left '0) (not (eq right '0)))
                right
            )
            ((and (eq right '0) (not (eq left '0)))
                left
            )
            ((and (numberp left) (numberp right))
                (+ left right)
            )
            (T
             (list '+ left right)))
)

(defun make_sub(left right)
    (cond 
            ((and (eq left '0) (eq right '0)) 
                '0
            )
            ((and (eq left '0) (not (eq right '0)))
                right
            )
            ((and (eq right '0) (not (eq left '0)))
                left
            )
            ((and (numberp left) (numberp right))
                (- left right)
            )
            (T
             (list '- left right)))
)
; pochodna sumy
; upraszcza wyrazenie, pomija zera i sumuje cyfry tam gdzie mozna
(defun d_sum(expr var)
    (make_sum (diff (nth 1 expr) var) (diff (nth 2 expr) var))
)

(defun d_sub(expr var)
    (make_sub (diff (nth 1 expr) var) (diff (nth 2 expr) var))
)         
 ; zamienia f(x) na odpowiednie g(x)
 ; np. cos(x) na -sin(x)
(defun change_fun(fun x)
    (cond ((eq fun 'sin) (list 'cos x))
        ((eq fun 'cos) (list '- 0 (list 'sin x)))
        ((eq fun 'tan) (list '+ 1 (list '* (list 'tan x) (list 'tan x))))
        ((eq fun 'log) (list '/ 1  x))
     (T
        (error "Not a function!" fun)))
)
         
         
(defun is_fun(fun)
    (cond ((eq fun 'sin) T)
        ((eq fun 'cos) T)
        ((eq fun 'tan) T)
        ((eq fun 'log) T)
     (T 
        NIL))
)   
