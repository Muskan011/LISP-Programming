(defun present (lst)
  (cond
    ((null lst) nil) 
    ((equal (car lst) '*) T)
    ((equal (car lst) '!) T)
    (t (present (cdr lst)))
  )
) 
(defun spec-ex (genstr str)
  (cond
    ((null (cdr str)) (helper (cdr genstr) (cdr str)))
    ((equal (car (cdr genstr))  '*)  (helper (cdr genstr) (cdr str)))
    ((equal (car (cdr genstr))  '!)  (spec-ex genstr str))
    ((equal (car (cdr genstr)) (car str)) (helper (cdr genstr) (cdr str)))
    ((equal (car (cdr genstr))  (car (cdr str)))  (helper (cdr genstr) (cdr str)))
    (t (helper genstr (cdr str)))   
  )
)


(defun helper (genstr str)
  (cond
    ((and (null str) (null genstr)) T)
    ((and (null str) (not (null genstr))) nil)
    ((equal (car genstr) '*)  (helper (cdr genstr) (cdr str)))
    ((equal (car genstr) '!) (spec-ex genstr str))
    ((equal (car genstr) (car str))  (helper (cdr genstr) (cdr str)))
    (t nil)
  )
)



(defun match(pattern assertion)
  (cond 
    ((equal pattern assertion) t)
    ((present pattern) (helper pattern assertion))     
    (t nil)
  )
)
