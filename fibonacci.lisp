(defun nth-fib (n)
  (if (<= n 1)
      n
      (+ (nth-fib (- n 1)) (nth-fib (- n 2)))
  )
)

(defun f-helper (prev2 prev1 num lst)
  (if (<= num 0) (reverse lst)
      (f-helper prev1 (+ prev1 prev2) (- num 1) (cons prev2 lst))
  )
)

(defun fib (n)
  (f-helper 0 1 n NIL)
)


(defun lt (prev2 prev1 num lst)
  (if (>= prev2 num) (reverse lst)
      (lt prev1 (+ prev1 prev2) num (cons prev2 lst))
  )
)

(defun fib-lt (n)
  ( lt 0 1 n NIL)
)

