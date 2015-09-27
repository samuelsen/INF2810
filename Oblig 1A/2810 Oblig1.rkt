;;Oppgave2
;;2b
(define (sign x)
  (if (> x 0)
      1
  (if (< x 0)
      -1
      0)))
(define (sign1 x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        ((= x 0) 0)))
;;2C
(define (sign2 x)
  (or(and (> x 0) 1)
  (and (< x 0 ) -1)
  (and (zero? x) 0)))

;;Oppgave 3
;;Function for adding 1
(define (add1 x)
    (+ x 1))
;;function for minus 1
(define(sub1 x)
  (- x 1))
;;function for adding two numbers
(define (pluss x y)
  (if (zero? y)
      x
      (pluss (add1 x) (sub1 y))      
      )
  )
;;Function for adding two numbers 2
(define (pluss2 x y)
  (if (> y 0)
      (add1 (pluss2 x (sub1 ypl)))
      x
      )
  )
;;3D
(define (power-close-to b n)
  (define (power-iter e)
     (if (> (expt b e) n)
          e
         (power-iter (+ 1 e))))
     (power-iter 1))
   