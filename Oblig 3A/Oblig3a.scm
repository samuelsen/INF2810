(load "prekode3a.scm")

;;1 A + B
(define (memoize proc)
  (let ((table (make-table)))
    (lambda x
      (or
       (lookup x table)
       (let ((result (apply proc x)))
         (begin
           (insert! x result table)
           result))))))

(define store-proc (make-table))

(define (mem message proc)
  (cond
   ((eq? message 'memoize)
    (let ((new-proc (memoize proc)))
      (begin (insert! new-proc proc store-proc)
      new-proc)))
   ((eq? message 'unmemoize)
    (lookup proc store-proc)
    )))


;;1c Det første kallet gjøres på mem-fib, funksjonen lagres, men resten kalles bare med den originale fib.

;;1D

(define (greet . args)
  (let ((table (make-table)))
    (insert! 'title "friend" table)
    (insert! 'time "day" table)
    (define (loop elem)
    (if (pair? elem)
     (cond 
       ((eq? (car elem) 'title)
            (insert! 'title (cadr elem) table))
       ((eq? (car elem) 'time)
            (insert! 'time (cadr elem) table))))
      (if (pair? elem)
          (loop (cdr elem))))
    (loop args)
     (display "good ")
     (display (lookup 'time table))
     (display " ")
     (display (lookup 'title table))))
    
;;2A
(define (list-to-stream args)
  (if (null? args)
      the-empty-stream
      (cons-stream (car args) (list-to-stream (cdr args)))))
 
(define (stream-to-list stream . args)
  (define (loop strom num)
    (cond ((or (stream-null? strom) (zero? num))
       '())
      ((= -1 num)
       (cons (stream-car strom) (loop (stream-cdr strom) -1)))
      (else
       (cons (stream-car strom) (loop (stream-cdr strom) (- num 1))))))
  (if (null? args)
      (loop stream  -1)
      (loop stream  (car args))))



;;2b
(define (check-null? stream-list)
  (cond ((null? stream-list) #f)
    ((stream-null? (car stream-list)) #t)
    (else (check-null? (cdr stream-list)))))


(define (stream-map proc . argstreams)
  (if (check-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
          (cons proc (map stream-cdr argstreams))))))
   

;;2C
;;kortsvar fordi denne vil lete i alle evigheter, siden strømmer kan være uendelig lange. 
;;Ettersom han begynner forfra og sjekker om det første elementet forekommer senere i strømmen,
;;vil han bli nøtt til å søke gjennom en uendelig lang strøm.

;;2D
(define (remove-duplicates stream)
  (define (not? elem)
    (not (eq? (stream-car stream) elem)))
     (if (stream-null? stream)
         the-empty-stream
         (cons-stream (stream-car stream)
                      (stream-filter not?
                                     (remove-duplicates (stream-cdr stream))))))
;;2E
;; Når vi definerer x får vi 0 som er første element i strømmen fra 0 - 10 ettersom show skal vise elementet som er gitt inn.
;; Når vi gjør andre kallet beregnes verdiene fremtil vi kommer til 5 og funksjonen show viser elementene som vi er innom til vi kommer til det vi leter etter.
;; Siste evaluering gir oss 6 og 7 fra show og 7 som verdi ettersom vi allerede har fått elementene opp til og med 5 blir ikke disse beregnet på nytt av strømmen.

;;2F
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 
                    1
                    (mul-streams factorials nats)))