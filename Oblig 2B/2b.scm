;; Oppgave 1A
(define counter 42)

(define (make-counter)
  (let ((counter 0))
  (lambda () 
    (set! counter (+ counter 1))
    counter)))

(define count1 (make-counter))
(define count2 (make-counter))

(count1)
(count1)

(count2)

(define counter 24)

(count1)

;;2A
(define (make-stack list)
  (let ((stack list))
  (define (push-iter! elem)
    (if (pair? elem)
        (begin (set! stack (cons (car elem) stack))
               (push-iter! (cdr elem)))))
    (lambda (message . elements)
      (cond
       ((eq? 'pop! message)
        (if (null? stack)
            (set! stack '())
            (set! stack (cdr stack))))
       ((eq? 'stack message)
        stack)
       ((eq? 'push! message)
        (push-iter! elements))))))


'-------------2A-------------- 
(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

'-------------2B-------------- 
;;2B
(define (pop! stack)
  (stack 'pop!))

(define (stack stck)
 (stck 'stack))

(define (push! stck . elements)
  (define (stackpusher elem)
  (if (pair? elem)
      (begin (stck 'push! (car elem))
         (stackpusher (cdr elem)))))
  (stackpusher elements))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)

'-------------3A--------------
;;se vadlagt bilde
;;vi setter cdr av  cdddr av listen til å peke på det samme som cdr av selve listen peker på. 
;;Dermed får vi en sirkulær struktur, og når vi går *n* steg i listen  med (list-ref n) kommer 
;;vi til element *n mod listelengden*.

'-------------3B--------------
;;se vedlagt bilde
;;Her blir car av bar satt til å peke på det samme som cdr av bar.  
;;Når vi så endrer hva som ligger i det som car av car av bar peker på, vil det være det samme 
;;som å endre det car av cdr av bar peker på.

'-------------3C--------------
(define (cycle? list)
  (define (cycle-checker tortoise hare)
    (cond ((null? hare) #f)
          ((null? (cdr hare)) #f)
      ((null? (cddr hare)) #f)
      ((eq? tortoise hare) #t)
          (else (cycle-checker (cdr tortoise)(cddr hare)))))
  (cycle-checker list (cdr list)))

'-------------3D--------------
;;bar blir evaulert til false ettersom den er syklisk/aldr kommer til den '() som er enden på en liste.
;;Bah blir evaluert til true ettersom vi her (selv om vi sette opp en sykel) vil kunne komme til den '()


'-------------3E--------------
     
;;3e
(define (make-ring inputList)
  (define (loop list)
    (if (null? (cddr list))
        (set-cdr! (cdr list) inputList)
        (loop (cdr list)))
    inputList)
  (define (loop-circle circle element)
    (if (eq? element (cdr circle))
        circle
        (loop-circle (cdr circle) element)))
  (let ((ring (loop inputList)))
    (define (make-loop inputList)
  (define (loop list)
    (if (null? (cddr list))
        (set-cdr! (cdr list) inputList)
        (loop (cdr list)))
    inputList)
  (loop inputList))
    (lambda (message . elements)
      (cond ((eq? 'top message)
             (car ring))
            ((eq? 'left-rotate! message)
             (begin (set! ring (cdr ring)) (car ring)))
            ((eq? 'right-rotate message)
             (begin (set! ring (loop-circle ring ring))
                    (car ring)))
            ((eq? 'delete! message)
             (begin (set-cdr! (loop-circle ring ring) (cdr ring))
                    (set! ring (cdr ring))
                    (car ring)))
            ((eq? 'insert message)
             (begin (set-cdr! (loop-circle ring ring) '())
                    (set! ring (make-loop (cons (car elements) ring)))
             ))))))
      
      
