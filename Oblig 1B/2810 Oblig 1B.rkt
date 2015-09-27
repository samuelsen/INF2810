;; Oppgave 1
;; F
(define foo(list 1 2 3 4))
(car (cdr (cdr foo)))

;;G
(define foo2 (list (list 1 2) (list 3 4)))
(car (car (cdr foo2)))

;; H
(define foo3 (list (list 1) (list 2) (list 3) (list 4)))
(car (car (cdr (cdr foo3))))

;; I
;; Ved bruk av list
(list (list 1 2) (list 3 4))

;; Ved bruk av cons
(cons (cons 1 (cons 2 '()))(cons (cons 3 (cons 4 '())) '()))


;; Oppgave 2
;; A
(define(length2 items)
  (define (len items x)
    (if(null? items)
     x
     (len (cdr items) (+ 1 x))))
  (len items 0))


;; B
(define (rev-list items)
  (define(rev-list1 items new)
  (if (null? items)
      new
      (rev-list1 (cdr items) (cons (car items) new))
      ))
  (rev-list1 items '())
  )
(rev-list foo)

;; Denne er hale rekursiv, dette kan vi sjekke mot debuggeren hvor man ser
;; at stacken ikke bygger seg opp underveis, den nye listen blir lagd ettersom vi går gjennom elementene.
;; Har valgt halerekursjon for å spare minne, samt at man da cons-er det neste elementet med på listen som er lagd
;; Dermed blir elementet lagt til fortløpende i listen reversert.

;;C
(define (ditch x items)
  (if (null? items)
      items
      (if(= x (car items))
         (ditch x (cdr items))
         (cons (car items) (ditch x (cdr items))))))
  
(ditch 3 foo)

;; Denne er løst med vanlig rekursjon, dette gjør at minne bruken bygges opp 
;; ettersom den holder på alle verdiene den skal legge sammen til den har gått 
;; gjennom alle elementene i listen som er gitt som argument og setter dem sammen til 
;; ny liste på vei tilbake.

;;D
(define (nth x items)
  (if(= 0 x)
     (car items)
     (nth (- x 1) (cdr items))))

(nth 3 foo)

;;E
(define (where x items)
  (define (count items y)
  (if (null? items)
      #f
      (if (= x (car items))
          y
          (count (cdr items) (+ 1 y)))))
  (count items 0))
(where 88 foo)

;;F
(define (map2 proc items1 items2)
  (cond
    ((null? items1) '())
    ((null? items2) '())
    (else (cons (proc (car items1) (car items2))
          (map2 proc (cdr items1) (cdr items2))))))

(map2 + '(1 5 3 4) '(3 4 5))
                    
;;G
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))

(map2 (lambda (x y) (and (even? x)(even? y))) '(1 2 3 4) '(3 4 5)) 

;;H
(define (both? proc)
  (lambda (x y)
    (and (proc x)(proc y))
    ))

(map2 (both? +) '(1 2 3) '(3 4 5))

;;I
(define (self proc)
  (lambda (x) (proc x x)))

((self list) "hello")