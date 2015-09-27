(load "evaluator.scm")
(set! the-global-environment (setup-environment))
;;Oppgave 2b (lagt til her slik at man kan kjøre read-eval-print-loop på det editerte eksemplet vårt.
;;------------b----------------------
(define (install-primitive! proc expr)
  (set-car! (car the-global-environment)
        (append (caar the-global-environment) (list proc)))
  (set-cdr! (car the-global-environment)
        (append (cdar the-global-environment) (list (list 'primitive expr)))))

;;test av install-primitive
(install-primitive! 'square (lambda (x) (* x x)))

;;Start read eval print loop
(read-eval-print-loop)
;;---------Oppgave 1---------------
;;(foo 2 square) evalueres til 0 (foo 4 square) evalueres til 4
;; Dette er for det vi definerer en metode på formen (foo cond else) hvor de lokale variablene 
;; overstyrer de globale variablene. si i disse tilfellene blir cond henholdsvis 2 og 4 og else blir funksjonen square.
;; siste uttrykket vi evaluerer ekskluderer ikke de globale variablene, og her er cons 3 og else definert til x / 2
;; det siste uttrykket evalueres til 2 da det blir gjort 2 / 2

;;---------Oppgave 2----------------
;;------------a---------------------
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
;;      her kan vi legge til flere primitiver.
        (list '1+ 
	      (lambda (x) (+ x 1)))
	(list '1-
	      (lambda (x) (- x 1)))
        ))
;;------------b----------------------
(define (install-primitive! proc expr)
  (set-car! (car the-global-environment)
        (append (caar the-global-environment) (list proc)))
  (set-cdr! (car the-global-environment)
        (append (cdar the-global-environment) (list (list 'primitive expr)))))
;;test av install-primitive
(install-primitive! 'square (lambda (x) (* x x)))

;;----------Oppgave 3-------------------
;;----------a---------------------------
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if-then exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
	((or? exp) (eval-or exp env)) ;;Lagt til
	((and? exp) (eval-and exp env))))

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((or? exp) #t)  ;; Lagt til
        ((and? exp) #t))) ;; Lagt til

(define (eval-or exp env)
  (cond ((null? (next-predicate exp)) #f)
	((false? (mc-eval (next-predicate exp) env)) (eval-or (cdr exp) env))
	(else  #t)))

(define (eval-and exp env)
  (cond ((null? (next-predicate exp)) #t)
	((true? (mc-eval (next-predicate exp) env)) (eval-and (cdr exp) env))
	(else  #f)))

(define (next-predicate exp) (cadr exp))

(define (or? exp) (tagged-list? exp 'or))
(define (and? exp) (tagged-list? exp 'and))

;;----------------b---------------------
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if-then exp env)) ;;endret til å bruke den nye funksjonen
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((or? exp) (eval-or exp env)) ;;Lagt til
        ((and? exp) (eval-and exp env)) ;;Lagt til
        ((let? exp) (mc-eval (let->lambda exp) env)))) ;;Lagt til

(define (eval-if-then exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-then-cons exp) env)
      (if (eq? 'elsif (if-next-elem exp))
          (eval-if-then (if-rest-elem exp) env)
          (mc-eval (if-else-elem exp) env))))
      
      
      
 (define (if-then-cons exp)(cadddr exp))
 (define (if-next-elem exp)(cadr (cdddr exp)))
 (define (if-rest-elem exp)(cdr (cdddr exp)))
 (define (if-else-elem exp)(caddr (cdddr exp)))


;;----------------------c--------------------------
 (define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if-then exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((or? exp) (eval-or exp env)) 
        ((and? exp) (eval-and exp env))
        ((let? exp) (mc-eval (let->lambda exp) env)))) ;;Lagt til

       

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((or? exp) #t)  
        ((and? exp) #t) 
        ((let? exp) #t) ;; Lagt til
        (else #f)))

(define (let->lambda exp)
  (cons (make-lambda (get-var-list (cadr exp)) (get-body exp)) (get-exp-list (cadr exp))))


(define (get-var-list let-list)
  (define (extractor let-list var-list)
    (if(null? let-list)
       var-list
       (cons (caar let-list)(extractor (cdr let-list) var-list))))
  (extractor let-list '()))

(define (get-exp-list let-list)
  (define (extractor let-list exp-list)
    (if(null? let-list)
       exp-list
       (cons (cadar let-list)(extractor (cdr let-list) exp-list))))
  (extractor let-list '()))

  
(define (get-body exp) (cddr exp))
(define (let? exp) (tagged-list? exp 'let))

;;------------------ d --------------------
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if-then exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((or? exp) (eval-or exp env)) 
        ((and? exp) (eval-and exp env)) 
        ((let? exp) (mc-eval (eval-let-2 exp) env)))) ;;Endret til den andre let metoden


(define (eval-let-2 exp)
  (define list1 '())
  (define list2 '())
  (define (loop expr list1 list2)
    (cond ((eq? 'in (car expr))
           (cons (make-lambda list1 (cdr expr)) list2))
        (else (loop (if-rest-elem expr) (cons (cadr expr) list1)(cons (cadddr expr) list2)))))
  (loop exp list1 list2))

;;------------------ e -------------------


(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if-then exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((or? exp) (eval-or exp env)) 
        ((and? exp) (eval-and exp env)) 
        ((let? exp) (mc-eval (eval-let-2 exp) env))
        ((while? exp) (mc-eval (while->if exp) env)))) ;;Lagt til

       

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((or? exp) #t)  
        ((and? exp) #t)
        ((let? exp) #t)
        ((while? exp) #t) ;;lagt til
        (else #f)))

(define (while? exp) (tagged-list? exp 'while))

(define (while->if exp)
  ('if (cadr exp) 'then (begin (cddr exp) (while->if exp))) 'else #f)