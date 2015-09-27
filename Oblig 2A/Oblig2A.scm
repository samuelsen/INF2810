(load "huffman.scm")

;; 1a
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))
  
 
;; 1b
(define foo 30)
((lambda (x y)
   (+ foo x y))
 foo
 20)
 
 ;;Evaluerer til 80
((lambda (foo) 
   ((lambda (x y) (+ foo x y)) foo 20))
   10)

;;Evaluerer til 40
((lambda (x y)
   (+ foo x y))
 foo
 20)
 
;; Vi definerer foo lokalt til en annen verdien en den globale verdien av foo i det andre let-uttrykket@

;; 1c
 (define a1 (list 1 2 3 4))
 (define a2 (list + - * /))
 (define a3 (list 5 6 7 8))
 
 ((lambda (x y z) (y x z)) 5 * 5)
 
 ;; 2A
 (define (member? x items)
  (if (null? items)
      #f
      (if(eq? x (car items))
            #t
        (member? x (cdr items)))))
 
 (member? 'lake '(lake river ocean))
 
 ;;2B
 
 ;;2C
 (define (decodeHale bits tree)
   (define (decode-1 bits current-branch list)
    (if (null? bits)
        list
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (append list (cons (symbol-leaf next-branch) '())))
              (decode-1 (cdr bits) next-branch list)))))
  (decode-1 bits tree '()))
 
 ;;2D
 (decodeHale sample-code sample-tree)
 (decode sample-code sample-tree)
 ;; resultatet er (ninjas fight ninjas by night)

 ;;2E
 ;;Encode
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (cond ((member? symbol (symbols (left-branch tree))) 
             (cons 0 (encode-symbol symbol (left-branch tree))))
            ((member? symbol (symbols (right-branch tree))) 
             (cons 1 (encode-symbol symbol (right-branch tree))))
            (else‬ (cons '#f '())))))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)

;;2F
(define (grow-huffman-tree list)
  (define sortList (make-leaf-set list))
  (define (grow list1)
    (if (null? (cdr list1))
        (car list1)
        (grow (adjoin-set(make-code-tree (car list1)(cadr list1)) (cddr list1)))))
  (grow sortList))

(define freqs '((ninjas 57) (samurais 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by
12) (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
(define codebook (grow-huffman-tree freqs))
(decode '(1 1 1 0 1 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 1 1 1 1 1 0 0 1 1 1 0 0 0) codebook)

                 
;;2G
;; 40 bits for å kode meldingen, bitene er:
;; '(1 1 1 0 1 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 1 1 1 1 1 0 0 1 1 1 0 0 0)
;; gjennomsnittlig antall bits brukt for å kode meldingen med 17 ord er 40/17 = 2,35
;; For å finne antall bits som trengs for å kode meldingne med fast bitsstørrelse tar vi log2 16 som er 4
;; (ettersom det er 16 symboler i alfabetet)
;; og ganger med antall symboler i meldingen som skal kodes (17 i dette tilfelle) det gir oss 4 * 17 = 68
;; dermed trengs det 68 bits for å kode denne meldingen med fast bitlengde

;;2H 
 (define (huffman-leaves tree)
       (if (leaf? tree)
           (list (list (symbol-leaf tree) (weight-leaf tree)))
           (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree)))))
 
 ;;2I
;;2 (i)
(define (expected-codeword-length tree)
  (define (get-word-length symbol tre)
    (if (leaf? tre)
        0
        (cond ((member? symbol (symbols (left-branch tre)))
               (+ 1 (get-word-length symbol (left-branch tre))))
              ((member? symbol (symbols (right-branch tre)))
               (+ 1 (get-word-length symbol (right-branch tre)))))))
  (define (exp-c-l symb-list)
    (if (null? symb-list)
        0
        (+ (* (/ (cadar symb-list) 
                 (weight tree)) 
              (get-word-length (caar symb-list) tree)) 
           (exp-c-l (cdr symb-list)))))
  (exp-c-l (huffman-leaves tree)))   
 

