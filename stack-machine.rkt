#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack null)

(define (make-stack) empty-stack)

(define (push element stack)
  (cons element stack))

(define (top stack)
  (car stack))

(define (pop stack)
  (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (hash 1 stack 2 co-varnames 3 co-consts 4 co-names 5 co-code 6 IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (hash-ref stack-machine 2))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (hash-ref stack-machine 3))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (hash-ref stack-machine 4))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (hash-ref stack-machine 5))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (hash-ref stack-machine 1))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (hash-ref stack-machine 6))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (aux symbols symbol 1))

(define (aux symbols symbol index)
  (cond
    [(null? symbols) index]
    [(eq? (car symbols) symbol) index] 
    [else (aux (cdr symbols) symbol (+ index 1))]))
    

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (hash-set stack-machine (get-symbol-index symbol) item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (hash-set stack-machine 1 (push value (get-stack stack-machine))))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (hash-set stack-machine 1 (pop (get-stack stack-machine))))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  
      (cond
        [(eq? (car (car (get-code stack-machine))) (get-opname 100))
                 (run-stack-machine (LOAD_CONST (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 124))
                 (run-stack-machine (LOAD_FAST (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 125))
                 (run-stack-machine (STORE_FAST (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 23))
                 (run-stack-machine (BINARY_ADD (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 24))
                 (run-stack-machine (BINARY_SUBTRACT (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 22))
                 (run-stack-machine (BINARY_MODULO (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]

        [(eq? (car (car (get-code stack-machine))) (get-opname 55))
                 (run-stack-machine (BINARY_ADD (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 56))
                 (run-stack-machine (BINARY_SUBTRACT (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 59))
                 (run-stack-machine (BINARY_MODULO (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 107))
                 (run-stack-machine (COMPARE_OP (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]
        
        [(eq? (car (car (get-code stack-machine))) (get-opname 114))
                 (run-stack-machine (POP_JUMP_IF_FALSE (cdr (car (get-code stack-machine))) (hash-set stack-machine 5 (cdr (get-code stack-machine)))))]

        
        [(eq? (car (car (get-code stack-machine))) (get-opname 83))
                stack-machine]

        ))
     

;;---------------------------------------------------

(define (LOAD_CONST index stack-machine)
    (hash-set
     (push-exec-stack(hash-ref (get-consts stack-machine) index) stack-machine)
     6
     (+ (get-IC stack-machine) 1)))



(define (STORE_FAST index stack-machine)
  (hash-set (hash-set (hash-set stack-machine 2 (hash-set (get-varnames stack-machine) index (car (get-stack stack-machine))))
                      1
                      (get-stack (pop-exec-stack stack-machine)))
            6
            (+ (get-IC stack-machine) 1)))



(define (LOAD_FAST index stack-machine)
    (hash-set
     (push-exec-stack(hash-ref (get-varnames stack-machine) index) stack-machine)
     6
     (+ (get-IC stack-machine) 1)))


(define (BINARY_ADD index stack-machine)
    (hash-set
        (push-exec-stack (+ (car (get-stack stack-machine)) (car (get-stack (pop-exec-stack stack-machine)))) (pop-exec-stack (pop-exec-stack stack-machine)))
        6
        (+ (get-IC stack-machine) 1)))


(define (BINARY_SUBTRACT index stack-machine)
    (hash-set
        (push-exec-stack (- (list-ref (get-stack stack-machine) 1) (list-ref (get-stack stack-machine) 0)) (pop-exec-stack (pop-exec-stack stack-machine)))
        6
        (+ (get-IC stack-machine) 1)))


(define (BINARY_MODULO index stack-machine)
    (hash-set
        (push-exec-stack (modulo (list-ref (get-stack stack-machine) 1) (list-ref (get-stack stack-machine) 0)) (pop-exec-stack (pop-exec-stack stack-machine)))
        6
        (+ (get-IC stack-machine) 1)))

(define (COMPARE_OP index stack-machine)
    (hash-set
        (push-exec-stack ((get-cmpop index) (car (get-stack (pop-exec-stack stack-machine))) (car (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack stack-machine)))
        6
        (+ (get-IC stack-machine) 1)))

(define (POP_JUMP_IF_FALSE target stack-machine)

  (if (eq? (car (get-stack stack-machine)) #f)
      (hash-set
         (multi-pop (- (/ target 2) (get-IC stack-machine) 1) (pop-exec-stack stack-machine))
         6
         (/ target 2))

      (hash-set
          (pop-exec-stack stack-machine)
          6
          (+ (get-IC stack-machine) 1))))

(define (multi-pop counter stack-machine)
  (if (eq? counter 0)
      stack-machine
      (multi-pop (- counter 1) (hash-set stack-machine 5 (cdr (get-code stack-machine))))))
