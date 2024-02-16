#lang racket

; evaluate <intexp> ::= <intexp> <op> <intexp> | <number>
; This examples does not use abstraction and so there are car's and cdr's everywhere.
; Not only is this harder to read, but it is harder to update if there is a specification change.
(define M_integer1
  (lambda (intexp)
    (cond
      ((number? intexp) intexp)
      ((eq? (car (cdr intexp)) '+) (+ (M_integer1 (car intexp)) (M_integer1 (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '-) (- (M_integer1 (car intexp)) (M_integer1 (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '*) (* (M_integer1 (car intexp)) (M_integer1 (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '/) (quotient (M_integer1 (car intexp)) (M_integer1 (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '%) (remainder (M_integer1 (car intexp)) (M_integer1 (car (cdr (cdr intexp)))))))))

; evaluate parse tree in prefix notation: '(<op> <operand> <operand>)
; Now this uses abstraction for the different parts of the expression.
; That makes it easier to read, and if the specifications change, only the abstraction functions must change
(define M_integer
  (lambda (intexp)
    (cond
      ((number? intexp) intexp)
      ((eq? (operator intexp) '+) (+ (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '-) (- (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '*) (* (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '/) (quotient (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '%) (remainder (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp)))))))

; Functions to define the parts of an expression, demonstrating two ways to write them.
(define operator (lambda (exp) (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)
      
  