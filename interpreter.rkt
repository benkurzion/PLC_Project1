#lang racket

(require "simpleParser.rkt")
(require "abstraction.rkt")

;; state = ((x y z) (5 8 10))
;; list of two lists variables and their corresponding values


;; Takes a file name and returns the value expressed in the code
(define interpret
  (lambda (filename)
    (get-next-expression(parser filename))))

;; Returns the next atom in the parse tree
(define get-next-expression
  (lambda (parseTree)
    (cond ((null? parseTree) '())
          ((not(list? (car (car parseTree)))) (car parseTree))
          (else (get-next-expression (car parseTree))))))

;; Returns a new state after an expression is evaluated
(define m-state
  (lambda (expression state)
    1))

;; Return the integer value of an expression given the current state
(define m-integer
  (lambda (expression state)
    1))


;; Return the boolean value of an expression given the current state
(define m-boolean
  (lambda (expression state)
    1))

;; Gets the value of a variable given the current state
(define get-value
  (lambda (name state)
    (get-value-helper name (car state) (cadr state))))

;; Helper to get value of variable name
(define get-value-helper
  (lambda (name varList valList)
    (cond ((null? varList) '~)
          ((null? name) '~)
          ((eq? name (car varList)) (car valList))
          (else (get-value-helper (cdr varList) (cdr valList))))))
                                    
                
           