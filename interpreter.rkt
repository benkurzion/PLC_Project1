#lang racket

(require "simpleParser.rkt")
(require "abstraction.rkt")

;; state = ((x y z) (5 8 10))
;; if a variable is only declared, then state = '((...x...) (...d...))
;; because x is only declared (d)


;; Takes a file name and returns the value expressed in the code
(define interpret
  (lambda (filename)
    (interpret-start (car (parser filename)) (parser filename) '(() () return))))

;; Recurses through the entire parse tree and inteprets all starts and statements
(define interpret-start
  (lambda (statement start state)
    (cond ((null? (cdr start)) (interpret-statement (car start) state)) ; start -> statement
          (else (interpret-start (interpret-statement(car start) state)
                (cdr start) (interpret-statement(car start) state))))))

;; Considers a new statement and figures out what to do with it
(define interpret-statement
  (lambda (statement state)
    (cond ((not (eq? 'return (caddr state))) (caddr state))
          ((eq? (car statement) '=) (interpret-assign statement state))
          ((eq? (car statement) 'var) (interpret-declare statement state))
          ((eq? (car statement) 'while) (interpret-while statement state))
          ((eq? (car statement) 'if) (interpret-if statement state))
          ((eq? (car statement) 'return) (interpret-return statement state))
          (else 'NaN))))
    

(define interpret-declare
  (lambda (declare state)
    (cond ((not (eq? (get-value (cadr declare)) '~)) 'NaN)
          ((null? (caddr declare)) (interpret-declare-helper (cadr declare) 'd state))
          (else (interpret-declare-helper (cadr declare) (caddr declare) state)))))


(define interpret-declare-helper
  (lambda (name assign state)
    (cond ((eq? 'd assign) (add-to-state name 'd state))
    (else (add-to-state name (interpret-expression assign) state)))))

;;((boolean? expression) expression) ?? ask harold
(define interpret-expression
  (lambda (expression state)
    (cond ((or(eq? 'd expression) (eq? '~ expression)) 'NaN)
          ((number? expression) expression)
          ((list? expression) (interpret-exp-helper expression state))
          (else (get-value expression state)))));is a variable

(define interpret-exp-helper
  (lambda (expression state)
    (cond
      ((eq? (leftoperand expression) 'NaN) 'NaN)
      ((eq? (rightoperand expression) 'NaN) 'NaN)
      ((eq? (operator expression) '+) (+ (interpret-expression (leftoperand expression)) (interpret-expression (rightoperand expression))))
      ((eq? (operator expression) '-) (- (interpret-expression (leftoperand expression)) (interpret-expression (rightoperand expression))))
      ((eq? (operator expression) '*) (* (interpret-expression (leftoperand expression)) (interpret-expression (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (interpret-expression (leftoperand expression)) (interpret-expression (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (interpret-expression (leftoperand expression)) (interpret-expression (rightoperand expression)))))))

(define operator (lambda (exp) (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)


(define interpret-assign
  (lambda (assignment state)
    (add-to-state (cadr assignment) (interpret-expression (caddr assignment)) state)))


(define interpret-return
  (lambda (return state)
    (list (car state) (cadr state) (interpret-expression return state)))
     

(define interpret-if
  (lambda (statement state)
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

;; adds a new key-value pair to the state and returns the new state
(define add-to-state
  (lambda (name value state)
    (list (cons name (car state)) (cons value (cadr state)) (caddr state))))
                                    
                
           