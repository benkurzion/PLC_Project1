#lang racket

(require "simpleParser.rkt")
(require "abstraction.rkt")

;; state = ((x y z) (5 8 10) return)
;; if a variable is only declared, then state = '((...x...) (...d...) return)
;; because x is only declared (d)


;; Takes a file name and returns the value expressed in the code
(define interpret
  (lambda (filename)
    (caddr (interpret-start (car (parser filename)) (parser filename) '(() () return)))))

;; Recurses through the entire parse tree and inteprets all starts and statements
(define interpret-start
  (lambda (statement start state)
    (cond ((null? (cdr start)) (interpret-statement (car start) state)) ; start -> statement
          (else (interpret-start (interpret-statement(car start) state)
                (cdr start) (interpret-statement(car start) state))))))

;; Considers a new statement and figures out how to evalute it
(define interpret-statement
  (lambda (statement state)
    (cond ((not (eq? 'return (caddr state))) (caddr state))
          ((eq? (car statement) '=) (interpret-assign statement state))
          ((eq? (car statement) 'var) (interpret-declare statement state))
          ((eq? (car statement) 'while) (interpret-while statement state))
          ((eq? (car statement) 'if) (interpret-if statement state))
          ((eq? (car statement) 'return) (interpret-return statement state))
          (else 'NaN))))
    
;; declares a new variable and optionally sets its value to an evaluated expression
(define interpret-declare
  (lambda (declare state)
    (cond ((not (eq? (get-value (cadr declare) state) '~)) 'NaN)
          ((null? (cddr declare)) (interpret-declare-helper (cadr declare) 'd state))
          (else (interpret-declare-helper (cadr declare) (caddr declare) state)))))

;; helper method for the declare non-terminal
(define interpret-declare-helper
  (lambda (name assign state)
    (cond ((eq? 'd assign) (add-to-state name 'd state))
          (else (add-to-state name (interpret-expression assign state) state)))))

;; evaluates any generic expression as either an integer or a boolean
(define interpret-expression
  (lambda (expression state)
    (cond ((or(eq? 'd expression) (eq? '~ expression)) 'NaN)
          ((number? expression) expression)
          ((boolean? expression) expression)
          ((eq? expression 'true) #t)
          ((eq? expression 'false) #f)
          ((and (list? expression) (is-condition? expression)) (interpret-boolean expression state))
          ((list? expression) (interpret-int expression state))
          (else (get-value expression state)))));is a variable

;; evaluates an integer expression
(define interpret-int
  (lambda (expression state)
    (cond
      ((eq? (leftoperand expression) 'NaN) 'NaN)
      ((and (eq? (rightoperand expression) 'NaN) (eq? (operator expression) '-)) (* (interpret-expression (leftoperand expression) state) -1)) ; Negative Sign
      ((eq? (rightoperand expression) 'NaN) 'NaN)
      ((eq? (operator expression) '+) (+ (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))))

(define operator (lambda (exp) (car exp)))
(define leftoperand cadr)
(define rightoperand
  (lambda (exp)
    (if (null? (cddr exp)) 'NaN (caddr exp))))

;; performes variable assignment
(define interpret-assign
  (lambda (assignment state)
    (add-to-state (cadr assignment) (interpret-expression (caddr assignment) state) state)))

;; evaluates a return statement
(define interpret-return
  (lambda (return state)
    (list (car state) (cadr state) (interpret-expression (cadr return) state))))
     
;; evaluates an if then else statement
(define interpret-if
  (lambda (statement state)
    (cond ((eq? 'NaN (interpret-boolean (cadr statement) state)) 'NaN)
          ((interpret-boolean (cadr statement) state) (interpret-statement (caddr statement) state))
          ((not (null? (cdddr statement))) (interpret-statement (cadddr statement) state))
          (else state)))) ; No else condition

;; evaluates a boolean expression as true or false
(define interpret-boolean
  (lambda (expression state)
    (cond
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((eq? (leftoperand expression) 'NaN) 'NaN)
      ((eq? (operator expression) '!) (not (interpret-expression (leftoperand expression) state)))
      ((eq? (rightoperand expression) 'NaN) 'NaN)
      ((eq? (operator expression) '||) (or (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '==) (eq? (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))
      ((eq? (operator expression) '<)  (< (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '>)  (> (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '<=)  (<= (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '>=)  (>= (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      (else 'NaN))))


;; Evaluates the while loop
(define interpret-while
  (lambda (statement state)
    (cond ((interpret-expression (cadr statement) state) (interpret-while statement (interpret-statement (caddr statement) state)))
          (else state))))

;; Gets the value of a variable given the current state
(define get-value
  (lambda (name state)
    (get-value-helper name (car state) (cadr state))))

;; Helper to get value of variable name as stored in state
(define get-value-helper
  (lambda (name varList valList)
    (cond ((null? varList) '~)
          ((null? name) '~)
          ((eq? name (car varList)) (car valList))
          (else (get-value-helper name (cdr varList) (cdr valList))))))

;; adds a new key-value pair to the state and returns the new state
;; deletes old assignment if it exists
(define add-to-state
  (lambda (name value state)
    (cond ((not (eq? '~ (get-value name state))) (redefine-value name value state))
    (else (list (cons name (car state)) (cons value (cadr state)) (caddr state))))))

;; changes a variable value into the given value and returns the new state
(define redefine-value
  (lambda (name value state) ;(car state) (cadr state) (caddr state)
    (append (redefine-val-helper name value (car state) (cadr state)) (list (caddr state)))))

; helper function to redefine a declared variable
(define  redefine-val-helper
  (lambda (name value varList valList)
    (cond ((eq? (car varList) name) (list varList (cons value (cdr valList))))
          (else (list varList (cons (car valList)
                (cadr (redefine-val-helper name value (cdr varList) (cdr valList)))))))))

;; checks if a statement evaluates to a boolean or an integer
(define is-condition?
  (lambda (statement)
    (cond ((eq? statement 'true) #t)
          ((eq? statement 'false) #t)
          ((boolean? statement) #t)
          ((eq? (car statement) '==) #t)
          ((eq? (car statement) '!=) #t)
          ((eq? (car statement) '<) #t)
          ((eq? (car statement) '>) #t)
          ((eq? (car statement) '<=) #t)
          ((eq? (car statement) '>=) #t)
          ((eq? (car statement) '&&) #t)
          ((eq? (car statement) '||) #t)
          ((eq? (car statement) '!) #t)
          (else #f))))
           