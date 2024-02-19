#lang racket

(require "simpleParser.rkt")

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
          (else (error "Bad statement formation")))))
    
;; declares a new variable and optionally sets its value to an evaluated expression
(define interpret-declare
  (lambda (declare state)
    (cond ((not (eq? (get-value (cadr declare) state) '~)) (error "Variable already declared"))
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
    (cond ((or (eq? 'd expression) (eq? '~ expression)) (error "Variable not initialized"))
          ((number? expression) expression)
          ((ret-boolean? expression) expression)
          ((and (list? expression) (is-condition? expression)) (interpret-boolean expression state))
          ((list? expression) (interpret-int expression state))
          (else (interpret-expression (get-value expression state) state)))));is a variable

;; evaluates an integer expression
(define interpret-int
  (lambda (expression state)
    (cond
      ((eq? (leftoperand expression) '~) (error "Bad statement formation"))
      ((and (eq? (rightoperand expression) '~) (eq? (operator expression) '-)) (* (interpret-expression (leftoperand expression) state) -1)) ; Negative Sign
      ((eq? (rightoperand expression) '~) (error "Bad statement formation"))
      ((eq? (operator expression) '+) (+ (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      (else (error "Bad statement formation")))))

(define operator (lambda (exp) (car exp)))
(define leftoperand cadr)
(define rightoperand
  (lambda (exp)
    (if (null? (cddr exp)) '~ (caddr exp))))

;; performes variable assignment
(define interpret-assign
  (lambda (assignment state)
    (if (eq? (get-value (cadr assignment) state) '~)
        (error "Variable not declared")
        (add-to-state (cadr assignment) (interpret-expression (caddr assignment) state) state))))

;; evaluates a return statement
(define interpret-return
  (lambda (return state)
    (list (car state) (cadr state) (interpret-expression (cadr return) state))))
     
;; evaluates an if then else statement
(define interpret-if
  (lambda (statement state)
    (cond ((interpret-boolean (cadr statement) state) (interpret-statement (caddr statement) state))
          ((not (null? (cdddr statement))) (interpret-statement (cadddr statement) state))
          (else state)))) ; No else condition

;; evaluates a boolean expression as true or false
(define interpret-boolean
  (lambda (expression state)
    (cond
      ((ret-boolean? expression) expression)
      ((eq? (leftoperand expression) '~) (error "Bad statement formation"))
      ((eq? (operator expression) '!) (ret-not (interpret-expression (leftoperand expression) state)))
      ((eq? (rightoperand expression) '~) (error "Bad statement formation"))
      ((eq? (operator expression) '||) (ret-or (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (ret-and (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))
      ((eq? (operator expression) '==) (convert-boolean (eq? (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))
      ((eq? (operator expression) '!=) (convert-boolean (not (eq? (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state)))))
      ((eq? (operator expression) '<)  (convert-boolean (< (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))
      ((eq? (operator expression) '>)  (convert-boolean (> (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))
      ((eq? (operator expression) '<=)  (convert-boolean (<= (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))
      ((eq? (operator expression) '>=)  (convert-boolean (>= (interpret-expression (leftoperand expression) state) (interpret-expression (rightoperand expression) state))))
      (else (error "Bad statement formation")))))

;; Evaluates the while loop
(define interpret-while
  (lambda (statement state)
    (cond ((interpret-boolean (cadr statement) state) (interpret-while statement (interpret-statement (caddr statement) state)))
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

;; checks if a complex expression evaluates to a boolean (false if an integer or non-complex expression)
(define is-condition?
  (lambda (statement)
    (cond ((eq? (car statement) '==) #t)
          ((eq? (car statement) '!=) #t)
          ((eq? (car statement) '<) #t)
          ((eq? (car statement) '>) #t)
          ((eq? (car statement) '<=) #t)
          ((eq? (car statement) '>=) #t)
          ((eq? (car statement) '&&) #t)
          ((eq? (car statement) '||) #t)
          ((eq? (car statement) '!) #t)
          (else #f))))

; Checks if an atom is 'true or 'false
(define ret-boolean?
  (lambda (atom)
    (or (eq? atom 'true) (eq? atom 'false))))

; Converts #t and #f to 'true and 'false, respectively
(define convert-boolean
  (lambda (bool)
    (if bool
        'true
        'false)))

; Returns negated version of input atoms 'true and 'false (as #t or #f)
(define ret-not
  (lambda (bool)
    (if (eq? bool 'true)
        #f
        #t)))

; Returns result of bool1 && bool2 for input atoms 'true and 'false (as #t or #f)
(define ret-and
  (lambda (bool1 bool2)
    (if (and (eq? bool1 'true) (eq? bool2 'true))
        #t
        #f)))

; Returns result of bool1 || bool2 for input atoms 'true and 'false (as #t or #f)
(define ret-or
  (lambda (bool1 bool2)
    (if (or (eq? bool1 'true) (eq? bool2 'true))
        #t
        #f)))