#lang racket
(require "simpleParser.rkt")

;;;; State Functions

;; Gets the variable names of a state
(define get-names car)

;; Gets the variable values of a state
(define get-values cadr)


;; Converts list of variable names and variable values into a state
(define pack-layer
  (lambda (varList valList)
    (list varList valList)))

;; Returns an empty layer
(define empty-layer (list null null))

;; Returns an empty state
(define empty-state (list empty-layer)) ; variables, values


;; Add a new, empty layer to the current state
(define add-new-layer
  (lambda (state)
    (cons '(() ()) state)))

;; Removes the top layer of the state
(define pop-layer
  (lambda (state)
    (cdr state)))


;;;; Statement Functions

;; Takes a file name and returns the value expressed in the code
(define interpret
  (lambda (filename)
    (call/cc (lambda (k) (interpret-block (parser filename) empty-state k null null null null)))))

;; Recurses through the entire parse tree and inteprets all starts and statements
(define interpret-block
  (lambda (start state return next break continue throw)
    (cond ((null? start) (pop-layer state))
          ((null? (rest-elements start)) (interpret-block null (interpret-statement (next-element start) state return next break continue throw) return next break continue throw))
          (else (interpret-block (rest-elements start)
                                 (interpret-statement (next-element start) state return
                                                      (lambda (s) (interpret-block (rest-elements start) s return next break continue throw)) break continue throw)
                                 return next break continue throw)))))


;; Considers a new statement and figures out how to evalute it
(define interpret-statement
  (lambda (statement state return next break continue throw)
    (cond ((eq? (statement-type statement) '=) (interpret-assign statement state))
          ((eq? (statement-type statement) 'var) (interpret-declare statement state))
          ((eq? (statement-type statement) 'while) (interpret-while statement (call/cc (lambda (k) (interpret-while statement state return next break k throw))) return next break continue throw))
          ((eq? (statement-type statement) 'if) (interpret-if statement state return next break continue throw))
          ((eq? (statement-type statement) 'return) (return (interpret-return statement state)))
          ((eq? (statement-type statement) 'begin) (interpret-block (rest-elements statement) (add-new-layer state) return next break continue throw))
          ((eq? (statement-type statement) 'break) (interpret-break break state))
          ((eq? (statement-type statement) 'continue) (interpret-continue continue state))
          ((eq? (statement-type statement) 'throw) (interpret-throw statement state (lambda (t) (throw t))))
          ((eq? (statement-type statement) 'try) (interpret-try statement state return next break continue throw))
          (else (error "Bad statement formation")))))

(define interpret-throw
  (lambda (statement state throw)
    (cond ((null? throw) (error "Uncaught exception error"))
          ((throw (interpret-expression statement state))))))

(define interpret-try
  (lambda (statement state return next break continue throw)
    (let* ((newNext (lambda (s) (interpret-finally (get-finally statement) s return next break continue throw)))
          (newContinue (lambda (s) (interpret-finally (get-finally statement) s return next break continue throw)))
          (newBreak (lambda (s) (interpret-finally (get-finally statement) s return break break continue throw)))
          (newThrow (lambda (s e) (interpret-finally (get-finally statement) s return next break continue)))
          (myThrow (lambda (s e) (interpret-catch (get-catch statement) s return
                                     (lambda (s1) (interpret-finally statement s1 return next break continue throw)) newBreak continue newThrow))))
          (interpret-block (get-try statement) (add-new-layer state) return newNext newBreak newContinue newThrow))))

(define interpret-finally
  (lambda (statement state return next break continue throw)
    (interpret-block (rest-elements statement) (add-new-layer state) return next break continue throw)))

(define interpret-catch
  (lambda (statement state return next break continue throw)
    (interpret-block (rest-elements statement) (add-new-layer state) return next break continue throw)))

(define get-try
  (lambda (statement)
    (cadr statement)))

(define get-catch
  (lambda (statement)
    (caddr statement)))

(define get-finally
  (lambda (statement)
    (cadddr statement)))

;; Gets highest element in parse tree segment
(define next-element car)

;; Gets all elements besides the highest one in parse tree segment
(define rest-elements cdr)

;; Returns type of statement
(define statement-type car)


;;;; Declaration Functions

;; Declares a new variable and optionally sets its value to an evaluated expression
(define interpret-declare
  (lambda (declare state)
    (cond ((not (eq? (get-value (dec-name declare) state) '~)) (error "Variable already declared"))
          ((has-assignment? declare) (interpret-declare-helper (dec-name declare) 'd state))
          (else (interpret-declare-helper (dec-name declare) (dec-value declare) state)))))

(define interpret-declare-helper
  (lambda (name assign state)
    (cond ((eq? 'd assign) (add-to-state name 'd state))
          (else (add-to-state name (interpret-expression assign state) state)))))

;; Returns declaration variable
(define dec-name cadr)

;; Returns optional declaration value
(define dec-value caddr)

;; Returns whether declaration has an assignment
(define has-assignment?
  (lambda (declare)
    (null? (cddr declare))))


;;;; Assignment Functions

;; Reassigns value to variable
(define interpret-assign
  (lambda (assignment state)
    (if (eq? (get-value (ass-name assignment) state) '~)
        (error "Variable not declared")
        (add-to-state (ass-name assignment) (interpret-expression (ass-value assignment) state) state))))

;; Returns assignment variable
(define ass-name cadr)

;; Returns assignment value
(define ass-value caddr)

;; Gets the value of a variable given the current state
(define get-value
  (lambda (name state)
    (cond ((null? state) '~)
          ((eq? '~ (get-value-helper name (get-names (get-top-layer state)) (get-values (get-top-layer state))))
           (get-value name (get-other-layers state)))
          (else (get-value-helper name (get-names (get-top-layer state)) (get-values (get-top-layer state)))))))
    
;; Gets the top layer from the state
(define get-top-layer car)

;; Gets all the other layers from the state not including the top
(define get-other-layers cdr)

(define get-value-helper
  (lambda (name varList valList)
    (cond ((null? varList) '~)
          ((null? name) '~)
          ((eq? name (car varList)) (car valList))
          (else (get-value-helper name (cdr varList) (cdr valList))))))

;; (Re)assigns value of variable in a new state
(define add-to-state
  (lambda (name value state)
    (cond ((not (eq? '~ (get-value name state))) (redefine-value name value state)) ; Already declared
    (else (cons (pack-layer (cons name (get-names (get-top-layer state))) (cons value (get-values (get-top-layer state)))) (get-other-layers state)))))) ; Not declared. Add value to top layer

;; Changes a variable value into the given value in a new state
(define redefine-value
  (lambda (name value state)
    (cond ((null? state) '())
          ((eq? (get-top-layer state) (redefine-val-helper name value (get-names (get-top-layer state)) (get-values (get-top-layer state))))
           (cons (get-top-layer state) (redefine-value name value (get-other-layers state))))
          (else (cons (redefine-val-helper name value (get-names (get-top-layer state)) (get-values (get-top-layer state))) (redefine-value name value (get-other-layers state)))))))

; helper function to redefine a declared variable
(define redefine-val-helper
  (lambda (name value varList valList)
    (cond ((null? varList) empty-layer)
          ((eq? (car varList) name) (list varList (cons value (cdr valList)))) ; Reassigns value to first element in sublist of variables
          (else (list varList (cons (car valList) (cadr (redefine-val-helper name value (cdr varList) (cdr valList))))))))) ; Looks through rest of variables


;;;; Expression Functions

;; Evaluates any generic expression as either an integer or a boolean
(define interpret-expression
  (lambda (expression state)
    (cond ((or (eq? 'd expression) (eq? '~ expression)) (error "Variable not initialized"))
          ((number? expression) expression)
          ((ret-boolean? expression) expression) ; 'true or 'false
          ((and (list? expression) (is-condition? expression)) (interpret-boolean expression state)) ; Complex boolean
          ((list? expression) (interpret-int expression state)) ; Complex integer
          (else (interpret-expression (get-value expression state) state))))) ; Variable

;; Evaluates an integer expression
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

;; Evaluates a boolean expression
(define interpret-boolean
  (lambda (expression state)
    (cond
      ((ret-boolean? expression) expression) ; 'true or 'false
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

;; Gets operator of an expression
(define operator (lambda (exp) (car exp)))

;; Gets left operand (or unary operand) of an expression
(define leftoperand cadr)

;; Gets optional right operand of an expression
(define rightoperand
  (lambda (exp)
    (if (null? (cddr exp)) '~ (caddr exp))))


;;;; Boolean Parsing Functions

;; checks if a complex expression evaluates to a boolean (false if an integer or non-complex expression)
(define is-condition?
  (lambda (statement)
    (cond ((eq? (operator statement) '==) #t)
          ((eq? (operator statement) '!=) #t)
          ((eq? (operator statement) '<) #t)
          ((eq? (operator statement) '>) #t)
          ((eq? (operator statement) '<=) #t)
          ((eq? (operator statement) '>=) #t)
          ((eq? (operator statement) '&&) #t)
          ((eq? (operator statement) '||) #t)
          ((eq? (operator statement) '!) #t)
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
        'false
        'true)))

; Returns result of bool1 && bool2 for input atoms 'true and 'false (as #t or #f)
(define ret-and
  (lambda (bool1 bool2)
    (if (and (eq? bool1 'true) (eq? bool2 'true))
        'true
        'false)))

; Returns result of bool1 || bool2 for input atoms 'true and 'false (as #t or #f)
(define ret-or
  (lambda (bool1 bool2)
    (if (or (eq? bool1 'true) (eq? bool2 'true))
        'true
        'false)))


;;;; Return Functions

;; Evaluates a return statement
(define interpret-return
  (lambda (statement state)
    (interpret-expression (ret-value statement) state)))

;; Gets return value of a return statement
(define ret-value cadr)


;;;; Conditional Functions
     
;; Evaluates an if then else statement
(define interpret-if
  (lambda (statement state return next break continue throw)
    (cond ((eq? (interpret-boolean (if-condition statement) state) 'true) (interpret-statement (if-then statement) state return next break continue throw))
          ((has-else? statement) (interpret-statement (if-else statement) state return next break continue throw))
          (else state)))) ; No else condition

;; Returns condition of if statement
(define if-condition cadr)

;; Returns then statement of if statement
(define if-then caddr)

;; Returns else statement of if statement
(define if-else cadddr)

;; Returns whether if statement has an else block
(define has-else?
  (lambda (statement)
    (not (null? (cdddr statement)))))


;;;; Loop Functions

;; Evaluates a while loop
(define interpret-while
  (lambda (statement state return next old-break continue throw)
    (loop (while-condition statement) (while-statement statement) state return next (lambda (s) (next (pop-layer s)))
          (lambda (s) (interpret-while statement (pop-layer s) return next old-break continue throw)) throw)))

(define loop
  (lambda (condition body state return next break continue throw)
    (cond ((eq? 'true (interpret-boolean condition state))
           (loop condition body (interpret-statement body state return next break continue throw) return next break continue throw))
          (else (next state))))) ; SHOULD POP???? FIXME/FATAL

;; Returns condition of while statement
(define while-condition cadr)

;; Returns block of while statement
(define while-statement caddr)

;; Executes a break statement
(define interpret-break
  (lambda (break state)
    (if (null? break)
        (error "Break outside loop body")
        (break state))))

;; Executes a continue statement
(define interpret-continue
  (lambda (continue state)
    (if (null? continue)
        (error "Continue outside loop body")
        (continue state))))