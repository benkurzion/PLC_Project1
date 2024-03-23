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
    (cond ((null? start) (pop-layer state)) ; No more statements in block
          ((null? (rest-elements start)) ; Last statement in block (interpret it)
           (interpret-block null (interpret-statement (next-element start) state return next break continue throw)
            return next break continue throw))
          (else (interpret-block (rest-elements start) ; Multiple statements left in block (interpret next statement and recursively the rest)
                 (interpret-statement (next-element start) state return
                  (lambda (s) (interpret-block (rest-elements start) s return next break continue throw)) break continue throw)
                 return next break continue throw)))))

;; Considers a new statement and figures out how to evalute it
(define interpret-statement
  (lambda (statement state return next break continue throw)
    (cond ((eq? (statement-type statement) '=) (interpret-assign statement state))
          ((eq? (statement-type statement) 'var) (interpret-declare statement state))
          ((eq? (statement-type statement) 'while) (interpret-while statement ; call/cc to jump to code after loop
                                                    (call/cc (lambda (k) (interpret-while statement state return next break k throw)))
                                                    return next break continue throw))
          ((eq? (statement-type statement) 'if) (interpret-if statement state return next break continue throw))
          ((eq? (statement-type statement) 'return) (return (interpret-return statement state)))
          ((eq? (statement-type statement) 'begin) (interpret-block (rest-elements statement) (add-new-layer state) return next break continue throw))
          ((eq? (statement-type statement) 'break) (interpret-break break state))
          ((eq? (statement-type statement) 'continue) (interpret-continue continue state))
          ((eq? (statement-type statement) 'throw) (interpret-throw statement state (lambda (s t) (throw s t))))
          ((eq? (statement-type statement) 'try) (interpret-try statement state return next break continue throw))
          ((eq? (statement-type statement) 'next) (next state)) ; For try-catch blocks
          (else (error "Bad statement formation")))))

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
          ((has-no-assignment? declare) (interpret-declare-helper (dec-name declare) 'd state)) ; Just declare variable
          (else (interpret-declare-helper (dec-name declare) (dec-value declare) state))))) ; Declare variable and initialize

(define interpret-declare-helper
  (lambda (name assign state)
    (cond ((eq? 'd assign) (add-to-state name 'd state)) ; Just declaration
          (else (add-to-state name (interpret-expression assign state) state))))) ; Initialization

;; Returns declaration variable
(define dec-name cadr)

;; Returns optional declaration value
(define dec-value caddr)

;; Returns whether declaration has an assignment
(define has-no-assignment?
  (lambda (declare)
    (null? (cddr declare))))


;;;; Assignment Functions

;; Reassigns value to variable
(define interpret-assign
  (lambda (assignment state)
    (if (eq? (get-value (ass-name assignment) state) '~)
        (error "Variable not declared")
        (add-to-state (ass-name assignment) (interpret-expression (ass-value assignment) state) state)))) ; Assign value to declared or initialized var

;; Returns assignment variable
(define ass-name cadr)

;; Returns assignment value
(define ass-value caddr)

;; Gets the value of a variable given the current state
(define get-value
  (lambda (name state)
    (cond ((null? state) '~) ; Not declared in any layer
          ((eq? '~ (get-value-helper name (get-names (get-top-layer state)) (get-values (get-top-layer state))))
           (get-value name (get-other-layers state))) ; Not declared in this layer, check next one deeper
          (else (get-value-helper name (get-names (get-top-layer state)) (get-values (get-top-layer state))))))) ; Declared in this layer, return value

(define get-value-helper
  (lambda (name varList valList)
    (cond ((null? varList) '~)
          ((null? name) '~)
          ((eq? name (car varList)) (car valList))
          (else (get-value-helper name (cdr varList) (cdr valList))))))

;; Gets the top layer from the state
(define get-top-layer car)

;; Gets all the other layers from the state not including the top
(define get-other-layers cdr)

;; (Re)assigns value of variable in a new state
(define add-to-state
  (lambda (name value state)
    (cond ((not (eq? '~ (get-value name state))) (redefine-value name value state)) ; Already declared, reassign variable
    (else (cons (pack-layer (cons name (get-names (get-top-layer state))) (cons value (get-values (get-top-layer state))))
                (get-other-layers state)))))) ; Not declared, add value to top layer

;; Changes a variable value into the given value in a new state
(define redefine-value
  (lambda (name value state)
    (cond ((null? state) '()) ; Not found in any layers
          ((eq? (get-top-layer state) (redefine-val-helper name value (get-names (get-top-layer state)) (get-values (get-top-layer state))))
           (cons (get-top-layer state) (redefine-value name value (get-other-layers state)))) ; Not found in this layer, check one deeper
          (else (cons (redefine-val-helper name value (get-names (get-top-layer state)) (get-values (get-top-layer state)))
                      (redefine-value name value (get-other-layers state))))))) ; Found in this layer, rearrange state

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
          ((number? expression) expression) ; Base case (integer)
          ((ret-boolean? expression) expression) ; Base case ('true or 'false)
          ((and (list? expression) (is-condition? expression)) (interpret-boolean expression state)) ; Complex boolean
          ((list? expression) (interpret-int expression state)) ; Complex integer
          (else (interpret-expression (get-value expression state) state))))) ; Variable

;; Evaluates an integer expression
(define interpret-int
  (lambda (expression state)
    (cond
      ((eq? (op1 expression) '~) (error "Bad statement formation"))
      ((and (eq? (op2 expression) '~) (eq? (op expression) '-)) (* (interpret-expression (op1 expression) state) -1)) ; Negative Sign
      ((eq? (op2 expression) '~) (error "Bad statement formation"))
      ((eq? (op expression) '+) (+ (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      ((eq? (op expression) '-) (- (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      ((eq? (op expression) '*) (* (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      ((eq? (op expression) '/) (quotient (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      ((eq? (op expression) '%) (remainder (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      (else (error "Bad statement formation")))))

;; Evaluates a boolean expression
(define interpret-boolean
  (lambda (expression state)
    (cond
      ((ret-boolean? expression) expression) ; 'true or 'false
      ((eq? (op1 expression) '~) (error "Bad statement formation"))
      ((eq? (op expression) '!) (ret-not (interpret-expression (op1 expression) state)))
      ((eq? (op2 expression) '~) (error "Bad statement formation"))
      ((eq? (op expression) '||) (ret-or (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      ((eq? (op expression) '&&) (ret-and (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))
      ((eq? (op expression) '==) (convert-boolean (eq? (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state))))
      ((eq? (op expression) '!=) (convert-boolean (not (eq? (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state)))))
      ((eq? (op expression) '<)  (convert-boolean (< (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state))))
      ((eq? (op expression) '>)  (convert-boolean (> (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state))))
      ((eq? (op expression) '<=)  (convert-boolean (<= (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state))))
      ((eq? (op expression) '>=)  (convert-boolean (>= (interpret-expression (op1 expression) state) (interpret-expression (op2 expression) state))))
      (else (error "Bad statement formation")))))

;; Gets operator of an expression
(define op (lambda (exp) (car exp)))

;; Gets left operand (or unary operand) of an expression
(define op1 cadr)

;; Gets optional right operand of an expression
(define op2
  (lambda (exp)
    (if (null? (cddr exp)) '~ (caddr exp))))


;;;; Boolean Parsing Functions

;; checks if a complex expression evaluates to a boolean (false if an integer or non-complex expression)
(define is-condition?
  (lambda (statement)
    (cond ((eq? (op statement) '==) #t)
          ((eq? (op statement) '!=) #t)
          ((eq? (op statement) '<) #t)
          ((eq? (op statement) '>) #t)
          ((eq? (op statement) '<=) #t)
          ((eq? (op statement) '>=) #t)
          ((eq? (op statement) '&&) #t)
          ((eq? (op statement) '||) #t)
          ((eq? (op statement) '!) #t)
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
    (cond ((eq? (interpret-boolean (if-condition statement) state) 'true) ; Interpret then
           (interpret-statement (if-then statement) state return next break continue throw))
          ((has-else? statement) (interpret-statement (if-else statement) state return next break continue throw)) ; Interpret else
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
           (loop condition body (interpret-statement body state return next break continue throw) return next break continue throw)) ; Next iteration
          (else (next state))))) ; Break from loop (condition became false)

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


;;;; Error functions

;; Evaluates a throw statement
(define interpret-throw
  (lambda (statement state throw)
    (cond ((null? throw) (error "Uncaught exception"))
          (throw state (interpret-expression (throw-value statement) state)))))

;; Returns value thrown by a throw statement
(define throw-value cadr)

;; Evaluates a try statement
(define interpret-try
  (lambda (statement state return next break continue throw)
    (let* ((newNext (lambda (s) (interpret-finally (get-finally statement) s return next break continue throw)))
          (newContinue (lambda (s) (interpret-finally (get-finally statement) s return next break continue throw)))
          (newBreak (lambda (s) (interpret-finally (get-finally statement) s return break break continue throw)))
          (newThrow (lambda (s e) (interpret-finally (get-finally statement) s return next break continue)))
          (myThrow (lambda (s e) (interpret-catch (get-catch statement) e s return
                                     (lambda (s1) (interpret-finally statement s1 return next break continue throw)) newBreak continue newThrow))))
          ; Interpret try block with catch and finally triggered by continuation functions when appropriate
          (interpret-block (get-try statement) (add-new-layer state) return newNext newBreak newContinue myThrow))))

;; Evaluates a finally statement
(define interpret-finally
  (lambda (statement state return next break continue throw)
    (interpret-block statement (add-new-layer state) return next break continue throw)))

;; Evaluates a catch statement
(define interpret-catch
  (lambda (statement e state return next break continue throw)
    (interpret-block (catch-block statement) (interpret-declare (list 'var (catch-error statement) e) (add-new-layer state)) ; Initialize error param
     return next break continue throw)))

;; Returns the try block
(define get-try
  (lambda (statement)
    (add-next (cadr statement))))

;; Returns the catch header and block
(define get-catch
  (lambda (statement)
    (add-next (caddr statement))))

;; Returns the error parameter of a catch block
(define catch-error cadr)

;; Returns the catch block
(define catch-block caddr)

;; Returns the finally block
(define get-finally
  (lambda (statement)
    (cadr (cadddr statement))))

;; Adds a 'next' statement to the end of a block to indicate that it should jump
(define add-next
  (lambda (block)
    (append-cps block (list (list 'next)) (lambda (v) v))))

;; Appends 2 lists together using tail recursion (needs basic return continuation with 1 input)
(define append-cps
  (lambda (lis1 lis2 return)
    (cond
      ((null? lis1) (return lis2))
      (else (append-cps (cdr lis1) lis2 (lambda (v1) (return (cons (car lis1) v1))))))))