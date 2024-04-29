; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
#lang racket
(require "classParser.rkt")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file classname)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (add-call-to-main (parser file) classname) (newenvironment) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown")) #f))))))

; Takes a list of lists (the parse tree) and adds a call to main at the end
(define add-call-to-main
  (lambda (file classname)
    (append file (list (append '(main-call main ()) (cons classname '()))))))


; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw useReturn)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw useReturn) return break
                                  continue throw useReturn))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw useReturn)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw useReturn))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw useReturn))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw useReturn))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement (push-frame environment (get-ID (topframe environment)) null) return break continue
                                                                throw useReturn))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw useReturn))
      ((eq? 'function (statement-type statement)) (interpret-function-declare statement environment throw))
      ((eq? 'funcall (statement-type statement)) (interpret-function-call statement environment throw #f))
      ((eq? 'class (statement-type statement)) (interpret-class-definition statement environment return break continue throw useReturn))
      ((eq? 'static-function (statement-type statement)) (interpret-function-declare statement environment throw))
      ((eq? 'main-call (statement-type statement)) (main-call statement environment throw #f))
      (else (myerror "Unknown statement:" (statement-type statement))))))


(define interpret-main
  (lambda (statement environment return break continue throw useReturn)
    (interpret-statement-list (get-function-body statement) environment return break continue throw useReturn)))

;; class has:  (instance variable names and method names) (instance variable values/expr and method closures) classID (super's ID)
; '((A B) (((y x main) (10 5 (main closure)) A superID) B-info) global null)


;; for a object var obj = new B()
;; store 'obj in varList and      (copy of B's closure) in valList
;; such that if we ever call obj.method()
;; we look through B's closure and then all superclasses thereon until we hit null
;; upon an object being created, we set the 'this variable to this new object
(define interpret-object-creation
  (lambda (expr environment throw)
    (get-obj-closure (get-class-def-name expr) environment)))

;Interprets a class definition
(define interpret-class-definition
  (lambda (statement environment return break continue throw useReturn)
    (insert (get-class-def-name statement) (topframe (interpret-statement-list (get-class-body statement)
            (push-frame environment (get-class-def-name statement) (get-class-superID statement)) return break continue throw useReturn)) environment)))
    
;Returns the class' super
(define get-class-superID
  (lambda (statement)
    (cond ((null? (caddr statement)) 'global)
    (else (cadr (caddr statement))))))

;Returns the class body
(define get-class-body cadddr)

; Returns the name of the class when it is defined
(define get-class-def-name cadr)

; Returns the ID of a class frame in the environment
(define get-class-ID caddr)

; Returns a pair of lists that contain the closure for an object's runtime type
; and all of the other classes that are higher in the inheritance (B extends A)
; closure in form of '( (class names) (class closures) classID link)
; Lets say that B extends A, C extends B.
; We have an object c = new C();
; now, we can access methods in classes A, B, and C from object c
; so we need to store all the methods for A, B, and C in the closure for object c
; closure of object c = ( (Same for A) ((B's var/methods) (B's valList) B A) ((C's var/methods) (C's valList) C B))
(define get-obj-closure
  (lambda (classID environment)
    (get-obj-closure-helper classID environment '())))

; Helper to find the closure for an object. Written in accumulator passing style
(define get-obj-closure-helper
  (lambda (classID environment closure)
    (cond ((eq? classID 'global) closure)
          (else (get-obj-closure-helper (get-link (get-class-closure classID environment)) environment (append closure (list (get-class-closure classID environment))))))))
    
; Returns a class closure from the environment
(define get-class-closure
  (lambda (classID environment)
    (lookup classID environment)))

;Returns the state frame with a 'global link
(define get-global-frame
  (lambda (environment)
    (cond ((null? environment) (myerror "Reached the end of environment while parsing"))
          ((eq? 'global (get-class-ID (car environment))) (car environment))
          (else (get-global-frame (cdr environment))))))


; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw useReturn)
    (if useReturn
        (return (eval-expression (get-expr statement) environment throw))
        environment)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
; first condition interprets a (dot a x)
; second condition interprets anything else
(define interpret-assign
  (lambda (statement environment throw)
    (cond ((list? (get-assign-lhs statement)) (update (operand2 (get-assign-lhs statement)) (get-assign-rhs statement) (operand1 (get-assign-lhs statement))))
          (else (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw useReturn)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw useReturn))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw useReturn))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw useReturn)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw useReturn))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw useReturn)
    (pop-frame (interpret-statement-list (cdr statement) environment return (lambda (env) (break (pop-frame env))) (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env))) useReturn))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block useReturn)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block (push-frame env (get-ID (topframe environment)) null) return break continue
                                                                           throw useReturn)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (push-frame (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env (get-ID (topframe environment)) null))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2))) useReturn)) (get-ID (topframe environment)) null)
                                     return break continue throw useReturn)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw useReturn)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block (push-frame environment (get-ID (topframe environment)) null) return break continue
                                                              throw useReturn) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block (push-frame env (get-ID (topframe environment)) null) return break continue throw
                                                               useReturn))))
              (new-continue (lambda (env) (continue (interpret-block finally-block (push-frame env (get-ID (topframe environment)) null) return break continue
                                                                     throw useReturn))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block useReturn)))
         (interpret-block finally-block
                          (push-frame (interpret-block try-block (push-frame environment (get-ID (topframe environment)) null) new-return new-break new-continue
                                                       new-throw useReturn) (get-ID (topframe environment)) null)
                          return break continue throw useReturn))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw)))))

; Interprets a call to a function and produces the return value/ updated environment
(define interpret-function-call
  (lambda (statement environment throw useReturn)
    (call/cc (lambda (new-return) 
               (cond ((list? (get-function-name statement))
                      (let* ((function-closure (eval-expression (get-function-name statement) environment throw))
                             (new-env (interpret-function-call-helper (get-function-param-names (get-function-name statement) environment) (get-function-param-vals statement function-closure)
                                                              (push-frame environment (get-function-name statement) (get-function-link (get-function-name statement)
                                                                                                                                       environment))
                                                              throw useReturn))
                     (new-break (lambda (s) (error "Break out of loop")))
                     (new-continue (lambda (s) (error "Continue used outside of loop"))))
                 (interpret-statement-list (cadr function-closure) new-env new-return new-break new-continue throw (or useReturn (eq? 'main (get-function-name statement))))))
                     (else
                      (let ((new-env (interpret-function-call-helper (get-function-param-names (get-function-name statement) environment) (get-function-param-vals statement
                                                              (if (eq? 'main (get-function-name statement)) '() (lookup 'this environment)))
                                                              (push-frame environment (get-function-name statement) (get-function-link (get-function-name statement) environment))
                                                              throw useReturn))
                     (function-body (get-function-body-environment (get-function-name statement) environment))
                     (new-break (lambda (s) (error "Break out of loop")))
                     (new-continue (lambda (s) (error "Continue used outside of loop"))))
                 (interpret-statement-list function-body new-env new-return new-break new-continue throw useReturn))))))))


(define main-call
  (lambda (statement environment throw useReturn)
    (call/cc (lambda (new-return) 
               (let ((new-env (interpret-function-call-helper (get-function-param-names (get-function-name statement) environment) (get-function-param-vals statement
                                                              (if (eq? 'main (get-function-name statement)) '() (lookup 'this environment)))
                                                              (push-frame environment (get-function-name statement) (get-function-link (get-function-name statement) environment))
                                                              throw useReturn))
                     (function-body (lookup 'main (lookup (get-main-class-name statement) environment)))
                     (new-break (lambda (s) (error "Break out of loop")))
                     (new-continue (lambda (s) (error "Continue used outside of loop"))))
                 (interpret-statement-list function-body new-env new-return new-break new-continue throw useReturn))))))


(define get-main-class-name cadddr)


; Returns the new environment with all parameters inserted into the top layer
(define interpret-function-call-helper
  (lambda (paramNames paramVals environment throw useReturn)
    (cond ((null? paramNames) environment)
          ((not (eq? (length paramNames) (length paramVals))) (myerror "Parameter Mismatch")) 
          (else (interpret-function-call-helper (cdr paramNames) (cdr paramVals)
                         (insert (car paramNames) (eval-expression (car paramVals) (pop-frame environment) throw) environment) throw useReturn)))))

; Returns the function parameters when the function is called
(define get-function-param-vals
  (lambda (statement this)
    (cons this (cddr statement))))

; Returns the parameter names for a function (ex: this, length, width)
(define get-function-param-names
  (lambda (name environment)
    (cons 'this (car (lookup name environment)))))

; Returns the function body as stored in the environment
(define get-function-body-environment
  (lambda (name environment)
    (cadr (lookup name environment))))

; Get the name of the static link for a function
(define get-function-link
  (lambda (name environment)
    (caddr (lookup name environment))))
    
; Returns the name of the function 
(define get-function-name cadr)

; Returns the parameter signature of the function
(define get-function-params caddr)

; Returns the body of the function
(define get-function-body cadddr)

; Combines the parameter list and the body of a function into a list
(define pack-function-definition
  (lambda (param-list body static-link)
    (list param-list body static-link)))

; Interprets a function declaration
(define interpret-function-declare
  (lambda (statement environment throw)
    (cond ((eq? (get-function-name statement) 'main) (interpret-function-call statement environment throw #t))
          (else (insert (get-function-name statement) (pack-function-definition (get-function-params statement)
                                           (get-function-body statement) (get-ID (topframe environment)))
            environment)))))

; Returns the identifier for a layer in the evironment 
(define get-ID caddr)

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      ((eq? 'funcall (operator expr)) (interpret-function-call expr environment throw #t))
      ((eq? 'new (operator expr)) (interpret-object-creation expr environment throw))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? 'dot (operator expr)) (interpret-dot (eval-expression op1value environment throw) (operand2 expr) environment throw))
      (else (myerror "Unknown operator:" (operator expr))))))


; Interprets the dot operator and returns the field/method closure that the dot is referencing
(define interpret-dot
  (lambda (obj construct environment throw)
    (lookup construct obj)))


; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

;; class has:  (instance variable names and method names) (instance variable values/expr and method closures) classID (super's closure)
;;                                                       constructors count as methods

;; object / instance has : (instance variable names) (instance variable values) runtime-type-closure

;; function defintion closure: (parameter-list body link classID) -- classID for the containing class

;; function call has: ( (vars and nested functions) (vals and nested function closures) functionID static-link)


;;; The global frame: '( (var1 var2 ... func1 func2 ...) (val1 val2 ... (paramlist1 body1 global) (paramlist2 body2 global) ...) global null)
;; --> id for global frame is global and link for global frame is null

;;; Function call frame for func1: '( (param1 param2 func3 ...) (val1 val2 (paramlist3 body3 func1) ... ) func1 global)
;;; Function call frame for func3: '( (param1 param2 ...) (val1 val2 ...) func3 func1)

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe 'global null))))

; Returns an empty frame
(define newframe
  (lambda (id link)
  (list '() '() id link)))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment id link)
    (cons (newframe id link) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))

;Get the link of the frame
(define get-link cadddr)

; Gets the frame (& following frames) that the static link points to
(define get-env-static-link
  (lambda (link environment)
    (cond ((null? environment) null)
          ((eq? (get-ID (topframe environment)) link) environment)
          (else (get-env-static-link link (cdr environment))))))

; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment (get-link (topframe environment)))))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment link)
    (cond
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      ((null? (cdr environment)) (myerror "error: undefined variable" var))
      ((eq? link null) (lookup-in-env var (cdr environment) (get-link (topframe (cdr environment))))) ;use dynamic link for blocks
      (else (lookup-in-env var (get-env-static-link link environment) (get-link (topframe (get-env-static-link link environment)))))))) ; use static link

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)) (get-ID frame) (get-link frame))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)) (get-ID frame) (get-link frame))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))