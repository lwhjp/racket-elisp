#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide
 (all-defined-out)
 (prefix-out el:
   (combine-out
    #%datum
    quote
    quasiquote
    unquote
    unquote-splicing)))

;; Reader

; TODO:
; () -> nil
; implicit quasiquote
; #' -> function
; vector
; char

;; Symbol components

(define global-variables (make-hasheq))
(define global-functions (make-hasheq))
(define global-plists (make-hasheq))

(hash-set*! global-variables
            'nil 'nil
            't 't)

;; Macros

;; TODO

(define-syntax-rule (el:#%module-begin form ...)
  (#%module-begin
   form ...))

;; Variables

; TODO: locals

(begin-for-syntax
  (struct dynamic-reference ()
    #:transparent
    #:property prop:set!-transformer
    (λ (stx)
      (syntax-case stx (set!)
        [(set! id v) (syntax-protect #'(set-dynamic-variable! 'id v))]
        [id (identifier? #'id) (syntax-protect #'(dynamic-variable-ref 'id))]))))

(define-syntax-rule (declare-dynamic id)
  (define-syntax id (dynamic-reference)))

(define-syntax-rule (el:#%top . id)
  (dynamic-variable-ref 'id))

(define-syntax el:defconst (make-rename-transformer #'el:defvar))

(define-syntax el:defvar
  (syntax-parser
    [(_ id:id)
     ;; Locally special
     (syntax-protect
      #'(begin
          (declare-dynamic id)
          'id))]
    [(_ id:id value:expr (~optional doc:string))
     (syntax-protect
      #'(begin
          (declare-dynamic id) ; XXX: only at toplevel
          (unless (hash-has-key? global-variables 'id)
            (hash-set! global-variables 'id value))
          'id))]))

(define (set-dynamic-variable! name value)
  (hash-set! global-variables name value))

(define (dynamic-variable-ref name)
  (hash-ref global-variables name (λ () (error "undefined variable:" name))))

(define (call-with-dynamic-binding thunk . bindings)
  (define binding-pairs
    (let loop ([bindings bindings])
      (if (null? bindings)
          '()
          (cons (cons (car bindings) (cadr bindings))
                (loop (cddr bindings))))))
  (define old-values
    (map (λ (b) (hash-ref global-variables (car b) #f))
         binding-pairs))
  (dynamic-wind
   (λ ()
     (for ([b (in-list binding-pairs)])
       (hash-set! global-variables (car b) (cdr b))))
   thunk
   (λ ()
     (for ([id (in-list (map car binding-pairs))]
           [v (in-list old-values)])
       (if v
           (hash-set! global-variables id v)
           (hash-remove! global-variables id))))))

(begin-for-syntax
  (define-syntax-class let-binding
    #:attributes (id value)
    (pattern id:id #:attr value #''nil)
    (pattern (id:id (~optional value:expr)))))

(define-syntax el:let
  (syntax-parser
    [(_ (binding:let-binding ...) body ...)
     (define-values (dynamic-bindings lexical-bindings)
       (partition
        (λ (b)
          (syntax-case b ()
            [(id _) (dynamic-reference? (syntax-local-value #'id (λ () #f)))]))
        (syntax->list #'((binding.id binding.value) ...))))
     (with-syntax ([((dynamic-id dynamic-v) ...) dynamic-bindings]
                   [((lexical-id lexical-v) ...) lexical-bindings])
       (with-syntax ([let-form #'(let ([lexical-id lexical-v] ...) (el:progn body ...))])
         (syntax-protect
          (if (null? dynamic-bindings)
              #'let-form
              #'(call-with-dynamic-binding (λ () let-form) (~@ 'dynamic-id dynamic-v) ...)))))]))

(define-syntax el:let*
  (syntax-rules ()
    [(_ () forms ...) (el:let () forms ...)]
    [(_ (binding) forms ...) (el:let (binding) forms ...)]
    [(_ (binding bindings ...) forms ...)
     (el:let (binding)
       (el:let* (bindings ...)
         forms ...))]))

(define-syntax el:letrec
  (syntax-parser
    [(_ (binding:let-binding ...) body ...)
     (syntax-protect
      #'(el:let (binding.id ...)
          (el:setq (~@ binding.id binding.value) ...)
          body ...))]))

(define-syntax el:setq
  (syntax-parser
    [(_ (~seq id:id value:expr) ...)
     (with-syntax ([(set!-form ...)
                    (for/list ([b (in-list (syntax->list #'((id value) ...)))])
                      (syntax-case b ()
                        [(id value)
                         (dynamic-reference? (syntax-local-value #'id (λ () #f)))
                         #'(let ([v value]) (hash-set! global-variables 'id v) v)]
                        [(id value)
                         #'(let ([v value]) (set! id v) v)]))])
       (syntax-protect #'(el:progn set!-form ...)))]))

; setq-default

;; Functions

(define-syntax (el:#%app stx)
  (syntax-case stx ()
    [(_ proc arg ...)
     (syntax-protect
      #`(#%plain-app
         #,(if (identifier? #'proc)
               #'(indirect-function/check 'proc)
               #'proc)
         arg ...))]))

(define (indirect-function name)
  (let loop ([seen '()])
    (define f (hash-ref global-functions name #f))
    (cond
      [(not f) 'nil]
      [(not (symbol? f)) f]
      [(memq f seen) (error "cyclic function indirection:" name)]
      [else (loop (cons f seen))])))

(define (indirect-function/check name)
  (define f (indirect-function name))
  (cond
    [(eq? 'nil f) (error "undefined function:" name)]
    [else f]))

(define-syntax-rule (el:defun id . rest)
  (hash-set! global-functions 'id (el:lambda . rest)))

(define-syntax-rule (el:function f) f)

(struct elisp-procedure (proc doc)
  #:property prop:procedure 0)

(begin-for-syntax
  (define-syntax-class arg-id
    (pattern (~and :id (~not (~or (~datum &optional) (~datum &rest)))))))

(define-syntax el:lambda
  (syntax-parser
    [(_ (arg:arg-id ...
         (~optional (~seq (~datum &optional) opt-arg:arg-id ...))
         (~optional (~seq (~datum &rest) rest-arg:arg-id)))
       (~optional doc:str)
       body:expr ...)
     (syntax-protect
      #'(elisp-procedure
         (λ (arg ... (~? (~@ [opt-arg 'nil] ...)) . (~? rest-arg ()))
           (el:progn body ...))
         (~? doc #'#f)))]))

;; Nonlocal exit

(define current-elisp-throw-handler
  (make-parameter
   (λ (tag val)
     (error (format "no catch for tag: ~a, ~a" tag val)))))

(define (call-with-elisp-catch tag thunk)
  (define super (current-elisp-throw-handler))
  (let/ec return
    (parameterize
        ([current-elisp-throw-handler
          (λ (throw-tag throw-val)
            (if (eq? tag throw-tag)
                (return throw-val)
                (super throw-tag throw-val)))])
      (thunk))))

(define-syntax-rule (el:catch tag body ...)
  (call-with-catch tag (λ () (progn body ...))))

(define (elisp-throw tag val)
  ((current-elisp-throw-handler) tag val))

; unwind-protect

;; Other special forms

(define-syntax el:and
  (syntax-rules ()
    [(_) 't]
    [(_ form) form]
    [(_ form1 forms ...) (el:cond (form1 (el:and forms ...)))]))

(define-syntax el:cond
  (syntax-rules ()
    [(_) 'nil]
    [(_ (test) rest ...)
     (let ([v test])
       (if (eq? 'nil v)
           (el:cond rest ...)
           v))]
    [(_ (test form ...) rest ...)
     (if (eq? 'nil test)
         (el:cond rest ...)
         (el:progn form ...))]))

; condition-case

(define-syntax-rule (el:if test then else ...)
  (if (eq? 'nil test) (el:progn else ...) then))

(define-syntax-rule (el:interactive . _)
  'nil)

(define-syntax-rule (el:or form ...)
  (el:cond (form) ...))

(define-syntax el:progn
  (syntax-rules ()
    [(_) 'nil]
    [(_ form ...) (begin form ...)]))

(define-syntax-rule (el:prog1 form1 forms ...)
  (begin0 form1 forms ...))

(define-syntax-rule (el:prog2 form1 form2 forms ...)
  (begin form1 (begin0 form2 forms ...)))

; save-current-buffer
; save-excursion
; save-restriction

(define-syntax-rule (el:while test forms ...)
  (let loop ()
    (el:if test (el:progn forms ... (loop)))))
