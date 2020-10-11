#lang racket/base

(require racket/match)

(provide (all-defined-out))

(define global-bindings (make-hasheq))

(define function-bindings (make-hasheq))

(define (get-function f)
  (let loop ([f f])
    (if (symbol? f)
        (loop (hash-ref function-bindings f #f))
        f)))

(struct closure
  (definition bindings)
  #:transparent)

(define (elisp-apply f arg-vals)
  (define-values (definition lexical-bindings)
    (if (closure? f)
        (values (closure-definition f) (closure-bindings f))
        (values f #f)))
  (match-define (list 'lambda arg-decl body ...) definition)
  (define bindings
    (letrec
        ([bind-args
          (λ (decls vals)
            (match decls
              [(cons '&optional rest) (optional rest vals)]
              [(cons '&rest _) (maybe-rest decls vals)]
              [(cons (? symbol? id) rest)
               (when (null? vals) (error "too few arguments"))
               (cons (list id (car vals)) (bind-args rest (cdr vals)))]
              [_ (maybe-rest decls vals)]))]
         [optional
          (λ (decls vals)
            (match decls
              [(cons '&rest _) (maybe-rest decls vals)]
              [(cons (? symbol? id) rest)
               (if (null? vals)
                   (cons (list id 'nil) (optional rest '()))
                   (cons (list id (car vals)) (optional rest (cdr vals))))]
              [_ (maybe-rest decls vals)]))]
         [maybe-rest
          (λ (decls vals)
            (match decls
              [(list '&rest (? symbol? id))
               (list (list id vals))]
              ['()
               (unless (null? vals) (error "too many arguments"))
               '()]
              [_ (error "invalid argument declaration")]))])
      (map (match-lambda [`(,id ,val) `(,id ',val)])
           (bind-args arg-decl arg-vals))))
  (elisp-eval `(let ,bindings ,@body) lexical-bindings))

(define current-elisp-throw-handler
  (make-parameter
   (λ (tag val)
     (error "throw used outside of call-with-elisp-exceptions"))))

(define (call-with-elisp-exceptions thunk)
  (parameterize
      ([current-elisp-throw-handler
        (λ (tag val)
          (error (format "no catch for tag: ~a, ~a" tag val)))])
    (thunk)))

(define (elisp-throw tag val)
  ((current-elisp-throw-handler) tag val))

(define (elisp-eval form [lexical-bindings #f])
  (define (eval form [bindings lexical-bindings])
    (elisp-eval form bindings))
  (define (bindings->alist forms)
    (let loop ([forms forms])
      (match forms
        [(list) '()]
        [(list-rest (list (? symbol? id) val-form) rest)
         (cons (cons id val-form) (loop rest))]
        [(list-rest (or (list (? symbol? id)) (? symbol? id)) rest)
         (cons (cons id ''nil) (loop rest))])))
  (define this-form form)
  (match this-form
    [(list 'and forms ...)
     (let loop ([forms forms]
                [ret 't])
       (if (null? forms)
           ret
           (let ([x (eval (car forms))])
             (if (eq? 'nil x)
                 'nil
                 (loop (cdr forms) x)))))]
    [(list 'catch tag body ...)
     (let ([super (current-elisp-throw-handler)]
           [tag-v (eval tag)])
       (let/ec return
         (parameterize
             ([current-elisp-throw-handler
               (λ (throw-tag throw-val)
                 (if (eq? tag-v throw-tag)
                     (return throw-val)
                     (super throw-tag throw-val)))])
           (eval `(progn ,@body)))))]
    [(list 'cond) 'nil]
    [(list 'cond (list cond forms ...) rest ...)
     (let ([r (eval cond)])
       (if (eq? 'nil r)
           (eval `(cond ,@rest))           
           (if (null? forms) r (eval `(progn ,@forms)))))]
    ; condition-case
    ; defconst
    ; defvar
    [(list 'function form) form]
    [(list 'if cond then elses ...)
     (if (eq? 'nil (eval cond))
         (eval (cons 'progn elses))
         (eval then))]
    [(list 'interactive _ ...) '()]
    [(list 'lambda args forms ...)
     (if lexical-bindings
         (closure this-form lexical-bindings)
         this-form)]
    [(list 'let (list bindings ...) body ...)
     (let*-values
         ([(ids vals)
           (for/lists (ids vals)
                      ([b (in-list bindings)])
             (match b
               [(? symbol? id) (values id 'nil)]
               [(list (? symbol? id)) (values id 'nil)]
               [(list (? symbol? id) val) (values id (eval val))]
               [_ (error "let: invalid binding syntax")]))]
          ; TODO: lexical
          )
       (let ([old-vs (map (λ (id) (hash-ref global-bindings id #f)) ids)])
         (dynamic-wind
          (λ ()
            (for ([id ids]
                  [val vals])
              (hash-set! global-bindings id val)))
          (λ ()
            (eval `(progn ,@body)))
          (λ ()
            (for ([id ids]
                  [val old-vs])
              (if val
                  (hash-set! global-bindings id val)
                  (hash-remove! global-bindings id)))))))]
    [(list 'let* (list bindings ...) body ...)
     (eval (foldr (λ (b e) `(let (,b) ,e))
                  `(progn ,@body)
                  bindings))]
    [(list 'letrec (list bindings ...) body ...)
     (let ([bindings (bindings->alist bindings)])
       (eval `(let ,(map car bindings)
                (setq ,@(foldl (λ (b rest)
                                 (list* (car b) (cdr b) rest))
                               bindings))
                ,@body)))]
    [(list 'or forms ...)
     (let loop ([forms forms])
       (if (null? forms)
           'nil
           (let ([x (eval (car forms))])
             (if (eq? 'nil x)
                 (loop (cdr forms))
                 x))))]
    [(list 'prog1 f forms ...) (begin0 (eval f) (for-each eval forms))]
    [(list 'prog2 f1 f2 forms ...)
     (let* ([r1 (eval f1)]
            [r2 (eval f2)])
       (for-each eval forms)
       r2)]
    [(list 'progn) 'nil]
    [(list 'progn forms ...) (for/last ([f (in-list forms)]) (eval f))]
    [(list 'quote f) f]
    ; save-current-buffer
    ; save-excursion
    ; save-restriction
    [(list 'setq forms ...)
     (let loop ([forms forms]
                [ret 'nil])
       (match forms
         [(list-rest (? symbol? sym) val-form rest)
          (let ([val (eval val-form)])
            (hash-set! global-bindings sym val)
            (loop rest val))]
         [_ (error "setq: wrong number of arguments")]))]
    ; setq-default
    [(list 'unwind-protect body cleanup ...)
     (dynamic-wind
      void
      (λ () (eval body))
      (λ () (for-each eval cleanup)))]
    [(list 'while cond forms ...)
     (let loop ()
       (if (eq? 'nil (eval cond))
           '()
           (begin
             (for-each eval forms)
             (loop))))]
    [(list op args ...)
     (match (get-function op)
       [#f (error "function not defined")]
       [(cons 'macro f) (eval (elisp-apply f args))]
       [(? procedure? f) (apply f (map eval args))]
       [f (elisp-apply f (map eval args))])]
    [(? symbol? sym) (hash-ref global-bindings sym)]
    ['() 'nil]
    [else this-form]))
