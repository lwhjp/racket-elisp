#lang racket/base

(require "eval.rkt")

(provide (all-defined-out))

(define-syntax-rule (defconst sym val)
  (defvar sym val))

(define-syntax-rule (defvar sym val)
  (hash-set! global-bindings 'sym val))

(define (wrap-bool f)
  (λ args (if (apply f args) 't 'nil)))

(define-syntax-rule (defalias name def)
  (hash-set! function-bindings 'name def))

(define-syntax-rule (define-aliases [name def] ...)
  (begin (defalias name def) ...))

(define-syntax-rule (define-boolean-aliases [name def] ...)
  (define-aliases [name (wrap-bool def)] ...))
