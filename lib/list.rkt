#lang racket/base

(require racket/function
         racket/list
         "../util.rkt")

(define (nil? v) (eq? 'nil v))

(define-boolean-aliases
  [consp mpair?]
  [atomp (negate mpair?)]
  [listp (disjoin mpair? nil?)]
  [nlistp (conjoin (negate mpair?) (negate nil?))]
  [null nil?])

(define-aliases
  ;proper-list-p
  [car (λ (v) (if (nil? v) 'nil (mcar v)))]
  [cdr (λ (v) (if (nil? v) 'nil (mcdr v)))]
  [car-safe (λ (v) (if (mpair? v) (mcar v) 'nil))]
  [cdr-safe (λ (v) (if (mpair? v) (mcdr v) 'nil))]
  ; ...
  )
