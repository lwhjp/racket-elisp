#lang racket/base

(require (for-syntax racket/base)
         racket/provide
         "kernel.rkt"
         "env.rkt"
         "lib/list.rkt"
         "lib/number.rkt")

(provide
 (filtered-out
  (λ (n)
    (and (regexp-match? #rx"^el:" n)
         (substring n 3)))
  (all-from-out "kernel.rkt")))

; TODO: reader