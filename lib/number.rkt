#lang racket/base

(require racket/flonum
         racket/function
         racket/math
         "../util.rkt")

(defconst float-e (exp 1))
(defconst float-pi pi)

(define-boolean-aliases
  [isnan nan?]
  ;frexp
  ;ldexp
  ;copysign
  ;logb
  ;bignump
  [fixnump fixnum?]
  [floatp inexact?]
  [integerp exact-integer?]
  [numberp number?]
  [natnump natural?]
  [zerop zero?]
  [= =]
  [eql eqv?]
  [/= (negate =)]
  [< <]
  [<= <=]
  [> >]
  [>= >=])

(define-aliases
  [wholenump 'natural]
  [max max]
  [min min]
  [abs abs]
  [float exact->inexact]
  ; FIXME: divisor arg
  [truncate exact-truncate]
  [floor exact-floor]
  [ceiling exact-ceiling]
  [round exact-round]
  [1+ add1]
  [1- sub1]
  [+ +]
  [- -]
  [* *]
  [/ /] ; FIXME: integer division
  [% remainder]
  [mod modulo] ; FIXME: floats
  [ffloor flfloor]
  [fceiling flceiling]
  [ftrunctate fltruncate]
  [fround flround]
  ; ash
  ; lsh
  ; logand
  ;logior
  ;logxor
  ;lognot
  ;logcount
  [sin sin]
  [cos cos]
  [tan tan]
  [asin asin]
  [acos acos]
  [atan atan]
  [exp exp]
  [log log]
  [expt expt]
  [sqrt sqrt] ; FIXME: no complex
  ;random
  )
