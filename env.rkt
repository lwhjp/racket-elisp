#lang racket/base

(require racket/flonum
         racket/function
         racket/math
         "eval.rkt")

; TODO: need to convert racket errors to elisp errors for library functions

(hash-set*! global-bindings
  't 't
  'nil 'nil
  )

; XXX: we really need to split this into multiple files in lib/
; and use (defun)-like syntax to allow for compilation later on

(hash-set*! function-bindings
  ;; Strings and Characters
  'stringp (wrap-bool string?)
  ;'string-or-null-p
  ;'char-or-string-p
  'make-string make-string
  'string string
  ;substring
  ;substring-no-properties
  ;concat
  ;split-string
  ;store-substring
  ;clear-string FIXME: mutability
  ; 4.5
  ; 4.6
  ; 4.7
  ; 4.8
  ; ...
  ;; Lists
  ;; Sequences, Arrays, Vectors
  ;; Records
  ;; Hash Tables
  ;; Symbols
  ;; Evaluation
  ;; Control Structures
  ;; Variables
  ;; Functions
  'symbol-function
  (λ (sym) (hash-ref function-bindings sym 'nil))
  'fboundp
  (λ (sym) (if (hash-has-key? function-bindings sym) 't 'nil))
  'fmakunbound
  (λ (sym) (hash-remove! function-bindings sym) sym)
  'fset
  (λ (sym def) (hash-set! function-bindings sym def) def)
  ;; Macros
  ;; Customization
  ;; Loading
  ;; Byte Compilation
  ;; Debugging
  ;; Read and Print
  
  )