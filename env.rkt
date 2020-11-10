#lang racket/base

(require racket/flonum
         racket/function
         racket/math
         "kernel.rkt"
         "util.rkt")

; TODO: need to convert racket errors to elisp errors for library functions

; XXX: we really need to split this into multiple files in lib/
; and use (defun)-like syntax to allow for compilation later on

(hash-set*! global-functions
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
  (λ (sym) (hash-ref global-functions sym 'nil))
  'fboundp
  (λ (sym) (if (hash-has-key? global-functions sym) 't 'nil))
  'fmakunbound
  (λ (sym) (hash-remove! global-functions sym) sym)
  'fset
  (λ (sym def) (hash-set! global-functions sym def) def)
  ;; Macros
  ;; Customization
  ;; Loading
  ;; Byte Compilation
  ;; Debugging
  ;; Read and Print
  
  )