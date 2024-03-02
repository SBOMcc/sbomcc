#lang racket

;; util.rkt - Utility functions

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(define (parse-comma-lists comma-string)
  (string-split comma-string ","))