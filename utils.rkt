#lang racket

;; util.rkt - Utility functions

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(define (parse-exclusions exclusion-string)
  (string-split exclusion-string ","))