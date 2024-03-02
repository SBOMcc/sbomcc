#lang racket

;; local.rkt - Local SBOM file parsing

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(require 
  "parser.rkt"
  "utils.rkt")

(define (sbom-parse-file filepath)
  (define json-data (read-json-file filepath))
  (print-json-table json-data))
