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

(define local-sboms '())

(define (sbom-parse-file filepath)
  (define json-data (read-json-file filepath))
  (if (not (web)) 
      (print-json-table json-data)
      (set! local-sboms (append local-sboms (list json-data)))))
