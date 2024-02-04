#lang racket

;; strings.rkt - Strings used throughout sbomcc

;; Ask and I shall provide
(provide
  all-defined-out)

;; —————————————————————————————————
;; import and implementation section

(define version-slug "v0.0.1")

(define help-text (format "sbomcc - ~a
SBOM Parsing for Humans

Use -h|--help for more details." version-slug))

(define (parse-start-text scan-type)
  (format "\nStarting ~a parse...\n" scan-type))

(define wtf-text "Not sure what to do with arguments provided.\n\nUse -h|--help for more details.")