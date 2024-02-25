#lang racket

;; sbomcc.rkt - main file

;; —————————————————————————————————
;; import and implementation section

(require
;   "local.rkt"
;   "github-user.rkt"
  "parser.rkt"
  "strings.rkt")

(define (main args)
  (displayln (string-append "sbom.cc " version-slug))
  (cond [(= (vector-length args) 0) (displayln help-text)])
;   (cond [(> (string-length (local-path)) 0) (local-scan (local-path))])
;   (cond [(> (string-length (github-user)) 0) (github-user-scan (github-user))])
;   (cond [(version) (displayln version-slug)])
  (values))

(main (current-command-line-arguments))