#lang racket

;; sbomcc.rkt - main file

;; —————————————————————————————————
;; import and implementation section

(require
  "local.rkt"
;   "github-user.rkt"
  "parser.rkt"
  "strings.rkt"
  "utils.rkt"
  "web.rkt")

(define (main args)
  (displayln (string-append "sbom.cc " version-slug))
  (cond [(= (vector-length args) 0) (displayln help-text)])
  (cond [(> (string-length (sbom-file-path)) 0) (sbom-parse-file (sbom-file-path))])
;   (cond [(> (string-length (github-user)) 0) (github-user-scan (github-user))])
  (cond [(symlink) (symbolic-link)])
  (cond [(version) (displayln version-slug)])
  (cond [(web) (web-server-start)])
  (values))

(main (current-command-line-arguments))
