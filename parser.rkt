#lang racket

;; parser.rkt - Command line options parameters and parser.

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

; Parameters
(define output-format (make-parameter ""))
(define github-owner (make-parameter ""))
(define github-token (make-parameter ""))
(define local-path (make-parameter ""))
(define silent (make-parameter #f))
(define debug (make-parameter #f))
(define verbose (make-parameter #f))
(define version (make-parameter #f))

; Command line parser
(define parser
  (command-line
    #:usage-help
    "Bogu - The Secret Scanner"
    #:once-each
    [("-p" "--path") PATH "Local scan path" (local-path PATH)]
    [("--github-owner") OWNER "GitHub Repo Scan by Owner" (github-owner OWNER)]
    [("-d" "--debug") "Debug" (debug #t)]
    [("-v" "--verbose") "Verbose" (verbose #t)]
    [("-s" "--silent") "Silent" (silent #t)]
    [("-t" "--token") TOKEN "GitHub Token" (github-token TOKEN)]
    [("-f" "--format") FORMAT "Output format" (output-format FORMAT)]
    #:once-any
    [("--version") "Version" (version #t)]))

