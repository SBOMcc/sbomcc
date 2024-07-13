#lang racket

;; parser.rkt - Command line options parameters and parser.

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

; Parameters
; (define output-format (make-parameter ""))
; (define github-owner (make-parameter ""))
; (define github-token (make-parameter ""))
(define exclude-sbom-section (make-parameter ""))
(define sbom-filter (make-parameter ""))
(define include-sbom-section (make-parameter ""))
(define sbom-file-path (make-parameter ""))
; (define silent (make-parameter #f))
; (define debug (make-parameter #f))
; (define verbose (make-parameter #f))
(define symlink (make-parameter #f))
(define version (make-parameter #f))
(define web (make-parameter #f))

; Command line parser
(define parser
  (command-line
    #:usage-help
    "sbom.cc - SBOM Parsing For Humans"
    #:once-each
    [("-f" "--file") FILE "SBOM File Path" (sbom-file-path FILE)]
    [("-i" "--include") INCLUDE "Include SBOM Section(s)" (include-sbom-section INCLUDE)]
    [("-x" "--exclude") EXCLUDE "Exclude SBOM Section(s)" (exclude-sbom-section EXCLUDE)]
    [("-t" "--filter") FILTER "Filter using text or regular expression" (sbom-filter FILTER)]
    ; [("--github-owner") OWNER "GitHub Repo Scan by Owner" (github-owner OWNER)]
    ; [("-d" "--debug") "Debug" (debug #t)]
    ; [("-v" "--verbose") "Verbose" (verbose #t)]
    ; [("-s" "--silent") "Silent" (silent #t)]
    ; [("-t" "--token") TOKEN "GitHub Token" (github-token TOKEN)]
    ; [("-f" "--format") FORMAT "Output format" (output-format FORMAT)]
    [("-w" "--web") "Web server mode" (web #t)]
    #:once-any
    [("--version") "sbomcc version" (version #t)]
    [("--symlink") "Create symbolic link in /usr/local/bin" (symlink #t)]))
