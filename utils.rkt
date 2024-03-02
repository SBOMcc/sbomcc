#lang racket

;; util.rkt - Utility functions

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(require
  json
  "parser.rkt")

;; Terminal escape sequences
(define black "\x1B[30m")
(define red "\x1B[31m")
(define green "\x1B[32m")
(define yellow "\x1B[33m")
(define blue "\x1B[34m")
(define magenta "\x1B[35m")
(define cyan "\x1B[36m")
(define white "\x1B[37m")
(define bold "\e[1m")
(define reset "\e[0m")
(define remove-previous-line "\e[A\e[2K")

;; Function to read JSON from a file
(define (read-json-file filepath)
  (with-input-from-file filepath
    (lambda ()
      (let ((json-content (port->string)))
        (string->jsexpr json-content)))))

(define (print-json-table json-data)
  (define exclusions (parse-comma-lists (exclude-sbom-section)))
  (define inclusions (parse-comma-lists (include-sbom-section)))
  (define (key-match? key key-symbol)
    (cond [(and (symbol=? key key-symbol) (> (length inclusions) 0) (member (symbol->string key-symbol) inclusions)) #t]
          [(and (symbol=? key key-symbol) (> (length inclusions) 0) (not (member (symbol->string key-symbol) inclusions))) #f]
          [(and (symbol=? key key-symbol) (> (length exclusions) 0) (member (symbol->string key-symbol) exclusions)) #f]
          [(and (symbol=? key key-symbol) (> (length exclusions) 0) (not (member (symbol->string key-symbol) exclusions))) #t]
          [else (symbol=? key key-symbol)]))
  (define (filter-match? v)
    (if (> (string-length (sbom-filter)) 0)
        (begin
          (cond [(string? v)
                 (let* ([filter-regexp (sbom-filter-regexp (sbom-filter))]
                        [result (regexp-match filter-regexp v)])
                   (if result #t #f))]
                [(or (boolean? v)) #f]                
                [else #t]))
        #t))
  (define (key-filter-match? key key-symbol v)
    (if (and (key-match? key key-symbol) (filter-match? v)) #t #f))
  (if (hash? json-data)
      (begin
        (newline)
        (for ([pair (hash->list json-data)])
          (let ([key (car pair)] [value (cdr pair)])
            (cond [(key-filter-match? key 'name value)
                   (printf "~a~aName~a\t\t\t~a\n" bold magenta reset value)]
                  [(key-filter-match? key 'spdxVersion value)
                   (printf "~a~aSPDX Version~a\t\t~a\n" bold magenta reset value)]
                  [(key-filter-match? key 'dataLicense value)
                   (printf "~a~aData License~a\t\t~a\n" bold magenta reset value)]
                  [(key-filter-match? key 'documentNamespace value)
                   (printf "~a~aDocument Namespace~a\t~a\n" bold magenta reset value)]
                  [(key-filter-match? key 'creationInfo value)
                   (begin
                     (printf "~a~aCreation Info~a\n" bold magenta reset)
                     (define has-results? #f)
                     (for ([pair (hash->list value)])
                       (let ([k (car pair)] [v (cdr pair)])
                         (cond [(key-filter-match? k 'created v)
                                (begin (set! has-results? #t) (printf "\t~aCreated~a\t\t~a\n" bold reset v))]
                               [(key-match? k 'creators)
                                (begin
                                  (printf "\t~aCreators~a\n" bold reset)
                                  (define has-results? #f)
                                  (for ([creator v])
                                    (cond [(filter-match? creator)
                                           (begin (set! has-results? #t) (printf "\t\t\t~a\n" creator))]))
                                  (cond [(not has-results?) (printf "~a" remove-previous-line)]))])))
                       (cond [(not has-results?) (printf "~a" remove-previous-line)]))]
                  [(key-filter-match? key 'documentDescribes value)
                   (begin
                     (printf "~a~aDocument Describes~a\n" bold magenta reset)
                     (define has-results? #f)
                     (for ([doc value])
                       (cond [(filter-match? doc)
                              (begin (set! has-results? #t) (printf "\t\t\t~a\n" doc))]))
                     (cond [(not has-results?) (printf "~a" remove-previous-line)]))]
                  [(key-filter-match? key 'packages value)
                   (begin
                     (printf "~a~aPackages~a\n" bold magenta reset)
                     (for ([package value])
                       (define sep #f)
                       (for ([pair (hash->list package)])
                         (let ([k (car pair)] [v (cdr pair)])
                           (cond [(key-filter-match? k 'name v)
                                  (begin (set! sep #t) (printf "\t~aName~a\t\t\t~a\n" bold reset v))]
                                 [(key-filter-match? k 'SPDXID v)
                                  (begin (set! sep #t) (printf "\t~aSPDX ID~a\t\t\t~a\n" bold reset v))]
                                 [(key-filter-match? k 'versionInfo v)
                                  (begin (set! sep #t) (printf "\t~aVersion~a\t\t\t~a\n" bold reset v))]
                                 [(key-filter-match? k 'downloadLocation v)
                                  (begin (set! sep #t) (printf "\t~aDownload Location~a\t~a\n" bold reset v))]
                                 [(key-filter-match? k 'filesAnalyzed v)
                                  (begin (set! sep #t) (printf "\t~aFiles Analyzed~a\t\t~a\n" bold reset v))]
                                 [(key-filter-match? k 'licenseConcluded v)
                                  (begin (set! sep #t) (printf "\t~aLicense Concluded~a\t~a\n" bold reset v))]
                                 [(key-filter-match? k 'supplier v)
                                  (begin (set! sep #t) (printf "\t~aSupplier~a\t\t~a\n" bold reset v))]
                                 [(key-match? k 'externalRefs)
                                  (begin
                                    (printf "\t~aExternal Refs~a\n" bold reset)
                                    (define has-results? #f)
                                    (for ([ref v])
                                      (for ([pair (hash->list ref)])
                                        (let ([k (car pair)] [v (cdr pair)])
                                          (cond [(key-filter-match? k 'referenceCategory v)
                                                 (begin (set! sep #t) (set! has-results? #t) (printf "\t\t~aReference Category~a\t~a\n" blue reset v))]
                                                [(key-filter-match? k 'referenceType v)
                                                 (begin (set! sep #t) (set! has-results? #t) (printf "\t\t~aReference Type~a\t\t~a\n" blue reset v))]
                                                [(key-filter-match? k 'referenceLocator v)
                                                 (begin (set! sep #t) (set! has-results? #t) (printf "\t\t~aReference Locator~a\t~a\n" blue reset v))]))))
                                      (cond [(not has-results?) (printf "~a" remove-previous-line)]))])))
                       (cond [sep (printf "\t~a--------------------------------------------------~a\n" red reset)])))]
                  [(key-filter-match? key 'relationships value)
                    (begin
                      (printf "~a~aRelationships~a\n" bold magenta reset)
                      (define has-results? #f)
                      (for ([relationship value])
                        (define sep #f)
                        (for ([pair (hash->list relationship)])
                          (let ([k (car pair)] [v (cdr pair)])
                            (cond [(key-filter-match? k 'relationshipType v)
                                    (begin (set! sep #t) (set! has-results? #t) (printf "\t~aRelationship Type~a\t~a\n" bold reset v))]
                                  [(key-filter-match? k 'spdxElementId v)
                                    (begin (set! sep #t) (set! has-results? #t) (printf "\t~aSPDX Element ID~a\t\t~a\n" bold reset v))]
                                  [(key-filter-match? k 'relatedSpdxElement v)
                                    (begin (set! sep #t) (set! has-results? #t) (printf "\t~aRelated SPDX Element~a\t~a\n" bold reset v))])))
                        (cond [sep (printf "\t~a--------------------------------------------------~a\n" red reset)])))
                      (cond [(not has-results?) (printf "~a" remove-previous-line)])]))))
      (error "Input is not a hash.")))

(define (parse-comma-lists comma-string)
  (string-split comma-string ","))

(define (sbom-filter-regexp filter-string)
  (regexp filter-string))