#lang racket

;; local.rkt - Local SBOM file parsing

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(require 
  json
  "parser.rkt"
  "utils.rkt")

;; Function to read JSON from a file
(define (read-json-file filepath)
  (with-input-from-file filepath
    (lambda ()
      (let ((json-content (port->string)))
        (string->jsexpr json-content)))))

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

(define (print-json-table json-data)
  (define exclusions (parse-comma-lists (exclude-sbom-section)))
  (define inclusions (parse-comma-lists (include-sbom-section)))
  (define (key-match? key key-symbol)
    (cond [(and (symbol=? key key-symbol) (> (length inclusions) 0) (member (symbol->string key-symbol) inclusions)) #t]
          [(and (symbol=? key key-symbol) (> (length inclusions) 0) (not (member (symbol->string key-symbol) inclusions))) #f]
          [(and (symbol=? key key-symbol) (> (length exclusions) 0) (member (symbol->string key-symbol) exclusions)) #f]
          [(and (symbol=? key key-symbol) (> (length exclusions) 0) (not (member (symbol->string key-symbol) exclusions))) #t]
          [else #f]))
  (if (hash? json-data)
      (begin
        (newline)
        (for ([pair (hash->list json-data)])
          (let ([key (car pair)] [value (cdr pair)])
            (cond [(key-match? key 'name)
                   (printf "~a~aName~a\t\t\t~a\n" bold magenta reset value)]
                  [(key-match? key 'spdxVersion)
                   (printf "~a~aSPDX Version~a\t\t~a\n" bold magenta reset value)]
                  [(key-match? key 'dataLicense)
                   (printf "~a~aData License~a\t\t~a\n" bold magenta reset value)]
                  [(key-match? key 'documentNamespace)
                   (printf "~a~aDocument Namespace~a\t~a\n" bold magenta reset value)]
                  [(key-match? key 'creationInfo)
                   (begin
                     (printf "~a~aCreation Info~a\n" bold magenta reset)
                     (for ([pair (hash->list value)])
                       (let ([k (car pair)] [v (cdr pair)])
                         (cond [(key-match? k 'created)
                                (printf "\t~aCreated~a\t\t~a\n" bold reset v)]
                               [(key-match? k 'creators)
                                (begin
                                  (printf "\t~aCreators~a\n" bold reset)
                                  (for ([creator v])
                                    (printf "\t\t\t~a\n" creator)))]))))]
                  [(key-match? key 'documentDescribes)
                   (begin
                     (printf "~a~aDocument Describes~a\n" bold magenta reset)
                     (for ([creator value])
                       (printf "\t\t\t~a\n" creator)))]
                  [(key-match? key 'packages)
                   (begin
                     (printf "~a~aPackages~a\n" bold magenta reset)
                     (for ([package value])
                       (for ([pair (hash->list package)])
                         (let ([k (car pair)] [v (cdr pair)])
                           (cond [(key-match? k 'name)
                                  (printf "\t~aName~a\t\t\t~a\n" bold reset v)]
                                 [(key-match? k 'SPDXID)
                                  (printf "\t~aSPDX ID~a\t\t\t~a\n" bold reset v)]
                                 [(key-match? k 'versionInfo)
                                  (printf "\t~aVersion~a\t\t\t~a\n" bold reset v)]
                                 [(key-match? k 'downloadLocation)
                                  (printf "\t~aDownload Location~a\t~a\n" bold reset v)]
                                 [(key-match? k 'filesAnalyzed)
                                  (printf "\t~aFiles Analyzed~a\t\t~a\n" bold reset v)]
                                 [(key-match? k 'licenseConcluded)
                                  (printf "\t~aLicense Concluded~a\t~a\n" bold reset v)]
                                 [(key-match? k 'supplier)
                                  (printf "\t~aSupplier~a\t\t~a\n" bold reset v)]
                                 [(key-match? k 'externalRefs)
                                  (begin
                                    (printf "\t~aExternal Refs~a\n" bold reset)
                                    (for ([ref v])
                                      (for ([pair (hash->list ref)])
                                        (let ([k (car pair)] [v (cdr pair)])
                                          (cond [(key-match? k 'referenceCategory)
                                                 (printf "\t\tReference Category\t~a\n" v)]
                                                [(key-match? k 'referenceType)
                                                 (printf "\t\tReference Type\t\t~a\n" v)]
                                                [(key-match? k 'referenceLocator)
                                                 (printf "\t\tReference Locator\t~a\n" v)])))))])))
                       (printf "\t~a--------------------------------------------------~a\n" red reset)))]
                  [(key-match? key 'relationships)
                    (begin
                      (printf "~a~aRelationships~a\n" bold magenta reset)
                      (for ([relationship value])
                        (for ([pair (hash->list relationship)])
                          (let ([k (car pair)] [v (cdr pair)])
                            (cond [(key-match? k 'relationshipType)
                                    (printf "\t~aRelationship Type~a\t~a\n" bold reset v)]
                                  [(key-match? k 'spdxElementId)
                                    (printf "\t~aSPDX Element ID~a\t\t~a\n" bold reset v)]
                                  [(key-match? k 'relatedSpdxElement)
                                    (printf "\t~aRelated SPDX Element~a\t~a\n" bold reset v)])))
                        (printf "\t~a--------------------------------------------------~a\n" red reset)))]))))
      (error "Input is not a hash.")))

(define (sbom-parse-file filepath)
  (define json-data (read-json-file filepath))
  (print-json-table json-data))