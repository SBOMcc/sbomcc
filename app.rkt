#lang racket

;; sbomcc

;; -------
;; imports
;; -------

(require
  helpful
  web-server/dispatch
  web-server/servlet-env
  web-server/http
  web-server/templates)

;; ----------------
;; request handlers
;; ----------------

(define (start req)
  (response/output
    (lambda (op) (display (include-template "src/index.html") op))))

; (define (get-title req)
;   (response/xexpr
;     `(h1
;       ([class "font-normal text-gray-900 text-4xl md:text-7xl leading-none mb-8"]
;        [id "title"])
;       ,(random-title title-header))))

; (define (get-lao-tzu req)
;   (response/xexpr
;     `(h1
;       ([class "font-normal text-gray-300 text-3xl md:text-6xl lg:text-7xl"]
;        [id "lao-tzu"])
;       ,(random-quote lao-tzu-quotes))))

(define-values (api-dispatch api-url)
    (dispatch-rules
      [("") start]))

(serve/servlet
  api-dispatch
  #:command-line? #t
  #:extra-files-paths (list 
                        (build-path 'same "src" "assets" "css") 
                        (build-path 'same "src" "assets" "image") 
                        (build-path 'same "src"))
  #:listen-ip "0.0.0.0"
  #:port 8080
  #:servlet-path ""
  #:servlet-regexp #rx"")
