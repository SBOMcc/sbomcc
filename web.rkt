#lang racket

;; web.rkt - Web Server for sbom.cc

;; Ask and I shall provide
(provide
  (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(require 
  "parser.rkt"
  "utils.rkt"
  web-server/servlet
  web-server/servlet-env
  web-server/dispatch
  web-server/templates)

(define (web-server-start)
  (printf "Starting web server on port 8080\n"))
  ;; Define the static file directories
  (define static-dir (build-path (current-directory) "static"))
  (define css-dir (build-path static-dir "css"))
  (define img-dir (build-path static-dir "img"))
  (define js-dir (build-path static-dir "js"))

  ;; Define the endpoints
  (define (home-handler req)
    (response/xexpr
     `(html (head (title "Home"))
            (body (h1 "Welcome to the Home Page!")))))

  (define (about-handler req)
    (response/xexpr
     `(html (head (title "About"))
            (body (h1 "About Us")))))

  ;; Define the template
  (define main-template
    (response/xexpr
     `(html (head (title "Web Server"))
            (body (h1 "Web Server")
                  (p "This is a simple web server.")))))

  ;; Define the dispatch rules
  (define-values (app-dispatch app-url)
    (dispatch-rules
     [("/home") home-handler]
     [("/about") about-handler]
     [("/") main-template]))

  ;; Define the web server entry point
  (define (web-server)
    (serve/servlet app-dispatch
                   #:servlet-path "/"
                   #:port 8080
                   #:launch-browser? #f))

  (web-server)
