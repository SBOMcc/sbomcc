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
  "local.rkt"
  web-server/servlet
  web-server/servlet-env
  web-server/dispatch
  web-server/templates)

(define (web-server-start)
  (printf "Starting web server on port 8080\n")
  ; (printf "~a\n" local-sboms)
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

  (define (sbom-info)
    (apply append
      (for/list ([sbom local-sboms])
        `((h2 ,(hash-ref sbom 'SPDXID))))))

  ;; Define the template
  (define (main-template req)
    (define info (sbom-info))
    (response/xexpr
     `(html (head (title "SBOM.cc")
                  (style ([type "text/css"])
                         ":root { 
                            --color-black: #000505;
                            --color-space-cadet: #3b3355;
                            --color-ultra-violet: #5d5d81;
                            --color-columbia-blue: #bfcde0;
                            --color-white: #fefcfd;

                            --background-color: var(--color-columbia-blue);
                            --text-color: var(--color-space-cadet);
                            --table-background: var(--color-white);
                            --table-text: var(--color-black);
                          }

                          [data-theme=\"dark\"] {
                            --background-color: var(--color-space-cadet);
                            --text-color: var(--color-columbia-blue);
                            --table-background: var(--color-ultra-violet);
                            --table-text: var(--color-columbia-blue);
                          }

                          body {
                            background-color: var(--background-color);
                            color: var(--text-color);
                            font-family: Arial, sans-serif;
                          }

                          table {
                            width: 100%;
                            border-collapse: collapse;
                          }

                          table, th, td {
                            border: 1px solid var(--color-space-cadet);
                          }

                          th, td {
                            text-align: left;
                            padding: 8px;
                          }

                          th {
                            background-color: var(--color-ultra-violet);
                            color: var(--color-white);
                          }

                          td {
                            background-color: var(--table-background);
                            color: var(--table-text);
                          }

                          tr:nth-child(even) {
                            background-color: var(--color-columbia-blue);
                          }

                          .dark-mode-toggle {
                            position: fixed;
                            top: 10px;
                            right: 10px;
                            cursor: pointer;
                            padding: 10px;
                            background-color: var(--color-ultra-violet);
                            color: var(--color-white);
                            border: none;
                            border-radius: 5px;
                          }

                          /* Example media query for responsiveness */
                          @media (max-width: 600px) {
                            .table-responsive {
                              overflow-x: auto;
                            }
                          }"))              
            (body (h1 "SBOM.cc")
                  (p "SBOM Parsing for Humans")
                  ,@info))))

  ;; Define the dispatch rules
  (define-values (app-dispatch app-url)
    (dispatch-rules
     [("home") home-handler]
     [("") main-template]))

  ;; Define the web server entry point
  (serve/servlet app-dispatch
                  #:command-line? #t
                  #:listen-ip "0.0.0.0"
                ;  #:extra-files-paths (list 
                ;                       (build-path 'same "src" "static" "css") 
                ;                       (build-path 'same "src" "static" "image") 
                ;                       (build-path 'same "src"))
                  #:servlet-path ""
                  #:servlet-regexp #rx""
                  #:port 8080
                  #:launch-browser? #f))
