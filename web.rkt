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
  (define (sbom-handler req)
    (let* ([params (request-bindings req)]
           [name (cdr (first params))])
      (if name
          (response/xexpr
            (apply append
              (for/list ([sbom local-sboms])
                (if (equal? (hash-ref sbom 'name) name)
                    `(div
                      (h2 ,(hash-ref sbom 'name))
                      (table
                        (tr
                          (td (b "SPDXID"))
                          (td ,(hash-ref sbom 'SPDXID)))
                        (tr
                          (td (b "spdxVersion"))
                          (td ,(hash-ref sbom 'spdxVersion)))
                        (tr
                          (td (b "dataLicense"))
                          (td ,(hash-ref sbom 'dataLicense)))
                        (tr
                          (td (b "documentDescribes"))
                          (td ,(string-join (hash-ref sbom 'documentDescribes) "<br/>")))
                        (tr
                          (td (b "documentNamespace"))
                          (td ,(hash-ref sbom 'documentNamespace))))
                      (h3 "Creation Information")
                      (table
                        (tr
                          (td (b "created"))
                          (td ,(hash-ref (hash-ref sbom 'creationInfo) 'created)))
                        (tr
                          (td (b "creators"))
                          (td ,(string-join (hash-ref (hash-ref sbom 'creationInfo) 'creators) "<br/>")))))
                    empty))))
          (response/xexpr
            `(p "Name parameter is missing.")))))

  (define (nav-container)
    (define nav-item (nav-items))
    `((div ([class "nav-container"]
            [_ "on load send click to the first .nav-item"])
        ,@nav-item)))

  (define (nav-items)
    (apply append
      (for/list ([sbom local-sboms])
        `((a ([class "nav-item"]
              [href  "#"]
              [hx-get ,(format "/sbom?name=~a" (hash-ref sbom 'name))]
              [hx-target ".main-content"])
          ,(hash-ref sbom 'name))))))

  (define (main-content)
    `((div ([class "main-content"]))))

  (define (sbom-info)
    (apply append
      (for/list ([sbom local-sboms])
        `((h2 ,(hash-ref sbom 'name))))))

  ;; Define the template
  (define (main-template req)
    (define nav (nav-container))
    (define main (main-content))
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

                          .nav-container {
                            position: fixed;
                            left: 0;
                            top: 0;
                            width: 250px;
                            height: 100%;
                            background-color: var(--color-space-cadet);
                            color: var(--color-columbia-blue);
                            overflow: auto;
                            z-index: 1000;
                          }

                          .nav-item {
                            display: block;
                            padding: 10px 15px;
                            text-decoration: none;
                            color: inherit;
                            border-bottom: 1px solid var(--color-ultra-violet);
                          }

                          table {
                            width: 100%;
                            border-collapse: collapse;
                          }

                          .nav-item:hover {
                            background-color: var(--color-ultra-violet);
                            color: var(--color-white);
                          }

                          table, th, td {
                            border: 1px solid var(--color-space-cadet);
                          }

                          .main-content {
                            margin-left: 250px;
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

                          @media (max-width: 768px) {
                            .nav-container {
                              width: 100px;
                            }

                            .nav-item {
                              padding: 10px;
                              font-size: 0.75rem;
                            }

                            .main-content {
                              margin-left: 100px;
                            }

                            .table-responsive {
                              overflow-x: auto;
                            }
                          }")
                  (script ([src "https://unpkg.com/htmx.org@1.9.11"]
                           [integrity "sha384-0gxUXCCR8yv9FM2b+U3FDbsKthCI66oH5IA9fHppQq9DDMHuMauqq1ZHBpJxQ0J0"]
                           [crossorigin "anonymous"]))
                  (script ([src "https://unpkg.com/hyperscript.org@0.9.12"]))
                  (script ([type "text/javascript"])
                          "document.addEventListener('DOMContentLoaded', function () {
                            const toggle = document.querySelector('.dark-mode-toggle');
                            const body = document.body;
                            // Check local storage for theme preference
                            const currentTheme = localStorage.getItem('theme');
                            if (currentTheme) {
                              body.setAttribute('data-theme', currentTheme);
                            }
                            toggle.addEventListener('click', function() {
                              if (body.getAttribute('data-theme') === 'dark') {
                                body.setAttribute('data-theme', 'light');
                                localStorage.setItem('theme', 'light');
                                this.innerText = 'light';
                              } else {
                                body.setAttribute('data-theme', 'dark');
                                localStorage.setItem('theme', 'dark');
                                this.innerText = 'dark';
                              }
                            });
                           });"))
            (body ([data-theme "light"])
                  (button ([class "dark-mode-toggle"])
                          "light")
                  ,@nav
                  ,@main))))

  ;; Define the dispatch rules
  (define-values (app-dispatch app-url)
    (dispatch-rules
     [("sbom") sbom-handler]
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
