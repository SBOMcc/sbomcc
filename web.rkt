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
                          (td ([class "row-header"]) (b "SPDXID"))
                          (td ,(hash-ref sbom 'SPDXID)))
                        (tr
                          (td ([class "row-header"]) (b "spdxVersion"))
                          (td ,(hash-ref sbom 'spdxVersion)))
                        (tr
                          (td ([class "row-header"]) (b "dataLicense"))
                          (td ,(hash-ref sbom 'dataLicense)))
                        (tr
                          (td ([class "row-header"]) (b "documentDescribes"))
                          (td ,(string-join (hash-ref sbom 'documentDescribes) "<br/>")))
                        (tr
                          (td ([class "row-header"]) (b "documentNamespace"))
                          (td ,(hash-ref sbom 'documentNamespace))))
                      (h3 "Creation Information")
                      (table
                        (tr
                          (td ([class "row-header"]) (b "created"))
                          (td ,(hash-ref (hash-ref sbom 'creationInfo) 'created)))
                        (tr
                          (td ([class "row-header"]) (b "creators"))
                          (td ,(string-join (hash-ref (hash-ref sbom 'creationInfo) 'creators) "<br/>"))))
                      (h3 "Packages")
                      (table
                        (tr
                          (th "SPDXID")
                          (th "Name")
                          (th "Version")
                          (th "Download Location")
                          (th "Files Analyzed")
                          (th "License")
                          (th "Supplier")
                          (th "External Refs"))
                          ,@(for/list ([pkg (hash-ref sbom 'packages)])
                            `(tr
                              (td ,(hash-ref pkg 'SPDXID))
                              (td ,(hash-ref pkg 'name))
                              (td ,(hash-ref pkg 'versionInfo))
                              (td ,(hash-ref pkg 'downloadLocation))
                              (td ,(format "~a" (if (hash-ref pkg 'filesAnalyzed) "true" "false")))
                              (td ,(if (hash-has-key? pkg 'licenseConcluded) (hash-ref pkg 'licenseConcluded) ""))
                              (td ,(hash-ref pkg 'supplier))
                              (td ,@(for/list ([ref (hash-ref pkg 'externalRefs)]) 
                                              `(span 
                                                ,(hash-ref ref 'referenceCategory)
                                                (br)
                                                ,(hash-ref ref 'referenceLocator)
                                                (br)
                                                ,(hash-ref ref 'referenceType)
                                                (br)))))))
                      (h3 "Relationships")
                      (table
                        (tr
                          (th "Relationship Type")
                          (th "SPDX Element ID")
                          (th "Related SPDX Element"))
                        ,@(for/list ([rel (hash-ref sbom 'relationships)])
                          `(tr
                            (td ,(hash-ref rel 'relationshipType))
                            (td ,(hash-ref rel 'spdxElementId))
                            (td ,(hash-ref rel 'relatedSpdxElement))))))
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
                            --color-black: #000;
                            --color-gray: #222;
                            --color-grayer: #444;
                            --color-white: #fff;
                            --color-offwhite: #ddd;

                            --background-color: var(--color-white);
                            --text-color: var(--color-black);
                            --table-background: var(--color-white);
                            --table-text: var(--color-black);
                            --hover-color: var(--color-offwhite);
                            --toggle-background: var(--color-black);
                            --toggle-text: var(--color-white);
                          }

                          [data-theme=\"dark\"] {
                            --background-color: var(--color-black);
                            --text-color: var(--color-white);
                            --table-background: var(--color-black);
                            --table-text: var(--color-white);
                            --hover-color: var(--color-grayer);
                            --toggle-background: var(--color-white);
                            --toggle-text: var(--color-black);
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
                            background-color: var(--background-color);
                            border-right: 1px solid var(--color-grayer);
                            color: var(--text-color);
                            overflow: auto;
                            z-index: 1000;
                          }

                          .nav-item {
                            display: block;
                            padding: 10px 15px;
                            text-decoration: none;
                            color: inherit;
                            border-bottom: 1px solid var(--color-grayer);
                          }

                          table {
                            width: 100%;
                            border-collapse: collapse;
                          }

                          .nav-item:hover {
                            background-color: var(--hover-color);
                            color: var(--text-color);
                          }

                          table, th, td {
                            border: 1px solid var(--color-grayer);
                          }

                          .main-content {
                            margin-left: 250px;
                          }

                          th, td {
                            text-align: left;
                            padding: 8px;
                          }

                          td {
                            background-color: var(--table-background);
                            color: var(--table-text);
                          }

                          .row-header, th {
                            background-color: var(--color-gray);
                            color: var(--color-white);
                          }

                          tr:nth-child(even) {
                            background-color: var(--color-white);
                          }

                          .dark-mode-toggle {
                            position: fixed;
                            top: 10px;
                            right: 10px;
                            cursor: pointer;
                            padding: 10px;
                            background-color: var(--toggle-background);
                            color: var(--toggle-text);
                            border: none;
                            ; border-radius: 0px;
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