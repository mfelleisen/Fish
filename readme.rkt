#lang racket

;; create the README.md file from README.source and the directories 

(define (main)
  (define untracked (git-status-check))
  (define dirs0 (for/set ([fd (directory-list)] #:when (directory-exists? fd)) fd))
  (define adirs (map path->string (remove-dots (set->list (set-remove dirs0 untracked)))))
  (define purps (purpose-statements adirs))
  (copy-file "README.source" "README.md" 'delete-existing-one)
  (with-output-to-file "README.md"
    #:exists 'append
    (λ () (printf (make-table adirs purps))))
  (system "open README.md"))

(define (make-table adirs purps)
  (define content
    (for/list ([d adirs] [p purps])
      (~a "| [" d "](" d "/README.md)" " | " p " | \n")))
  (apply string-append header content))

#; {[Listof PathString] -> [Listof String]}
(define (purpose-statements l)
  (for/list ([d l])
    (with-input-from-file (build-path d "README.md")
      (λ ()
        (string-trim (caddr (port->lines)))))))

#; {[Listof PathString] -> [Listof PathString]}
(define (remove-dots l)
  (filter (λ (x) (not (regexp-match #px"\\.|compiled" x))) l))

#; {[Path] -> [Setof PathString]}
;; EFFECT check git status, abort if not committed; then pull
(define (git-status-check [which-one "./"])
  (parameterize ((current-custodian (make-custodian))
                 (current-directory which-one))
    (match-define (list in out pid err control) (process "git status"))
    (define status (port->list read-line in))
    
    (let loop ((status status))
      (unless (empty? status)
        (define l (first status))
        (cond
          [(regexp-match #px"Untracked" l)
           (list->set

           (let inner ([status (cdddr status)])
            (define next (string-trim (first status)))
            (cond
              [(equal? "" next) '()]
              [else (cons next (inner (rest status)))])))]
          [else (loop (rest status))])))))

(define header
  #<< here

| directory   | purpose									      |
| ----------- | ----------------------------------------------------------------------------- |

 here
  )

;; ---------------------------------------------------------------------------------------------------
(module+ main
  (main))