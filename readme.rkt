#lang racket

;; create the README.md files from README.source in the top-level and all code directories 

(define (main . x)
  (define show (empty? x))
  (define untracked (git-status-check))
  (define adirs (for/list ([fd (directory-list)] #:when (good? untracked fd)) fd))
  (define d-s (remove "scribblings" adirs))
  (displayln d-s)
  (readme d-s show)
  (define afils (map (λ (d) (build-path d "README.md")) adirs))
  (write-readme-and-show (make-header "directory") afils values show))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof PathString] Any -> Void}
(define (readme adirs show)
  (for ([dir adirs]) 
    (parameterize ([current-directory dir])
      (define afils (for*/list ([f (directory-list)] #:when (regexp-match #px"\\.rkt" f)) f))
      (write-readme-and-show (make-header "file") afils (λ (l) (substring l 3)) show))))

#; {String [Listof PathString] [String -> String] Any -> Void}
(define (write-readme-and-show header afils clean show)
  (define purps (purpose-statements afils clean))
  (copy-file "README.source" "README.md" 'delete-existing-one)
  (with-output-to-file "README.md"
    #:exists 'append
    (λ () (printf (make-table header afils purps))))
  (when show (system "open README.md")))

#; {String [Listof PathString] [Listof String] -> String}
(define (make-table header adirs purps)
  (define content
    (for/list ([d adirs] [p purps])
      (~a "| [" d "](" d ")" " | " p " | \n")))
  (apply string-append header content))

#; {[Listof PathString] -> [Listof String]}
(define (purpose-statements l clean)
  (for/list ([d l])
    (with-input-from-file d
      (λ ()
        (clean (string-trim (caddr (port->lines))))))))

#; {[Path] -> [Setof PathString]}
;; a primitive way to exclude Untracked directories and files 
(define (git-status-check [which-one "./"])
  (parameterize ((current-directory which-one))
    (match-define (list in out pid err control) (process "git status"))
    (define status (port->list read-line in))
    (let loop ((status status))
      (unless (empty? status)
        (define l (first status))
        (cond
          [(regexp-match #px"Untracked" l)
           (let inner ([status (cdddr status)])
             (define next (string-trim (first status)))
             (cond
               [(equal? "" next) '()]
               [else (cons next (inner (rest status)))]))]
          [else (loop (rest status))])))))

#; {[Listof PathString] PathString -> Boolean}
(define (good? untracked fd)
  (and (directory-exists? fd)
       (not (regexp-match #px"\\.|compiled" fd))
       (not (member fd untracked))))

(define (make-header x)
  (string-append
   "\n"
   (string-append "| " x " | purpose |\n")
   "|--------------------- | ------- |\n"))

;; ---------------------------------------------------------------------------------------------------
(module+ main (main 'dontshow))