#lang at-exp racket

(require scriblib/figure)
(provide (all-from-out scriblib/figure))

;; this file defines some common utilities needed for the Standard scribble
;; files as well as the scribble files in ../Assignment

;; -----------------------------------------------------------------------------
;; requiring and explorting the documentation links for BSL, ISL, teachpacks
(require (for-label racket))
(provide (for-label (all-from-out racket)))

(require (for-label syntax/parse))
(provide (for-label (all-from-out syntax/parse)))

(require (only-in (for-label typed/racket) define-type))
(provide (for-label (all-from-out typed/racket)))

;; -----------------------------------------------------------------------------
(provide protocol)

(define (protocol label title stuff)
  @figure[label title #:style left-figure-style]{@nested[#:style "smaller"]{
   @stuff}})


;; -----------------------------------------------------------------------------
(require scribble-abbrevs/manual)

(provide show-file)

(define-syntax-rule
  (show-file label file-name title ...)
  (figure label (elem (~a 'title ...))
    (filebox (elem (strip-dots file-name)) (racketfile file-name))))

(define (strip-dots s)
  (substring s 3))

;; -----------------------------------------------------------------------------
(provide task new-tasks)

(define *task 0)
(define (new-tasks)
  (set! *task 0))
(define (task)
  (set! *task (+ *task 1))
  (blue "Task " (number->string *task)))

;; -----------------------------------------------------------------------------

(provide json stdin stdout stderr)

(define (json) @link["https://www.json.org"]{JSON})
(define (stdin) @tt{STDIN})
(define (stdout) @tt{STDOUT})
(define (stderr) @tt{STDERR})

;; -----------------------------------------------------------------------------

(provide
  ; [List Text Text] ... -> Nested
  faq)

(define (faq . questions-and-answers)
   (define blank @list[ @t{ } @t{ } ])
   (define zipped
     (for/fold ((zipped '())) ((qa (reverse questions-and-answers)))
       (cons qa (cons blank zipped))))
   @nested[#:style 'inset]{
    @tabular[ #:sep @hspace[5]
	     #:row-properties '(bottom-border top)
	     @cons[ @list[ @t{question} @t{answer}] @zipped ] ]})
   


;; -----------------------------------------------------------------------------
(provide nl hs)

(define nl @element['newline]{ })
(define hs @element['hspace]{---})


;; -----------------------------------------------------------------------------
(provide when* how-many-assignments-to-show)

(define how-many-assignments-to-show
  (let* ([args (current-command-line-arguments)]
         [arg# (vector-length args)])
    (if (zero? arg#) 0 (string->number (vector-ref args 0)))))

(define-syntax-rule
  (when* i s ...)
  (when (<= i how-many-assignments-to-show)
    (list s ...)))

;; -----------------------------------------------------------------------------

(provide
  ;; a base evaluator with the test engine pre-loaded so that check-expect
  ;; is available
  ev ;; QuotedCode -> Void

  ;; (def code ...)
  ;; expands
  ;; to type-set racket code and evaluates the code with ev,
  ;; one piece at a time; at the end the evaluator runs (test)
  def ;; syntax 
  )

(require teachpack/2htdp/scribblings/img-eval)

(define ev (make-img-eval))

(ev '(require test-engine/racket-tests))
(ev '(define true #t))
(ev '(define false #t))
(ev '(require (only-in lang/htdp-intermediate local)))
;; can't do the following, why? 
; (ev '(require 2htdp/image))
; (ev '(require 2htdp/universe))

(define-syntax-rule
  (def code ...)
  ;; ==> 
  (begin
    (racketblock
code
      ...)
    (ev 'code)
    ...
    (ev '(test))
    ))

;; -----------------------------------------------------------------------------

;; to be documented 

(provide (all-from-out scribble/manual)
         (all-from-out scribble/core))

(provide top usec husec usub) ;; section headers 

(provide strike)

(provide nested-table the-end
  ;; syntax: (ps <number or string> string)
  ;; creates a begin that is expected to be sliced into the top level
  ;; scribble doc
  ps 
  ;; syntax: (lab <number or string> string)
  ;; creates a begin that is expected to be sliced into the top level
  ;; scribble doc
  lab

  (contract-out
    ;; create a problem label with running number, per problem set 
    [problem (-> element?)])
  ;; create a sample problem element, nested, indented 
  sample-problem 

  exercise

  grid
  

  separator)

(provide make-list first) ;; Racket functions 

(provide red link-color blue green yellow orange pink purple background-white)

;; ---------------------------------------------------------------------------------------------------
(require scribble/manual scribble/core scribble/html-properties 2htdp/image)

(require (only-in scribble/base title section))

;; ---------------------------------------------------------------------------------------------------
(define separator (line 700 0 "blue"))

(define the-end (toc-element #f "" (hyperlink "http://www.rust-lang.org/" "Rust")))

(define *p 0)
(define *e 0)
(define (*reset) (set! *p 0) (set! *e 0))
(define-syntax-rule
  (ps n pl)
  (begin
    (*reset)
    (define n:str (if (string? n) n (number->string n)))
    (margin-note (image "Images/homework.png" #:scale .22 "home work!"))
    (title #:tag (string-append "ps" n:str) #:style '(toc grouper unnumbered) "Problem Set " n:str)
    (bold "Programming Language")
    "  "
    pl))

(define-syntax-rule
  (lab n pl)
  (begin
    (*reset)
    (define n:str (if (string? n) n (number->string n)))
    (margin-note (image "Images/lab.png" #:scale .22 "home work!"))
    (title #:tag (string-append "lab" n:str) #:style '(grouper unnumbered) "Lab  " n:str " "pl)))
  
(define (problem)
  (set! *p (+ *p 1))
  (bold "Problem " (number->string *p)))

(define (grid s)
  (style #f
    (list
      (attributes `((cellpadding . "2") (cellspacing . ,(number->string s)) (border . "1"))))))

(define (sample-problem . t)
  (nested #:style 'inset (bold "Sample Problem ") t))

(define (exercise . t) 
  (set! *e (add1 *e))
  (nested #:style 'inset (bold (format "Exercise ~a " *e)) t))

;; ---------------------------------------------------------------------------------------------------

(define (nested-table t)
  (nested (tabular #:style 'boxed #:sep (hspace 3) t)))

;; ---------------------------------------------------------------------------------------------------
(define (husec #:tag (tag #f) . t)
  (if tag
      (section #:tag tag #:style '(unnumbered toc-hidden) t)
      (section #:style '(unnumbered toc-hidden) t)))

(define (usec . t) (section #:style '(unnumbered) #:tag (symbol->string (gensym)) t))
(define (usub . t) (subsection #:style '(unnumbered) #:tag (symbol->string (gensym)) t))

;; ---------------------------------------------------------------------------------------------------
(define (top #:tag-prefix [tp #f] #:tag [t #f] #:unnumbered? [unnumbered? #f] #:style [s '()]. c)
 (list
  (apply title
    #:tag-prefix tp
    #:style (append s '(grouper unnumbered)) #:tag t c)
  (section #:tag (and t (string-append "chap:" t))
           #:style (append
                    (if unnumbered? '(toc unnumbered) null)
                    '(hidden toc-hidden)))))

;; ---------------------------------------------------------------------------------------------------
(define (strike . t)
  (element (style "strike" (list (attributes '((style . "text-decoration:line-through")))))
           t))

(provide colored)
(define ((colored c) . t)
  @element[(style #f (list (color-property c)))]{@t})

(define-syntax-rule (define-colored c) (define c (colored (format "~a" 'c))))

(define link-color (colored "#07A"))

(define-colored red)
(define-colored green)
(define-colored yellow)
(define-colored orange)
(define-colored pink)
(define-colored purple)
(define-colored blue)

(define (background-white c . t)
  (define s (style #f `(,(background-color-property "white") ,(color-property c))))
  (element s t))

;; ---------------------------------------------------------------------------------------------------
(define (ltable c #:sep (sep #f))
   (if sep 
       (tabular #:sep (hspace 1) #:style (ltable-style c #t) c)
       (tabular #:style (ltable-style c) c)))

(define (ltable-style content (sep #f))
   (define col# (length (car content)))
   (define cell-left
     (make-list (length content)
                (make-list (if sep (sub1 (* 2 col#)) col#) (style 'left '()))))
   (style #f `(,(table-cells cell-left))))

;; ---------------------------------------------------------------------------------------------------

(provide
  ; PathString LabelString -> Element 
  resource
  
  resource/unlabeled)

(require (only-in scribble/html-properties link-resource install-resource))

(define (resource path . label)
  (elem #:style (style #f (list (link-resource path))) (string-join label " ")))

(define (resource/unlabeled path . label)
  (elem #:style (style #f (list (install-resource path))) (string-join label " ")))

;; ---------------------------------------------------------------------------------------------------

(provide
  ;; [Listof Element] { Alpha } -> [Listof (U Element Alpha)]
  element-join)

(define (element-join loe (separator " "))
  (if (empty? (rest loe))
      loe
      (cons (first loe)
            (let ej ([l (cdr loe)])
	      (cond
		[(null? l) '()]
		[else
		  (list* separator (car l) (ej (cdr l)))])))))
	    
;; ---------------------------------------------------------------------------------------------------

(provide developing)
(define developing "https://felleisen.org/matthias/Thoughts/Developing_Developers.html")

;; ---------------------------------------------------------------------------------------------------

(provide big-block)
;; Nat String ... -> Blockquote
(define (big-block h . stuff)
  ;  @nested[#:style 'inset]{
  @tabular[
 @append[
 (build-list h (lambda (i) (list @~)))
 @list[@list[(t stuff)]]
 (build-list h (lambda (i) (list @~)))]
 ]
  ;}
  )


;; -----------------------------------------------------------------------------
(provide at)

@(define (at) @tt|{@}|)

;; -----------------------------------------------------------------------------
(provide file-number)


(define (file-number stx)
 (define-values (base name dir?) (split-path (syntax-source stx)))
 (define n (regexp-match #px"(\\d+).scrbl" (path->string name)))
 (and n (second n)))
