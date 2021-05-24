#lang at-exp racket

;; specify remote JSON messages in an abbreviated form: see example below 

(provide
 #; {expr == ... (messages 1msg ...)}
 #; (1msg == [ [method-name:id argument:arg ...] result:arg note:string ]
          || [ [method-name:id argument:arg ...] result:arg])
 ;; (messages ...) generates a table where
 ;; each message spec becomes a row in a table:
 #;   [ method-name [argument.fmt, ...] result note ]
 ;; where argument.fmt turns id-s into strings and joins arrays via comma; note default: 'cont 
 ;; the optional note is typically a foonote
 ;; generate a table of message call formats 
 messages)

;; ---------------------------------------------------------------------------------------------------
(require "shared.rkt")
(require scribble/manual)
(require (for-syntax syntax/parse))

;; ---------------------------------------------------------------------------------------------------
(begin-for-syntax
  (define-syntax-class abbrev
    [pattern x:id  #:attr fmt #'@tt{@(~a 'x)}]
    [pattern x:str #:attr fmt #'@tt{@(~s 'x)}]
    [pattern ((y ...) ...)
     #:with (z ...) #'`(,(y ...) ...)
     #:with fmt #'@tt{[@(element-join (z ...) ", ")]}]
    [pattern y #:attr fmt #'@tt{@y}]))

(define-syntax (messages stx)
  (syntax-parse stx
    [(_ one ...)
     #:with def-arg #'(elem (deftech "Argument")", ...")
     #'(tabular
        #:row-properties '(bottom-border top) #:sep @hspace[4]
        (cons
         @list[ @t{Name} def-arg @deftech{Result}  @t{Note}]
         @; --------------------------------------------------------------
         (list (1msg one) ...)))]))

(define-syntax (1msg stx)
  (syntax-parse stx
    [(_ [[x:id argument:abbrev ...] result:abbrev (~optional note #:defaults ([note #''cont]))])
     #:with arg #'(element-join (list @argument.fmt ...) ", ")
     #'(list (tt (~a 'x)) arg @tt{@result.fmt} @note )]))
     
;; ---------------------------------------------------------------------------------------------------
(module+ test
  
 @messages[
 [ [ start         Boolean                                  ] "void"                 ]
 [ [ playing-as    @tech{Color}                             ] "void"                 ]
 [ [ playing-with  [@tech{Color} (~a " ... ")  @tech{Color}]] "void"                 ]
 [ [ setup         @tech{State}                             ] @tech{Position}        ]
 [ [ take-turn     @tech{State} @tech{Actions}              ] @tech{Action}      "*" ]
 [ [ end           Boolean                                  ] "void"                 ]
 ]
  )
