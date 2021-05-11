#lang racket/gui

;; a framework for creating a controller:
;; 1. define
#; (.. sub-class-of base-control with mandatory-control-fields ..)
;; with a method for a specific use case. 
;; 2. apply 
#; (make-control this-sub-class)
;; The result is a complete control class. 

;                                                                  
;                                                                  
;                                                            ;;;   
;                     ;                                        ;   
;                     ;                                        ;   
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;   
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;   
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;   
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;   
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;   
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;   
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(require Fish/Common/board)

(define opt-posn/c (or/c #f posn/c))

(define controller/c (->m [is-a?/c frame%] opt-posn/c posn/c [channel/c opt-posn/c] any))

(define base/c
  (class/c
   [control
    ;; runs a thread that deals with the gui coordinates of where a snip got moved
    ;; -- if it is legal,
    ;;     it uses the given channel to inform the editor of adjustments to the localtion of the snip
    ;;     it asks for confirmation with a message dialog on the given frame
    ;; it then informs the model via a field-specified channel:
    ;; -- a posn means 'move the selected penguin to the specified position' 
    ;; -- #false means no conformation/illegal
    controller/c]))

(define mandatory-control-fields/c
  (class/c
   [field [TITLE string?]]
   (field [2model any/c])
   (field [ok-step? any/c])))

(define mixin/c
  (class/c
   (init-field [result-channel [channel/c [or/c #false any/c]]])
   (init-field
    [gui->model
     ;; converts GUI coordinates to Model coordinates plus Picture coordinrats (for adjustments)
     ;; compute the logical coordinates and the centered coordinates of the target (x,y)
     ;; IF the targeted logical position isn't already occupied
     ;; TODO: should this second part should ne left to `ok-step?`
     (-> posn/c (values opt-posn/c opt-posn/c))])))

(define control/c (and/c base/c mixin/c))

(provide
 (contract-out
  [control/c     contract?]
  [controller/c  contract?]
  [base-control% base/c]
  [make-control% (-> (and/c base/c mandatory-control-fields/c) control/c)]))

;                                                          
;                                                          
;                                                    ;;;   
;                             ;                        ;   
;                             ;                        ;   
;     ;;;    ;;;;   ; ;;;   ;;;;;;   ;;;;    ;;;;      ;   
;    ;   ;  ;;  ;;  ;;   ;    ;      ;;  ;  ;;  ;;     ;   
;   ;       ;    ;  ;    ;    ;      ;      ;    ;     ;   
;   ;       ;    ;  ;    ;    ;      ;      ;    ;     ;   
;   ;       ;    ;  ;    ;    ;      ;      ;    ;     ;   
;    ;   ;  ;;  ;;  ;    ;    ;      ;      ;;  ;;     ;   
;     ;;;    ;;;;   ;    ;     ;;;   ;       ;;;;       ;;;
;                                                          
;                                                          
;                                                          
;                                                          

(define ILLEGAL "the proposed place is illegal")

(define base-control%
  (class object%
    (super-new)
    (define/public (control . x) (error 'control "must be implemented"))))

(define (make-control% c%)
  (class c%
    (init-field result-channel gui->model)
    (inherit-field TITLE 2model ok-step?)
    (super-new)
    
    (define/override (control frame current-row-column gui-posn view-channel)
      (thread
       (λ ()
         (define-values (candidate-row-column candidate-center) (gui->model gui-posn))
         (cond
           [(and candidate-row-column (ok-step? current-row-column candidate-row-column))
            (channel-put view-channel candidate-center)
            (define answer (message-box TITLE (question "place") frame '(yes-no)))
            (define result (and (eq? answer 'yes) (2model current-row-column candidate-row-column)))
            (channel-put result-channel result)]
           [else
            (channel-put view-channel #false)
            (message-box TITLE ILLEGAL frame '(ok))])
         (kill-thread (current-thread)))))
    
    #; {String -> String}
    (define/private (question x)
      (~a "do you wish to " x " the avatar here"))))
