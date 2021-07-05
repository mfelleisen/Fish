#lang racket/gui

;; a canvas 

(provide
 #; (class (init-field pict0 : pict?) (eet (-> pict void?)))
 ;; show a Pict in the canvas 
 gcanvas%)

;; -----------------------------------------------------------------------------
(require Fish/Lib/toint)
(require pict)

;; -----------------------------------------------------------------------------
(define gcanvas%
  (class canvas% (init-field pict0)
    (inherit on-paint refresh-now)

    (define/public (set new-picture)
      (set! pict0 new-picture)
      (paint))

    (define/override (on-char e)
      (paint))
      
    (define/private (paint)
      (refresh-now (λ (dc) (draw-pict pict0 dc 0 0)))
      (set! pict0 pict0))

    (super-new
     [paint-callback (λ (_e dc) (paint))]
     [min-width (toint (pict-width pict0))]
     [min-height (toint (pict-height pict0))]
     [stretchable-width #false]	 
     [stretchable-height #false])

    (paint)
    (set pict0)))
