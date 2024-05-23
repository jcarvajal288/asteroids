(import (scheme base)
        (scheme inexact)
        (scheme write)
        (hoot debug)
        (hoot ffi)
        (hoot hashtables)
        (hoot match)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window)
        (math)
        (math rect)
        (math vector)
        (draw)
        (input)
        (types)
        (update))

(define (make-level-1)
  (make-level (make-ship (vec2 0.0 0.0)
                         0.0
                         1.0
                         2.0
                         (make-rect (- (/ game-width 2.0) (/ ship-width 2)) 
                               (- (/ game-height 2.0) (/ ship-height 2))
                               ship-width 
                               ship-height))))

(define *level* (make-level-1))

(define (update-func)
  (update-all *level*)
  (timeout update-callback dt))
(define update-callback (procedure->external update-func))

(define (draw prev-time)
  (draw-all-objects context *level* prev-time)
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(load-all-images)
(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(request-animation-frame draw-callback)
(timeout update-callback dt)
