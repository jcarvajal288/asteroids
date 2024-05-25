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
        (level)
        (update))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define *level* (make-default-level))

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
(set-element-width! canvas (exact (level-width *level*)))
(set-element-height! canvas (exact (level-height *level*)))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(request-animation-frame draw-callback)
(timeout update-callback dt)
