(import (scheme base)
        (scheme inexact)
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
        (math vector))

(define image:ship (make-image "assets/images/ship-1.png"))

(define game-width    1280.0)
(define game-height   1024.0)
(define ship-width    43.0)
(define ship-height   41.0)
(define ship-hitbox (make-rect (- (/ game-width 2.0) (/ ship-width 2)) 
                               (- (/ game-height 2.0) (/ ship-height 2))
                               ship-width 
                               ship-height))

(define (draw-background)
    (set-fill-color! context "#140c1c")
    (fill-rect context 0.0 0.0 game-width game-height))


(define (draw prev-time)
  (draw-background)
  (draw-image context image:ship
              0.0 0.0 ship-width ship-height
              (rect-x ship-hitbox) (rect-y ship-hitbox) ship-width ship-height)
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))
(request-animation-frame draw-callback)
