(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (dom image)
  #:use-module (types)
  #:export (draw-all-objects))

(define image:ship (make-image "assets/images/ship-1.png"))

(define (draw-background)
    (set-fill-color! context "#140c1c")
    (fill-rect context 0.0 0.0 game-width game-height))

(define (draw-ship)
  (let ((ship (level-ship *level*)))
    (draw-image context image:ship
                0.0 0.0 ship-width ship-height
                (rect-x (ship-hitbox ship)) (rect-y (ship-hitbox ship)) ship-width ship-height)))

(define (draw-all-objects prev-time)
  (draw-background)
  (draw-ship))

