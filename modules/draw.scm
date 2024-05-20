(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math rect)
  #:use-module (types)
  #:export (load-all-images draw-all-objects))

(define ship-image #f)

(define (load-all-images)
  (set! ship-image (make-image "assets/images/ship-1.png")))

(define (draw-background context)
  (set-fill-color! context "#140c1c")
  (fill-rect context 0.0 0.0 game-width game-height))

(define (draw-ship context ship)
    (draw-image context ship-image
                0.0 0.0 ship-width ship-height
                (rect-x (ship-hitbox ship)) (rect-y (ship-hitbox ship)) ship-width ship-height))

(define (draw-all-objects context level prev-time)
  (let ((ship (level-ship level)))
    (draw-background context)
    (draw-ship context ship)))

