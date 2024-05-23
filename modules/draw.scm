(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot ffi)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math rect)
  #:use-module (types)
  #:export (load-all-images draw-all-objects))

(define PI 3.14159)

(define ship-image #f)

(define (load-all-images)
  (set! ship-image (make-image "assets/images/ship-1.png")))

(define (draw-background context width height)
  (set-fill-color! context "#140c1c")
  (fill-rect context 0.0 0.0 width height))

(define (draw-ship context ship)
  (let* ((ship-rect (ship-hitbox ship))
         (width (ship-width ship))
         (height (ship-height ship))
         (ship-x (+ (rect-x ship-rect) (/ width 2)))
         (ship-y (+ (rect-y ship-rect) (/ height 2)))
         (ship-center (list ship-x ship-y)))
    (translate! context ship-x ship-y) 
    (rotate! context (/ (* (ship-heading ship) PI) 180.0))
    (translate! context (- ship-x) (- ship-y))
    (draw-image context ship-image
                0.0 0.0 width height
                (rect-x ship-rect) (rect-y ship-rect) width height)))

(define (draw-all-objects context level prev-time)
  (reset-transform! context)
  (draw-background context (level-width level) (level-height level))
  (let ((ship (level-ship level)))
    (draw-ship context ship)))

