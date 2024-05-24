(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot ffi)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (math rect)
  #:use-module (types)
  #:use-module (ship)
  #:export (load-all-images draw-all-objects))

(define image:ship #f)
(define image:asteroid-1 #f)

(define (load-all-images)
  (set! image:ship (make-image "assets/images/ship-1.png"))
  (set! image:asteroid-1 (make-image "assets/images/asteroid1.png")))

(define (draw-background context width height)
  (set-fill-color! context "#140c1c")
  (fill-rect context 0.0 0.0 width height))

(define (draw-ship context ship)
  (let* ((ship-rect (ship-hitbox ship))
         (width (ship-width ship))
         (height (ship-height ship))
         (ship-x (+ (rect-x ship-rect) (/ width 2)))
         (ship-y (+ (rect-y ship-rect) (/ height 2))))
    (save context)
    (translate! context ship-x ship-y) 
    (rotate! context (to-radians (ship-heading ship)))
    (translate! context (- ship-x) (- ship-y))
    (draw-image context image:ship
                0.0 0.0 width height
                (rect-x ship-rect) (rect-y ship-rect) width height)
    (restore! context)))


(define (draw-asteroids context)
  (draw-image context image:asteroid-1
              0 0 130 130
              30 30 130 130))

(define (draw-all-objects context level prev-time)
  (reset-transform! context)
  (draw-background context (level-width level) (level-height level))
  (let ((ship (level-ship level)))
    (draw-ship context ship))
  (draw-asteroids context))

