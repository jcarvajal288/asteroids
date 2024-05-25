(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot ffi)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (math rect)
  #:use-module (level)
  #:use-module (ship)
  #:use-module (asteroid)
  #:export (load-all-images draw-all-objects))

(define image:ship #f)
(define image:asteroid-1 #f)

(define (load-all-images)
  (set! image:ship (make-image "assets/images/ship-1.png"))
  (set! image:asteroid-1 (make-image "assets/images/asteroid2.png")))

(define (draw-background context width height)
  (set-fill-color! context "#140c1c")
  (fill-rect context 0.0 0.0 width height))

(define (draw-with-rotation context image heading sx sy sw sh dx dy dw dh)
  (let ((center-x (+ dx (/ dw 2)))
        (center-y (+ dy (/ dh 2))))
    (save context)
    (translate! context center-x center-y) 
    (rotate! context (to-radians heading))
    (translate! context (- center-x) (- center-y))
    (draw-image context image
                sx sy sw sh
                dx dy dw dh)
    (restore! context)))

(define (draw-ship context ship)
  (let* ((ship-rect (ship-hitbox ship))
         (width (ship-width ship))
         (height (ship-height ship)))
    (draw-with-rotation context image:ship (ship-heading ship)
                        0 0 width height
                        (rect-x ship-rect) (rect-y ship-rect) width height)))

(define (draw-asteroids context asteroids)
  (define (draw-asteroid asteroid)
    (let* ((ast-rect (asteroid-hitbox asteroid))
           (width (asteroid-width asteroid))
           (height (asteroid-height asteroid)))
      (draw-with-rotation context image:asteroid-1 (asteroid-heading asteroid)
                  0 0 width height
                  (rect-x ast-rect) (rect-y ast-rect) width height)))
  (for-each draw-asteroid asteroids))


(define (draw-all-objects context level prev-time)
  (draw-background context (level-width level) (level-height level))
  (draw-ship context (level-ship level))
  (draw-asteroids context (level-asteroids level)))

