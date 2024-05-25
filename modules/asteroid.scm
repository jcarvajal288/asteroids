(define-module (asteroid)
  #:pure
  #:use-module (scheme base)
  #:use-module (math)
  #:use-module (math vector)
  #:use-module (math rect)
  #:export (build-asteroid
            asteroid-width
            asteroid-height
            asteroid-velocity
            asteroid-rotation-speed
            asteroid-hitbox))

(define-record-type <asteroid>
  (make-asteroid width height velocity rotation-speed hitbox)
  asteroid?
  (width asteroid-width)
  (height asteroid-height)
  (velocity asteroid-velocity)
  (rotation-speed asteroid-rotation-speed)
  (hitbox asteroid-hitbox))

(define (build-asteroid level-width level-height)
  (let* ((width 71)
         (height 69)
         (velocity (vec2 (- (random-float 1 10) 5) (- (random-float 1 10) 5)))
         (rotation-speed (random-float 0 2))
         (hitbox (make-rect (random-float 0 level-width)
                            level-height
                            width
                            height)))
  (make-asteroid width height velocity rotation-speed hitbox)))
