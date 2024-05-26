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
            asteroid-heading
            asteroid-heading-set!
            asteroid-score-value
            asteroid-rotation-speed
            asteroid-hitbox))

(define-record-type <asteroid>
  (make-asteroid width height velocity heading rotation-speed score-value hitbox)
  asteroid?
  (width asteroid-width)
  (height asteroid-height)
  (velocity asteroid-velocity)
  (heading asteroid-heading asteroid-heading-set!)
  (rotation-speed asteroid-rotation-speed)
  (score-value asteroid-score-value)
  (hitbox asteroid-hitbox))

(define (build-asteroid level-width level-height)
  (let* ((width 71)
         (height 69)
         (velocity (vec2 (- (random-float 1 10) 5) (- (random-float 1 10) 5)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 2))
         (score-value 100)
         (hitbox (if (> (random) 0.5)
                   (make-rect (random-float 0 level-width)
                               level-height
                               width
                               height)
                   (make-rect level-width
                              (random-float 0 level-height)
                              width
                              height))))
  (make-asteroid width height velocity heading rotation-speed score-value hitbox)))
