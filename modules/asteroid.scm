(define-module (asteroid)
  #:pure
  #:use-module (scheme base)
  #:use-module (math)
  #:use-module (math vector)
  #:use-module (math rect)
  #:export (build-small-asteroid
            build-medium-asteroid
            build-large-asteroid
            asteroid-width
            asteroid-height
            asteroid-size
            asteroid-velocity
            asteroid-heading
            asteroid-heading-set!
            asteroid-score-value
            asteroid-rotation-speed
            asteroid-hitbox))

(define-record-type <asteroid>
  (make-asteroid width height size velocity heading rotation-speed score-value hitbox)
  asteroid?
  (width asteroid-width)
  (height asteroid-height)
  (size asteroid-size)
  (velocity asteroid-velocity)
  (heading asteroid-heading asteroid-heading-set!)
  (rotation-speed asteroid-rotation-speed)
  (score-value asteroid-score-value)
  (hitbox asteroid-hitbox))

(define (build-small-asteroid level-width level-height)
  (let* ((width 68)
         (height 58)
         (size 'small)
         (velocity (vec2 (- (random-float 4 12) 6) (- (random-float 4 12) 6)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 4))
         (score-value 250)
         (hitbox (if (> (random) 0.5)
                   (make-rect (random-float 0 level-width)
                               level-height
                               width
                               height)
                   (make-rect level-width
                              (random-float 0 level-height)
                              width
                              height))))
  (make-asteroid width height size velocity heading rotation-speed score-value hitbox)))

(define (build-medium-asteroid level-width level-height)
  (let* ((width 71)
         (height 69)
         (size 'medium)
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
  (make-asteroid width height size velocity heading rotation-speed score-value hitbox)))

(define (build-large-asteroid level-width level-height)
  (let* ((width 91)
         (height 86)
         (size 'large)
         (velocity (vec2 (- (random-float 0.5 5) 2.5) (- (random-float 0.5 5) 2.5)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 2))
         (score-value 50)
         (hitbox (if (> (random) 0.5)
                   (make-rect (random-float 0 level-width)
                               level-height
                               width
                               height)
                   (make-rect level-width
                              (random-float 0 level-height)
                              width
                              height))))
  (make-asteroid width height size velocity heading rotation-speed score-value hitbox)))
