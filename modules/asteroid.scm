(define-module (asteroid)
  #:pure
  #:use-module (scheme base)
  #:use-module (math)
  #:use-module (math vector)
  #:use-module (math rect)
  #:export (build-small-asteroid
            build-small-asteroid-at
            build-medium-asteroid
            build-medium-asteroid-at
            build-large-asteroid
            build-random-asteroid
            build-debris-at
            build-ore-at
            asteroid-width
            asteroid-height
            asteroid-type
            asteroid-velocity
            asteroid-heading
            asteroid-heading-set!
            asteroid-score-value
            asteroid-rotation-speed
            asteroid-hitbox))

(define ore-value 500)
(define small-asteroid-value 100)
(define medium-asteroid-value 50)
(define large-asteroid-value 25)

(define-record-type <asteroid>
  (make-asteroid width height type velocity heading rotation-speed score-value hitbox)
  asteroid?
  (width asteroid-width)
  (height asteroid-height)
  (type asteroid-type)
  (velocity asteroid-velocity)
  (heading asteroid-heading asteroid-heading-set!)
  (rotation-speed asteroid-rotation-speed)
  (score-value asteroid-score-value)
  (hitbox asteroid-hitbox))

(define (build-small-asteroid level-width level-height)
  (let* ((width 68)
         (height 58)
         (type 'small)
         (velocity (vec2-of-length (random-float 0 360) (random-float 4 8)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 4))
         (hitbox (if (> (random) 0.5)
                   (make-rect (random-float 0 level-width)
                               level-height
                               width
                               height)
                   (make-rect level-width
                              (random-float 0 level-height)
                              width
                              height))))
  (make-asteroid width height type velocity heading rotation-speed small-asteroid-value hitbox)))

(define (build-medium-asteroid level-width level-height)
  (let* ((width 71)
         (height 69)
         (type 'medium)
         (velocity (vec2-of-length (random-float 0 360) (random-float 1 5)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 2))
         (hitbox (if (> (random) 0.5)
                   (make-rect (random-float 0 level-width)
                               level-height
                               width
                               height)
                   (make-rect level-width
                              (random-float 0 level-height)
                              width
                              height))))
  (make-asteroid width height type velocity heading rotation-speed medium-asteroid-value hitbox)))

(define (build-large-asteroid level-width level-height)
  (let* ((width 91)
         (height 86)
         (type 'large)
         (velocity (vec2 (- (random-float 0.5 5) 2.5) (- (random-float 0.5 5) 2.5)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 2))
         (hitbox (if (> (random) 0.5)
                   (make-rect (random-float 0 level-width)
                               level-height
                               width
                               height)
                   (make-rect level-width
                              (random-float 0 level-height)
                              width
                              height))))
  (make-asteroid width height type velocity heading rotation-speed large-asteroid-value hitbox)))

(define (build-random-asteroid level-width level-height)
  (let ((selection (random-float 0 3)))
    (cond ((< selection 1.2)
            (build-small-asteroid level-width level-height))
          ((< selection 2.0)
            (build-medium-asteroid level-width level-height))
          (else  
            (build-large-asteroid level-width level-height)))))

(define (build-medium-asteroid-at x y)
  (let* ((width 71)
         (height 69)
         (type 'medium)
         (velocity (vec2-of-length (random-float 0 360) (random-float 1 5)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 2))
         (hitbox (make-rect x y width height)))
  (make-asteroid width height type velocity heading rotation-speed medium-asteroid-value hitbox)))

(define (build-small-asteroid-at x y)
  (let* ((width 48)
         (height 46)
         (type 'small)
         (velocity (vec2-of-length (random-float 0 360) (random-float 4 8)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 0 4))
         (hitbox (make-rect x y width height)))
  (make-asteroid width height type velocity heading rotation-speed small-asteroid-value hitbox)))

(define (build-debris-at x y)
  (let* ((width 10)
         (height 9)
         (type 'debris)
         (velocity (vec2-of-length (random-float 0 360) (random-float 4 12)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 2 6))
         (score-value 0)
         (hitbox (make-rect x y width height)))
  (make-asteroid width height type velocity heading rotation-speed score-value hitbox)))

(define (build-ore-at x y)
  (let* ((width 15)
         (height 14)
         (type 'ore)
         (velocity (vec2-of-length (random-float 0 360) (random-float 4 12)))
         (heading (random-float 0 360))
         (rotation-speed (random-float 2 5))
         (hitbox (make-rect x y width height)))
  (make-asteroid width height type velocity heading rotation-speed ore-value hitbox)))
