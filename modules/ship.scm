(define-module (ship)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (math vector)
  #:use-module (math rect)
  #:export (ship-width
            ship-height
            ship-velocity
            ship-heading-set!
            ship-heading
            ship-thrust-accel
            ship-rotation-speed
            ship-top-speed
            ship-alive?
            ship-alive-set!
            ship-hitbox
            make-default-ship
            ready-to-fire?
            progress-fire-cooldown
            reset-fire-cooldown))


(define-record-type <ship>
  (make-ship width height velocity heading thrust-accel rotation-speed top-speed alive?
             fire-cooldown hitbox)
  ship?
  (width ship-width)
  (height ship-height)
  (velocity ship-velocity)
  (heading ship-heading ship-heading-set!)
  (thrust-accel ship-thrust-accel)
  (rotation-speed ship-rotation-speed)
  (top-speed ship-top-speed)
  (alive? ship-alive? ship-alive-set!)
  (fire-cooldown ship-fire-cooldown ship-fire-cooldown-set!)
  (hitbox ship-hitbox))

(define (make-default-ship level-width level-height)
  (let* ((width 43.0)
         (height 41.0)
         (velocity (vec2 0.0 0.0))
         (heading 0.0)
         (thrust-accel 0.2)
         (rotation-speed 2.0)
         (top-speed 6.0)
         (alive? #t)
         (fire-cooldown 0)
         (hitbox (make-rect (- (/ level-width 2) (/ width 2)) 
                            (- (/ level-height 2) (/ height 2))
                            width 
                            height)))
    (make-ship width height velocity heading thrust-accel rotation-speed top-speed alive?
               fire-cooldown hitbox)))

(define (ready-to-fire? ship)
  (<= (ship-fire-cooldown ship) 0))

(define (progress-fire-cooldown ship)
  (let ((new-cooldown (- (ship-fire-cooldown ship) 1)))
    (if (>= new-cooldown 0) (ship-fire-cooldown-set! ship new-cooldown))))

(define (reset-fire-cooldown ship)
  (ship-fire-cooldown-set! ship 60))
