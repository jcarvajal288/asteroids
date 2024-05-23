(define-module (types)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (math vector)
  #:use-module (math rect)
  #:export (game-width
            game-height
            ship-width
            ship-height
            dt
            make-default-ship
            ship-velocity
            ship-heading-set!
            ship-heading
            ship-thrust-accel
            ship-rotation-speed
            ship-hitbox
            make-level
            level-ship))

(define game-width    1280.0)
(define game-height   1024.0)
(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define-record-type <ship>
  (make-ship width height velocity heading thrust-accel rotation-speed hitbox)
  ship?
  (width ship-width)
  (height ship-height)
  (velocity ship-velocity)
  (heading ship-heading ship-heading-set!)
  (thrust-accel ship-thrust-accel)
  (rotation-speed ship-rotation-speed)
  (hitbox ship-hitbox))

(define (make-default-ship)
  (let* ((width 43.0)
         (height 41.0)
         (velocity (vec2 0.0 0.0))
         (heading 0.0)
         (thrust-accel 1.0)
         (rotation-speed 2.0)
         (hitbox (make-rect (- (/ game-width 2.0) (/ width 2)) 
                            (- (/ game-height 2.0) (/ height 2))
                            width 
                            height)))
    (make-ship width height velocity heading thrust-accel rotation-speed hitbox)))

(define-record-type <level>
  (make-level ship)
  level?
  (ship level-ship))
