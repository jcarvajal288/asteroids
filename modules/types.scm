(define-module (types)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (game-width
            game-height
            ship-width
            ship-height
            dt
            make-ship
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
(define ship-width    43.0)
(define ship-height   41.0)
(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define-record-type <ship>
  (make-ship velocity heading thrust-accel rotation-speed hitbox)
  ship?
  (velocity ship-velocity)
  (heading ship-heading ship-heading-set!)
  (thrust-accel ship-thrust-accel)
  (rotation-speed ship-rotation-speed)
  (hitbox ship-hitbox))

(define-record-type <level>
  (make-level ship)
  level?
  (ship level-ship))
