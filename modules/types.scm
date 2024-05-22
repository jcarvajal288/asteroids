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
            ship-hitbox
            make-level
            level-ship 
            thrust-accel))

(define game-width    1280.0)
(define game-height   1024.0)
(define ship-width    43.0)
(define ship-height   41.0)
(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define-record-type <ship>
  (make-ship velocity hitbox)
  ship?
  (velocity ship-velocity)
  (hitbox ship-hitbox))

(define-record-type <level>
  (make-level ship)
  level?
  (ship level-ship))

(define thrust-accel  1.0)

