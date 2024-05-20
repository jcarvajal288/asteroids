(define-module (types)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (make-ship
            ship-velocity
            ship-hitbox
            make-level
            level-ship 
            thrust-accel))

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

