(define-module (types)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (math vector)
  #:use-module (math rect)
  #:export (ship-width
            ship-height
            dt
            make-default-level
            ship-velocity
            ship-heading-set!
            ship-heading
            ship-thrust-accel
            ship-rotation-speed
            ship-top-speed
            ship-hitbox
            make-level
            level-width
            level-height
            level-ship))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define-record-type <ship>
  (make-ship width height velocity heading thrust-accel rotation-speed top-speed hitbox)
  ship?
  (width ship-width)
  (height ship-height)
  (velocity ship-velocity)
  (heading ship-heading ship-heading-set!)
  (thrust-accel ship-thrust-accel)
  (rotation-speed ship-rotation-speed)
  (top-speed ship-top-speed)
  (hitbox ship-hitbox))

(define (make-default-ship level-width level-height)
  (let* ((width 43.0)
         (height 41.0)
         (velocity (vec2 0.0 0.0))
         (heading 0.0)
         (thrust-accel 0.2)
         (rotation-speed 2.0)
         (top-speed 6.0)
         (hitbox (make-rect (- (/ level-width 2) (/ width 2)) 
                            (- (/ level-height 2) (/ height 2))
                            width 
                            height)))
    (make-ship width height velocity heading thrust-accel rotation-speed top-speed hitbox)))

(define-record-type <level>
  (make-level width height ship)
  level?
  (width level-width)
  (height level-height)
  (ship level-ship))

(define (make-default-level)
  (let* ((width 1280.0)
         (height 1024.0)
         (ship (make-default-ship width height)))
    (make-level width height ship)))


