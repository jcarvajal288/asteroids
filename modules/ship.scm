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
            ship-hitbox
            make-default-ship))


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

