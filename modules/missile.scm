(define-module (missile)
  #:pure
  #:use-module (scheme base)
  #:use-module (math rect)
  #:use-module (math vector)
  #:export (build-missile
            missile-width
            missile-height
            missile-velocity
            missile-acceleration
            missile-top-speed
            missile-heading
            missile-hitbox
            progress-arm-timer
            is-armed?))

(define-record-type <missile>
  (make-missile width height velocity acceleration top-speed time-to-arm heading hitbox)
  missile?
  (width missile-width)
  (height missile-height)
  (velocity missile-velocity)
  (acceleration missile-acceleration)
  (top-speed missile-top-speed)
  (time-to-arm missile-time-to-arm missile-time-to-arm-set!)
  (heading missile-heading)
  (hitbox missile-hitbox))

(define (build-missile ship-velocity heading center)
  (let* ((width 6)
         (height 20)
         (velocity (vec2-of-length heading 10)) 
         (acceleration 0.2)
         (top-speed 10.0)
         (time-to-arm 60)
         (hitbox (make-rect (+ (car center) (/ width 2))
                            (+ (cdr center) (/ height 2))
                            width
                            height)))
    (vec2-add! velocity ship-velocity)
    (make-missile width height velocity acceleration top-speed time-to-arm heading hitbox)))

(define (progress-arm-timer missile)
  (if (> (missile-time-to-arm missile) 0)
    (missile-time-to-arm-set! missile (- (missile-time-to-arm missile) 1))))

(define (is-armed? missile)
  (<= (missile-time-to-arm missile) 0))
