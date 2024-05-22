(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:use-module (scheme write)
  #:use-module (types)
  #:use-module (input)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (dom window)
  #:export (update-all))

(define thrust-accel  1.0)

(define (move-left ship-vel) 
  (set-vec2-x! ship-vel (- (vec2-x ship-vel) thrust-accel)))
(define (move-right ship-vel) (set-vec2-x! ship-vel (+ (vec2-x ship-vel) thrust-accel)))
(define (move-forward ship-vel) (set-vec2-y! ship-vel (- (vec2-y ship-vel) thrust-accel)))
(define (move-backward ship-vel) (set-vec2-y! ship-vel (+ (vec2-y ship-vel) thrust-accel)))
        
(define (update-all *level*)
  (let* ((ship (level-ship *level*))
         (ship-hitbox (ship-hitbox ship))
         (ship-velocity (ship-velocity ship)))
    (if command:move-left (move-left ship-velocity))
    (if command:move-right (move-right ship-velocity))
    (if command:move-forward (move-forward ship-velocity))
    (if command:move-backward (move-backward ship-velocity))
    ;(move-left ship-velocity)
    (set-rect-x! ship-hitbox (+ (rect-x ship-hitbox) (vec2-x ship-velocity)))
    (set-rect-y! ship-hitbox (+ (rect-y ship-hitbox) (vec2-y ship-velocity)))))
