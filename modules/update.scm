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

(define (move-left ship) 
  (let ((ship-vel (ship-velocity ship))
        (thrust-accel (ship-thrust-accel ship)))
  (set-vec2-x! ship-vel (- (vec2-x ship-vel) thrust-accel))))

(define (move-right ship) 
  (let ((ship-vel (ship-velocity ship))
        (thrust-accel (ship-thrust-accel ship)))
  (set-vec2-x! ship-vel (+ (vec2-x ship-vel) thrust-accel))))

(define (move-forward ship) 
  (let ((ship-vel (ship-velocity ship))
        (thrust-accel (ship-thrust-accel ship)))
  (set-vec2-y! ship-vel (- (vec2-y ship-vel) thrust-accel))))

(define (move-backward ship) 
  (let ((ship-vel (ship-velocity ship))
        (thrust-accel (ship-thrust-accel ship)))
  (set-vec2-y! ship-vel (+ (vec2-y ship-vel) thrust-accel))))


(define (rotate-left ship)
  (let* ((new-heading (- (ship-heading ship) (ship-rotation-speed ship)))
         (clamped-heading (modulo new-heading 360.0)))
    (ship-heading-set! ship clamped-heading)))

(define (rotate-right ship)
  (let* ((new-heading (+ (ship-heading ship) (ship-rotation-speed ship)))
         (clamped-heading (modulo new-heading 360.0)))
    (ship-heading-set! ship clamped-heading)))
        
(define (update-all *level*)
  (let* ((ship (level-ship *level*))
         (ship-hitbox (ship-hitbox ship))
         (ship-heading (ship-heading ship))
         (ship-velocity (ship-velocity ship)))
    (if command:move-left (move-left ship))
    (if command:move-right (move-right ship))
    (if command:move-forward (move-forward ship))
    (if command:move-backward (move-backward ship))
    (if command:rotate-left (rotate-left ship))
    (if command:rotate-right (rotate-right ship))
    (set-rect-x! ship-hitbox (+ (rect-x ship-hitbox) (vec2-x ship-velocity)))
    (set-rect-y! ship-hitbox (+ (rect-y ship-hitbox) (vec2-y ship-velocity)))))
