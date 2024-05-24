(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (types)
  #:use-module (input)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (dom window)
  #:export (update-all))

(define PI 3.14159)

(define (get-thrust-vector heading acceleration)
  (let ((heading-in-radians (* heading (/ PI 180))))
    (vec2 (* acceleration (sin heading-in-radians))
          (- (* acceleration (cos heading-in-radians)))))) ; negative because positive y is towards the bottom

(define (accelerate-left ship) 
  (let ((ship-vel (ship-velocity ship))
        (thrust-accel (ship-thrust-accel ship)))
  (set-vec2-x! ship-vel (- (vec2-x ship-vel) thrust-accel))))

(define (accelerate-right ship) 
  (let ((ship-vel (ship-velocity ship))
        (thrust-accel (ship-thrust-accel ship)))
  (set-vec2-x! ship-vel (+ (vec2-x ship-vel) thrust-accel))))

(define (accelerate-forward ship) 
  (let* ((ship-vel (ship-velocity ship))
         (thrust-accel (ship-thrust-accel ship))
         (thrust-vector (get-thrust-vector (ship-heading ship) thrust-accel)))
    (vec2-add! ship-vel thrust-vector)))

(define (accelerate-backward ship) 
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


(define (apply-commands ship)
  (if command:accelerate-left (accelerate-left ship))
  (if command:accelerate-right (accelerate-right ship))
  (if command:accelerate-forward (accelerate-forward ship))
  (if command:accelerate-backward (accelerate-backward ship))
  (if command:rotate-left (rotate-left ship))
  (if command:rotate-right (rotate-right ship)))

(define (move-ship ship)
  (let ((hitbox (ship-hitbox ship))
        (velocity (ship-velocity ship)))
    (set-rect-x! hitbox (+ (rect-x hitbox) (vec2-x velocity)))
    (set-rect-y! hitbox (+ (rect-y hitbox) (vec2-y velocity)))))

(define (edge-warp hitbox width height)
  (let* ((northernmost (rect-y hitbox))
         (southernmost (+ northernmost (rect-height hitbox)))
         (westernmost (rect-x hitbox))
         (easternmost (+ westernmost (rect-width hitbox))))
    (cond 
      ((< southernmost 0) 
        (cons (rect-x hitbox) height))
      ((< easternmost 0)
        (cons width (rect-y hitbox)))
      ((> northernmost height)
        (cons (rect-x hitbox) (- (rect-height hitbox))))
      ((> westernmost width)
        (cons (- (rect-width hitbox)) (rect-y hitbox)))
      (else (cons (rect-x hitbox) (rect-y hitbox))))))

        
(define (update-all *level*)
  (let* ((ship (level-ship *level*)))
    (apply-commands ship)
    (move-ship ship)
    (let ((warp-point (edge-warp (ship-hitbox ship) (level-width *level*) (level-height *level*))))
      (set-rect-x! (ship-hitbox ship) (car warp-point))
      (set-rect-y! (ship-hitbox ship) (cdr warp-point)))))
