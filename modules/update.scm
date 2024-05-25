(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (types)
  #:use-module (ship)
  #:use-module (asteroid)
  #:use-module (input)
  #:use-module (math)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (dom window)
  #:export (update-all))

(define (get-thrust-vector heading acceleration)
  (let ((heading-in-radians (to-radians heading)))
    (vec2 (* acceleration (sin heading-in-radians))
          (- (* acceleration (cos heading-in-radians)))))) ; negative because positive y is towards the bottom


(define (accelerate-forward ship) 
  (let* ((ship-vel (ship-velocity ship))
         (thrust-accel (ship-thrust-accel ship))
         (thrust-vector (get-thrust-vector (ship-heading ship) thrust-accel)))
    (vec2-add! ship-vel thrust-vector)
    (vec2-limit! ship-vel (ship-top-speed ship))))

(define (accelerate-backward ship) 
  (let* ((ship-vel (ship-velocity ship))
         (thrust-accel (/ (ship-thrust-accel ship) 2))
         (thrust-vector (get-thrust-vector (ship-heading ship) thrust-accel)))
    (vec2-sub! ship-vel thrust-vector)
    (vec2-limit! ship-vel (ship-top-speed ship))))


(define (rotate-left ship)
  (let* ((new-heading (- (ship-heading ship) (ship-rotation-speed ship)))
         (clamped-heading (modulo new-heading 360.0)))
    (ship-heading-set! ship clamped-heading)))

(define (rotate-right ship)
  (let* ((new-heading (+ (ship-heading ship) (ship-rotation-speed ship)))
         (clamped-heading (modulo new-heading 360.0)))
    (ship-heading-set! ship clamped-heading)))


(define (apply-commands ship)
  (if command:accelerate-forward (accelerate-forward ship))
  (if command:accelerate-backward (accelerate-backward ship))
  (if command:rotate-left (rotate-left ship))
  (if command:rotate-right (rotate-right ship)))

(define (move hitbox velocity)
  (set-rect-x! hitbox (+ (rect-x hitbox) (vec2-x velocity)))
  (set-rect-y! hitbox (+ (rect-y hitbox) (vec2-y velocity))))

(define (move-ship ship lev-width lev-height)
  (let ((hitbox (ship-hitbox ship))
        (velocity (ship-velocity ship)))
    (move hitbox velocity)
    (edge-warp hitbox lev-width lev-height)))

(define (move-asteroids asteroids lev-width lev-height)
  (define (move-func asteroid)
    (let ((hitbox (asteroid-hitbox asteroid))
          (velocity (asteroid-velocity asteroid)))
      (move hitbox velocity)
      (asteroid-heading-set! asteroid (+ (asteroid-heading asteroid) (asteroid-rotation-speed asteroid)))
      (edge-warp hitbox lev-width lev-height)))
  (for-each move-func asteroids))

(define (edge-warp hitbox width height)
  (let* ((northernmost (rect-y hitbox))
         (southernmost (+ northernmost (rect-height hitbox)))
         (westernmost (rect-x hitbox))
         (easternmost (+ westernmost (rect-width hitbox)))
         (warp-point (cond 
                       ((< southernmost 0) 
                         (cons (rect-x hitbox) height))
                       ((< easternmost 0)
                         (cons width (rect-y hitbox)))
                       ((> northernmost height)
                         (cons (rect-x hitbox) (- (rect-height hitbox))))
                       ((> westernmost width)
                         (cons (- (rect-width hitbox)) (rect-y hitbox)))
                       (else (cons (rect-x hitbox) (rect-y hitbox))))))
      (set-rect-x! hitbox (car warp-point))
      (set-rect-y! hitbox (cdr warp-point))))

        
(define (update-all *level*)
  (let* ((ship (level-ship *level*))
         (lev-width (level-width *level*))
         (lev-height (level-height *level*)))
    (apply-commands ship)
    (move-ship ship lev-width lev-height)
    (move-asteroids (level-asteroids *level*) lev-width lev-height)))

