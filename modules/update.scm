(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (stdlib)
  #:use-module (level)
  #:use-module (ship)
  #:use-module (asteroid)
  #:use-module (missile)
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


(define (fire-missile ship *level*)
  (let* ((ship-rect (ship-hitbox ship))
         (ship-center (cons (+ (rect-x ship-rect) (/ (ship-width ship) 2))
                            (+ (rect-y ship-rect) (/ (ship-height ship) 2))))
         (new-missile (build-missile (vec2 0.0 0.0) 
                                     (ship-heading ship) 
                                     ship-center)))
    (level-missiles-set! *level* (append (list new-missile) (level-missiles *level*)))
    (reset-fire-cooldown ship)))


(define (apply-commands ship *level*)
  (if command:accelerate-forward (accelerate-forward ship))
  (if command:accelerate-backward (accelerate-backward ship))
  (if command:rotate-left (rotate-left ship))
  (if command:rotate-right (rotate-right ship))
  (if command:fire-missile 
    (if (ready-to-fire? ship) (fire-missile ship *level*))))

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


(define (asteroid-hit? hitbox asteroid)
  (rect-intersects? hitbox (asteroid-hitbox asteroid)))

(define (handle-collisions *level*)
  (let* ((ship (level-ship *level*))
         (asteroids (level-asteroids *level*))
         (colliding-asteroids (filter (lambda (a) (asteroid-hit? (ship-hitbox ship) a)) asteroids)))
    (if (> (length colliding-asteroids) 0)
      (ship-alive-set! ship #f))))

        
(define (update-all *level*)
  (let* ((ship (level-ship *level*))
         (lev-width (level-width *level*))
         (lev-height (level-height *level*)))
    (apply-commands ship *level*)
    (move-ship ship lev-width lev-height)
    (progress-fire-cooldown ship)
    (move-asteroids (level-asteroids *level*) lev-width lev-height)
  (handle-collisions *level*)))

