(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (hoot match)
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
    (vec2-of-length heading acceleration))

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
         (new-missile (build-missile (ship-velocity ship) 
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

(define (move-ore ores lev-width lev-height)
  (define (move-func ore)
    (let ((hitbox (asteroid-hitbox ore))
          (velocity (asteroid-velocity ore)))
      (move hitbox velocity)
      (asteroid-heading-set! ore (+ (asteroid-heading ore) (asteroid-rotation-speed ore)))
      (edge-warp hitbox lev-width lev-height)))
  (for-each move-func ores))

(define (move-missiles missiles lev-width lev-height)
  (define (move-func missile)
    (let ((hitbox (missile-hitbox missile))
          (velocity (missile-velocity missile)))
      (move hitbox velocity)
      (edge-warp hitbox lev-width lev-height)))
  (for-each move-func missiles)
  (for-each progress-arm-timer missiles))


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


(define (asteroid-collides? hitbox asteroid)
  (rect-intersects? hitbox (asteroid-hitbox asteroid)))

(define (missile-collides? hitbox missile)
  (rect-intersects? hitbox (missile-hitbox missile)))

(define (asteroid-hit-missile? asteroid missiles)
  (let* ((colliding-missiles (filter (lambda (m) 
                                       (asteroid-collides? (missile-hitbox m) asteroid))
                                     missiles)))
    (> (length colliding-missiles) 0)))

(define (missile-hit-asteroid? missile asteroids)
  (let* ((colliding-asteroids (filter (lambda (a)
                                        (missile-collides? (asteroid-hitbox a) missile))
                                      asteroids)))
    (> (length colliding-asteroids) 0)))

(define (handle-ship-collisions *level*)
  (let* ((ship (level-ship *level*))
         (asteroids (level-asteroids *level*))
         (ore (level-ore *level*))
         (colliding-asteroids (filter (lambda (a) (asteroid-collides? (ship-hitbox ship) a)) asteroids))
         (non-colliding-ore (filter (lambda (a) (not (asteroid-collides? (ship-hitbox ship) a))) ore)))
    (if (< (length non-colliding-ore) (length (level-ore *level*)))
      (score-ore *level* non-colliding-ore))
    (if (and (> (length colliding-asteroids) 0) (ship-alive? ship))
      (kill-player ship *level*))))

(define (handle-missile-collisions *level*)
  (let* ((missiles (level-missiles *level*))
         (ship-rect (ship-hitbox (level-ship *level*)))
         (asteroids (level-asteroids *level*))
         ;; so much redundant searching because hoot doesn't have a remove function
         ;; but it's the last day and I gotta finish aaaaaaaaa
         (colliding-asteroids (filter (lambda (a) (asteroid-hit-missile? a missiles))
                                        asteroids))
         (non-colliding-asteroids (filter (lambda (a) (not (asteroid-hit-missile? a missiles)))
                                          asteroids))
         (non-colliding-missiles (filter (lambda (m) (not (missile-hit-asteroid? m asteroids)))
                                         missiles))
         (missiles-hitting-player (filter is-armed? 
                                          (filter (lambda (m) (missile-collides? ship-rect m)) 
                                                  missiles))))
    (add-score-for-asteroids *level* colliding-asteroids)
    (level-asteroids-set! *level* non-colliding-asteroids)
    (break-up-destroyed-asteroids *level* colliding-asteroids)
    (level-missiles-set! *level* non-colliding-missiles)
    (if (and (> (length missiles-hitting-player) 0) (ship-alive? (level-ship *level*)))
      (let ((missiles-missing-player (filter (lambda (m) (not (missile-collides? ship-rect m)))
                                            missiles)))
        (kill-player (level-ship *level*) *level*)
        (level-missiles-set! *level* missiles-missing-player)))))

(define (break-up-destroyed-asteroids *level* asteroids)
  (define (break-up-asteroid asteroid *level*)
    (let* ((hitbox (asteroid-hitbox asteroid))
           (x (rect-x hitbox))
           (y (rect-y hitbox))
           (type (asteroid-type asteroid)))
      (cond 
        ((eqv? type 'large)
          (add-asteroids *level* (map (lambda (_) (build-medium-asteroid-at x y)) (make-list 2 0))))
        ((eqv? type 'medium)
          (add-asteroids *level* (map (lambda (_) (build-small-asteroid-at x y)) (make-list 2 0))))
        (else 
          (add-ore *level* (map (lambda (_) (build-ore-at x y)) (make-list 2 0)))))))
  (for-each (lambda (a) (break-up-asteroid a *level*)) asteroids))

        
(define (update-all *level*)
  (let* ((ship (level-ship *level*))
         (lev-width (level-width *level*))
         (lev-height (level-height *level*)))
    (apply-commands ship *level*)
    (move-ship ship lev-width lev-height)
    (progress-fire-cooldown ship)
    (move-asteroids (level-asteroids *level*) lev-width lev-height)
    (move-ore (level-ore *level*) lev-width lev-height)
    (move-missiles (level-missiles *level*) lev-width lev-height))
  (handle-ship-collisions *level*)
  (handle-missile-collisions *level*)
  (check-for-new-asteroid *level*))

