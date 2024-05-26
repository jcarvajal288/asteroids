(define-module (level)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (ship)
  #:use-module (asteroid)
  #:export (make-default-level
            make-level
            level-width
            level-height
            level-ship
            level-score
            level-asteroids
            level-asteroids-set!
            level-ore
            level-missiles
            level-missiles-set!
            add-score-for-asteroids
            check-for-new-asteroid
            kill-player
            add-asteroids
            add-ore))

(define default-asteroid-timer 300)

(define-record-type <level>
  (make-level width height ship score new-asteroid-timer asteroids ore missiles)
  level?
  (width level-width)
  (height level-height)
  (ship level-ship level-ship-set!)
  (score level-score level-score-set!)
  (new-asteroid-timer level-new-asteroid-timer level-new-asteroid-timer-set!)
  (asteroids level-asteroids level-asteroids-set!)
  (ore level-ore level-ore-set!)
  (missiles level-missiles level-missiles-set!))

(define (make-default-level)
  (let* ((width 1280.0)
         (height 1024.0)
         (ship (make-default-ship width height))
         (score 0)
         (asteroids (map (lambda (_) (build-large-asteroid width height)) (make-list 3 0)))
         (missiles '())
         (ore '()))
    (make-level width height ship score default-asteroid-timer asteroids ore missiles)))

(define (add-score-for-asteroids *level* asteroids)
  (if (ship-alive? (level-ship *level*))
    (let ((new-points (apply + (map asteroid-score-value asteroids))))
      (level-score-set! *level* (+ (level-score *level*) new-points)))))

(define (check-for-new-asteroid *level*)
  (level-new-asteroid-timer-set! *level* (- (level-new-asteroid-timer *level*) 1))
  (if (and (<= (level-new-asteroid-timer *level*) 0) (ship-alive? (level-ship *level*)))
    (let ((new-asteroid (build-random-asteroid (level-width *level*) (level-height *level*))))
      (level-asteroids-set! *level* (append (list new-asteroid) (level-asteroids *level*)))
    (level-new-asteroid-timer-set! *level* default-asteroid-timer))))

(define (kill-player ship *level*)
  (ship-alive-set! ship #f)
  (spawn-debris (rect-x (ship-hitbox ship)) (rect-y (ship-hitbox ship)) *level*))

(define (spawn-debris x y *level*)
  (let ((debris (map (lambda (_) (build-debris-at x y)) (make-list 5 0))))
    (level-asteroids-set! *level* (append debris (level-asteroids *level*)))))

(define (add-asteroids *level* asteroids)
  (level-asteroids-set! *level* (append asteroids (level-asteroids *level*))))

(define (add-ore *level* ore)
  (level-ore-set! *level* (append ore (level-ore *level*))))
