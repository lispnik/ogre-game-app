;;;; auto.lisp -- a plain computer player for the defenders.
;;;;
;;;; Used for autoplay in the simulator and for balance runs on the host.
;;;; Not clever: every defender closes to its range and shoots, at the drive
;;;; when the Behemoth is near the bunker, otherwise at whichever weapon it
;;;; is likeliest to kill, weighted by what that weapon could do.

(in-package #:behemoth)

(defun threat (sys)
  (* (system-property (sys-kind sys) :attack) (system-property (sys-kind sys) :damage)))

(defun choose-target (game unit)
  (let* ((b (game-behemoth game))
         (weapons (live-systems b))
         (turns-left (let ((speed (behemoth-speed b)))
                       (if (zerop speed)
                           most-positive-fixnum
                           (/ (behemoth-distance-to-bunker game) speed)))))
    (if (or (null weapons)
            (and (plusp (behemoth-drive b)) (<= turns-left 3)))
        (if (plusp (behemoth-drive b)) :drive (and weapons (sys-id (first weapons))))
        (sys-id (first (sort (copy-list weapons) #'>
                             :key (lambda (s)
                                    (* (threat s)
                                       (target-chance game unit (sys-id s))))))))))

(defun best-firing-hex (game unit)
  "Where UNIT should move: in range of the Behemoth, as far from it as its
range allows; failing that, as close as it can get."
  (let* ((there (behemoth-hex (game-behemoth game)))
         (range (unit-range unit))
         (hexes (cons (unit-hex unit) (reachable-hexes game unit))))
    (first (sort hexes #'<
                 :key (lambda (h)
                        (let ((d (hex-distance h there)))
                          (if (<= d range) (- range d) (+ 10 d))))))))

(defun auto-step (game)
  "Make one defender action.  Returns its events, or NIL when every
defender is done for the turn."
  (when (eq (game-phase game) :player)
    (dolist (unit (game-units game))
      (when (and (combat-unit-p unit) (not (unit-fired unit)))
        (cond ((in-range-p game unit)
               (let ((target (choose-target game unit)))
                 (when target
                   (return (fire-at game (unit-id unit) target)))))
              ((not (unit-moved unit))
               (let ((hex (best-firing-hex game unit)))
                 (setf (unit-moved unit) t)
                 (unless (equal hex (unit-hex unit))
                   (return (list (move-unit game (unit-id unit) hex)))))))))))

(defun auto-play-turn (game)
  "Play the defenders' whole turn and end it.  Returns all its events."
  (let ((events '()))
    (loop for step = (auto-step game)
          while step do (setf events (append events step)))
    (append events (end-turn game))))
