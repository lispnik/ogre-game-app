;;;; tests.lisp -- FiveAM tests for the Behemoth rules.
;;;;
;;;;   sbcl --non-interactive --eval '(asdf:test-system "behemoth")'

(defpackage #:behemoth/tests
  (:use #:cl #:behemoth #:fiveam)
  (:export #:behemoth-tests))

(in-package #:behemoth/tests)

(def-suite behemoth-tests :description "The rules of Behemoth.")
(in-suite behemoth-tests)

;;; ------------------------------------------------------------------
;;; helpers

(defun clear-board (game)
  "Open ground everywhere, so a test can place what it needs."
  (let ((terrain (game-terrain game)))
    (dotimes (c (game-cols game))
      (dotimes (r (game-rows game))
        (setf (aref terrain c r) :open))))
  game)

(defun place (game id col row)
  (let ((u (find-unit game id)))
    (setf (unit-col u) col (unit-row u) row)
    u))

(defun only-units (game &rest kinds)
  "Keep the bunker and the first defender of each of KINDS; drop the rest."
  (let ((kept (list (bunker game))))
    (dolist (kind kinds)
      (push (find-if (lambda (u) (and (eq (unit-kind u) kind) (not (member u kept))))
                     (game-units game))
            kept))
    (setf (game-units game) (remove-if-not (lambda (u) (member u kept)) (game-units game)))
    (reverse kept)))

(defun put-behemoth (game col row)
  (let ((b (game-behemoth game)))
    (setf (behemoth-col b) col (behemoth-row b) row)
    b))

(defun battle (seed)
  "A fresh game moved straight to the player's first turn."
  (let ((game (new-game :seed seed)))
    (begin-battle game)
    game))

(defun rigged-rng (value)
  "An RNG whose next draw is close to VALUE, by searching seeds."
  (loop for seed from 1
        for rng = (make-rng seed)
        when (< (abs (- (rng-float (make-rng seed)) value)) 0.01d0)
          return rng))

;;; ------------------------------------------------------------------
;;; random numbers and hexes

(test rng-is-deterministic
  (let ((a (make-rng 42)) (b (make-rng 42)))
    (is (equal (loop repeat 20 collect (rng-next a))
               (loop repeat 20 collect (rng-next b))))))

(test rng-float-range
  (let ((rng (make-rng 7)))
    (is (every (lambda (x) (and (<= 0 x) (< x 1)))
               (loop repeat 1000 collect (rng-float rng))))))

(test rng-below-range
  (let ((rng (make-rng 9)))
    (is (every (lambda (x) (<= 0 x 5)) (loop repeat 1000 collect (rng-below rng 6))))))

(test hex-distance-basics
  (is (= 0 (hex-distance (hex 3 3) (hex 3 3))))
  (is (= 1 (hex-distance (hex 3 3) (hex 3 4))))
  (is (= 5 (hex-distance (hex 0 0) (hex 0 5))))
  ;; Odd columns sit half a hex lower, so (1,0) touches (0,0) and (0,1).
  (is (= 1 (hex-distance (hex 0 0) (hex 1 0))))
  (is (= 1 (hex-distance (hex 0 1) (hex 1 0))))
  (is (= 2 (hex-distance (hex 0 0) (hex 2 0)))))

(test hex-distance-symmetric
  (loop for (a b) in (list (list (hex 1 2) (hex 7 9)) (list (hex 0 11) (hex 8 0))
                           (list (hex 4 4) (hex 5 7)))
        do (is (= (hex-distance a b) (hex-distance b a)))))

(test neighbours-are-adjacent
  (dolist (hex (list (hex 4 5) (hex 3 5) (hex 0 0) (hex 8 11)))
    (let ((ns (hex-neighbors hex)))
      (is (every (lambda (n) (= 1 (hex-distance hex n))) ns))
      (is (= (length ns) (length (remove-duplicates ns :test #'equal)))))))

(test neighbour-counts
  (is (= 6 (length (hex-neighbors (hex 4 5)))))
  (is (= 6 (length (hex-neighbors (hex 3 5)))))
  (is (= 2 (length (hex-neighbors (hex 0 0)))))
  (is (every #'on-map-p (hex-neighbors (hex 8 11)))))

;;; ------------------------------------------------------------------
;;; numbers

(test hit-chances
  (is (= 1/2 (hit-chance 4 4)))
  (is (= 3/4 (hit-chance 3 1)))
  (is (= 0 (hit-chance 0 3))))

(test drive-slows-then-stops
  (is (equal (quote (3 3 2 2 1 1 0))
             (mapcar (function drive-speed) (quote (24 17 16 9 8 1 0))))))

;;; ------------------------------------------------------------------
;;; setting up

(test new-games-are-reproducible
  (let ((a (new-game :seed 123)) (b (new-game :seed 123)))
    (is (equalp (game-terrain a) (game-terrain b)))
    (is (equal (mapcar #'unit-hex (game-units a)) (mapcar #'unit-hex (game-units b))))
    (is (equal (behemoth-hex (game-behemoth a)) (behemoth-hex (game-behemoth b))))))

(test new-game-layout
  (dolist (seed '(1 2 3 50 999))
    (let* ((game (new-game :seed seed))
           (b (game-behemoth game)))
      (is (eq :deploy (game-phase game)))
      (is (= 0 (behemoth-row b)))
      (is (= *max-drive* (behemoth-drive b)))
      (is (= 7 (length (behemoth-systems b))))
      (is (= (1- (game-rows game)) (unit-row (bunker game))))
      (is (= 13 (length (game-units game))))
      ;; Nothing stacked, nothing on a ridge.
      (is (= 13 (length (remove-duplicates (mapcar #'unit-hex (game-units game)) :test #'equal))))
      (is (notany (lambda (u) (eq :ridge (terrain-at game (unit-hex u)))) (game-units game)))
      ;; And the Behemoth can always get through.
      (is (plusp (behemoth-distance-to-bunker game))))))

(test redeployment
  (let* ((game (clear-board (new-game :seed 4)))
         (lancer (find :lancer (game-units game) :key #'unit-kind)))
    (is-true (redeploy-unit game (unit-id lancer) (hex 0 6)))
    (is (equal (hex 0 6) (unit-hex lancer)))
    ;; Not outside the zone, not onto another unit, not the bunker.
    (is-false (redeploy-unit game (unit-id lancer) (hex 0 5)))
    (is-false (redeploy-unit game (unit-id lancer) (unit-hex (bunker game))))
    (is-false (redeploy-unit game (unit-id (bunker game)) (hex 0 7)))
    ;; Not after the battle has begun.
    (begin-battle game)
    (is-false (redeploy-unit game (unit-id lancer) (hex 0 7)))))

(test battle-begins-with-the-behemoth-moving
  (let* ((game (new-game :seed 11))
         (events (begin-battle game)))
    (is (eq :player (game-phase game)))
    (is (= 1 (game-turn game)))
    (is (eq :behemoth-move (first (first events))))
    (is (= 3 (behemoth-row (game-behemoth game))))
    (is (eq :turn (first (car (last events)))))))

;;; ------------------------------------------------------------------
;;; moving

(test movement-allowance
  (let* ((game (clear-board (battle 5)))
         (units (only-units game :lancer))
         (lancer (second units)))
    (put-behemoth game 4 0)
    (place game (unit-id lancer) 1 8)
    (is (= 3 (unit-move-left lancer)))
    (is (null (move-unit game (unit-id lancer) (hex 1 4))) "four hexes is too far")
    (let ((event (move-unit game (unit-id lancer) (hex 1 5))))
      (is (eq :move (first event)))
      (is (= 3 (length (third event))))
      (is (= 0 (unit-move-left lancer))))
    (is (null (move-unit game (unit-id lancer) (hex 1 4))) "nothing left")))

(test rubble-slows-vehicles-not-skimmers
  (let* ((game (clear-board (battle 5)))
         (units (only-units game :lancer :skimmer))
         (lancer (second units))
         (skimmer (third units)))
    (put-behemoth game 8 0)
    (place game (unit-id lancer) 1 8)
    (place game (unit-id skimmer) 3 8)
    (setf (aref (game-terrain game) 1 7) :rubble
          (aref (game-terrain game) 1 6) :rubble
          (aref (game-terrain game) 0 7) :ridge
          (aref (game-terrain game) 2 7) :ridge
          (aref (game-terrain game) 0 8) :ridge
          (aref (game-terrain game) 2 8) :ridge
          (aref (game-terrain game) 3 7) :rubble)
    (is (null (move-unit game (unit-id lancer) (hex 1 5))) "rubble costs a Lancer 2")
    (is-true (move-unit game (unit-id lancer) (hex 1 7)))
    (is-true (move-unit game (unit-id skimmer) (hex 3 4)) "hovering over rubble costs 1")))

(test ridges-stop-vehicles-not-infantry
  (let* ((game (clear-board (battle 5)))
         (units (only-units game :lancer :infantry))
         (lancer (second units))
         (infantry (third units)))
    (put-behemoth game 8 0)
    (place game (unit-id lancer) 4 8)
    (place game (unit-id infantry) 2 8)
    (setf (aref (game-terrain game) 4 7) :ridge
          (aref (game-terrain game) 2 7) :ridge)
    (is (null (move-unit game (unit-id lancer) (hex 4 7))))
    (is-true (move-unit game (unit-id infantry) (hex 2 7)))))

(test no-stopping-on-others
  (let* ((game (clear-board (battle 5)))
         (units (only-units game :lancer :skimmer))
         (lancer (second units))
         (skimmer (third units)))
    (put-behemoth game 8 0)
    (place game (unit-id lancer) 4 8)
    (place game (unit-id skimmer) 4 7)
    (is (null (move-unit game (unit-id lancer) (hex 4 7))))
    (is-true (move-unit game (unit-id lancer) (hex 4 6)) "but passing through is fine")))

(test no-entering-the-behemoth
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer))))
    (put-behemoth game 4 6)
    (place game (unit-id lancer) 4 8)
    (is (null (move-unit game (unit-id lancer) (hex 4 6))))
    (is (not (member (hex 4 6) (reachable-hexes game lancer) :test #'equal)))))

;;; ------------------------------------------------------------------
;;; firing

(test out-of-range
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer))))
    (put-behemoth game 4 2)
    (place game (unit-id lancer) 4 8)
    (is (null (fire-at game (unit-id lancer) :drive)))))

(test firing-once-a-turn
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer))))
    (put-behemoth game 4 6)
    (place game (unit-id lancer) 4 8)
    (let ((events (fire-at game (unit-id lancer) :drive)))
      (is (eq :fire (first (first events))))
      (is (= 4/9 (fourth (first events)))))
    (is (null (fire-at game (unit-id lancer) :drive)))
    (is (= 0 (unit-move-left lancer)) "firing ends a Lancer's movement")))

(test skimmers-scoot
  (let* ((game (clear-board (battle 5)))
         (skimmer (second (only-units game :skimmer))))
    (put-behemoth game 4 6)
    (place game (unit-id skimmer) 4 8)
    (is-true (fire-at game (unit-id skimmer) :drive))
    (is (= 4 (unit-move-left skimmer)))
    (is-true (move-unit game (unit-id skimmer) (hex 4 10)))))

(test a-hit-on-the-drive
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer)))
         (b (put-behemoth game 4 6)))
    (place game (unit-id lancer) 4 8)
    (setf (behemoth::game-rng game) (rigged-rng 0.1d0))
    (let ((event (first (fire-at game (unit-id lancer) :drive))))
      (is-true (fifth event))
      (is (= 20 (behemoth-drive b))))))

(test a-miss
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer)))
         (b (put-behemoth game 4 6)))
    (place game (unit-id lancer) 4 8)
    (setf (behemoth::game-rng game) (rigged-rng 0.9d0))
    (let ((event (first (fire-at game (unit-id lancer) :drive))))
      (is-false (fifth event))
      (is (= *max-drive* (behemoth-drive b))))))

(test a-hit-destroys-a-weapon
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer)))
         (b (put-behemoth game 4 6))
         (cannon (find :cannon (behemoth-systems b) :key #'sys-kind)))
    (place game (unit-id lancer) 4 8)
    (setf (behemoth::game-rng game) (rigged-rng 0.05d0))
    (let ((event (first (fire-at game (unit-id lancer) (sys-id cannon)))))
      (is-true (sixth event))
      (is-false (sys-alive cannon)))
    (is (null (fire-at game (unit-id lancer) (sys-id cannon))) "and it stays destroyed")))

(test infantry-attack-with-their-squads
  (let* ((game (clear-board (battle 5)))
         (infantry (second (only-units game :infantry))))
    (is (= 3 (unit-attack infantry)))
    (setf (unit-hp infantry) 1)
    (is (= 1 (unit-attack infantry)))))

(test rubble-is-cover
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer))))
    (place game (unit-id lancer) 4 8)
    (is (= 3 (unit-defense game lancer)))
    (setf (aref (game-terrain game) 4 8) :rubble)
    (is (= 4 (unit-defense game lancer)))))

;;; ------------------------------------------------------------------
;;; the Behemoth

(test it-heads-for-the-bunker
  (let* ((game (clear-board (new-game :seed 8)))
         (before (behemoth-distance-to-bunker game)))
    (setf (game-units game) (list (bunker game)
                                  (find :mortar (game-units game) :key #'unit-kind)))
    (begin-battle game)
    (is (= (- before 3) (behemoth-distance-to-bunker game)))))

(test it-slows-when-its-drive-is-hurt
  (let* ((game (clear-board (new-game :seed 8))))
    (setf (game-units game) (list (bunker game)
                                  (find :mortar (game-units game) :key #'unit-kind)))
    (setf (behemoth-drive (game-behemoth game)) 3)
    (begin-battle game)
    (is (= 1 (behemoth-row (game-behemoth game))))))

(test it-goes-around-ridges
  (let* ((game (clear-board (new-game :seed 8))))
    (setf (game-units game) (list (bunker game)))
    (put-behemoth game 4 0)
    (setf (unit-col (bunker game)) 4)
    (dolist (c '(3 4 5)) (setf (aref (game-terrain game) c 1) :ridge))
    (is (> (behemoth-distance-to-bunker game) 11))))

(test it-flattens-rubble
  (let* ((game (clear-board (new-game :seed 8)))
         (b (put-behemoth game 4 0)))
    (setf (game-units game) (list (bunker game)
                                  (find :mortar (game-units game) :key #'unit-kind)))
    (setf (unit-col (bunker game)) 4)
    (dotimes (r 12) (setf (aref (game-terrain game) 4 r) :rubble))
    (let ((events (begin-battle game)))
      (is (= 3 (behemoth-row b)))
      (is (= 3 (count :flatten events :key #'first)))
      (is (eq :open (terrain-at game (hex 4 2)))))))

(test it-crushes-infantry
  (let* ((game (clear-board (new-game :seed 8)))
         (b (put-behemoth game 4 0))
         (infantry (find :infantry (game-units game) :key #'unit-kind)))
    (setf (game-units game) (list (bunker game) infantry))
    (setf (unit-col (bunker game)) 4)
    (setf (unit-col infantry) 4 (unit-row infantry) 2)
    (dolist (s (behemoth-systems b)) (setf (behemoth::sys-alive s) nil))
    (let ((events (begin-battle game)))
      (is (= 1 (count :crush events :key #'first)))
      (is (= 2 (unit-hp infantry)))
      (is (not (equal (unit-hex infantry) (behemoth-hex b))) "survivors are thrown aside"))))

(test it-shoots
  (let* ((game (clear-board (new-game :seed 8)))
         (lancer (find :lancer (game-units game) :key #'unit-kind)))
    (setf (game-units game) (list (bunker game) lancer))
    (put-behemoth game 4 0)
    (setf (unit-col (bunker game)) 4)
    (setf (unit-col lancer) 6 (unit-row lancer) 4)
    (let ((fire (remove :fire (begin-battle game) :key #'first :test-not #'eq)))
      (is (plusp (length fire)))
      (is (every (lambda (e) (eql (unit-id lancer) (third e))) fire)))))

(test rockets-are-saved
  (let* ((game (clear-board (new-game :seed 8)))
         (infantry (find :infantry (game-units game) :key #'unit-kind))
         (b (put-behemoth game 4 0)))
    (setf (game-units game) (list (bunker game) infantry))
    (setf (unit-col (bunker game)) 0)
    ;; Infantry five hexes from where it will stop: only rockets reach.
    (setf (unit-col infantry) 8 (unit-row infantry) 7)
    (begin-battle game)
    (is (= 2 (count :rocket (live-systems b) :key #'sys-kind)))))

;;; ------------------------------------------------------------------
;;; winning and losing

(test reaching-the-bunker-loses
  (let* ((game (clear-board (new-game :seed 8))))
    (setf (game-units game) (list (bunker game)
                                  (find :mortar (game-units game) :key #'unit-kind)))
    (put-behemoth game (unit-col (bunker game)) 9)
    (let ((events (begin-battle game)))
      (is (eq :lost (game-phase game)))
      (is (equal '(:outcome :lost "The bunker has fallen.")
                 (find :outcome events :key #'first))))))

(test stopping-it-wins
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer)))
         (b (put-behemoth game 4 2)))
    (place game (unit-id lancer) 4 4)
    (setf (behemoth-drive b) 1)
    (setf (behemoth::game-rng game) (rigged-rng 0.05d0))
    (let ((events (fire-at game (unit-id lancer) :drive)))
      (is (eq :won (game-phase game)))
      (is (eq :won (second (find :outcome events :key #'first)))))))

(test stopped-but-in-range-is-not-a-win
  (let* ((game (clear-board (battle 5)))
         (lancer (second (only-units game :lancer)))
         (b (put-behemoth game (unit-col (bunker game)) 8)))
    (place game (unit-id lancer) (unit-col (bunker game)) 6)
    (setf (behemoth-drive b) 1)
    (setf (behemoth::game-rng game) (rigged-rng 0.05d0))
    (fire-at game (unit-id lancer) :drive)
    (is (zerop (behemoth-drive b)))
    (is (eq :player (game-phase game)) "its cannon still reaches the bunker")))

(test losing-every-defender-loses
  (let* ((game (clear-board (battle 5))))
    (setf (game-units game) (list (bunker game)))
    (end-turn game)
    (is (eq :lost (game-phase game)))))

(test nothing-happens-after-the-end
  (let ((game (battle 5)))
    (setf (behemoth::game-phase game) :won)
    (is (null (end-turn game)))
    (is (null (fire-at game (unit-id (second (game-units game))) :drive)))))

;;; ------------------------------------------------------------------
;;; whole games

(test computer-games-finish
  (dolist (seed '(1 2 3 4 5 6 7 8 9 10))
    (let ((game (battle seed)))
      (loop repeat 60
            while (eq (game-phase game) :player)
            do (auto-play-turn game))
      (is (member (game-phase game) '(:won :lost))
          "seed ~d did not finish: phase ~a" seed (game-phase game)))))
