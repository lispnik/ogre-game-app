;;;; model.lisp -- the rules of Behemoth.
;;;;
;;;; One enormous automated war machine, the Behemoth, rolls down from the top
;;;; of the map toward your command bunker at the bottom.  You command a
;;;; handful of defenders.  Each attack picks one target on the Behemoth: its
;;;; drive, which sets how far it moves, or one of its weapons.  The Behemoth
;;;; crushes whatever it rolls over, and if it reaches the bunker, or shoots
;;;; it to pieces, the battle is lost.  Stop it -- drive wrecked and nothing
;;;; left that can reach the bunker -- and it is won.
;;;;
;;;; Every chance in the game is ATTACK / (ATTACK + DEFENSE).
;;;;
;;;; Actions return events -- lists the view animates:
;;;;   (:move id path)                       a defender moved
;;;;   (:behemoth-move path)                 the Behemoth moved
;;;;   (:crush id damaged destroyed shove)   it rolled over a defender
;;;;   (:flatten hex)                        and turned rubble to open ground
;;;;   (:fire attacker target chance hit destroyed)
;;;;       ATTACKER is a unit id or (:system id); TARGET is a unit id,
;;;;       :drive, or (:system id)
;;;;   (:turn n)                             a new player turn
;;;;   (:outcome :won|:lost reason)

(in-package #:behemoth)

;;; ------------------------------------------------------------------
;;; kinds

(defparameter *unit-kinds*
  '((:lancer   :name "Lancer"   :glyph "L" :attack 4   :range 2 :defense 3 :move 3 :hp 2 :value 4)
    (:skimmer  :name "Skimmer"  :glyph "S" :attack 2   :range 2 :defense 2 :move 4 :hp 1 :value 3
               :hover t :scoot t)
    (:mortar   :name "Mortar"   :glyph "M" :attack 3   :range 4 :defense 1 :move 1 :hp 1 :value 3)
    (:infantry :name "Infantry" :glyph "I" :attack :hp :range 1 :defense 2 :move 2 :hp 3 :value 1
               :foot t)
    (:bunker   :name "Bunker"   :glyph "B" :attack 0   :range 0 :defense 4 :move 0 :hp 5 :value 100))
  "Infantry attack with one point per surviving squad.  Skimmers hover over
rubble and may keep moving after they fire.  Infantry climb ridges.")

(defun kind-property (kind property)
  (getf (rest (assoc kind *unit-kinds*)) property))

(defparameter *system-kinds*
  '((:cannon     :name "Main cannon" :attack 5 :range 3 :damage 2 :defense 6 :count 1)
    (:autocannon :name "Autocannon"  :attack 3 :range 2 :damage 1 :defense 3 :count 4)
    (:rocket     :name "Rocket"      :attack 5 :range 5 :damage 2 :defense 2 :count 2
                 :single-use t))
  "The Behemoth's weapons.  A hit on one destroys it.  A rocket is gone once fired.")

(defun system-property (kind property)
  (getf (rest (assoc kind *system-kinds*)) property))

(defparameter *max-drive* 24)
(defparameter *drive-defense* 5)
(defparameter *deploy-first-row* 6 "Defenders deploy on this row and below.")

(defun drive-speed (drive)
  "Hexes the Behemoth moves a turn: drive damage slows it, then stops it."
  (ceiling (* 3 (max drive 0)) *max-drive*))

(defun hit-chance (attack defense)
  (if (plusp attack) (/ attack (+ attack defense)) 0))

;;; ------------------------------------------------------------------
;;; pieces

(defstruct unit id kind col row hp (move-left 0) (fired nil) (moved nil))
(defstruct sys id kind (alive t) (fired nil))
(defstruct behemoth col row (drive *max-drive*) systems)

(defun unit-hex (unit) (hex (unit-col unit) (unit-row unit)))
(defun behemoth-hex (b) (hex (behemoth-col b) (behemoth-row b)))
(defun behemoth-speed (b) (drive-speed (behemoth-drive b)))
(defun unit-name (unit) (kind-property (unit-kind unit) :name))
(defun unit-glyph (unit) (kind-property (unit-kind unit) :glyph))
(defun unit-max-hp (unit) (kind-property (unit-kind unit) :hp))
(defun unit-range (unit) (kind-property (unit-kind unit) :range))
(defun sys-name (sys) (system-property (sys-kind sys) :name))

(defun unit-attack (unit)
  (let ((attack (kind-property (unit-kind unit) :attack)))
    (if (eq attack :hp) (unit-hp unit) attack)))

(defun combat-unit-p (unit) (not (eq (unit-kind unit) :bunker)))

(defun live-systems (b)
  (remove-if-not #'sys-alive (behemoth-systems b)))

(defun find-system (b id)
  (find id (behemoth-systems b) :key #'sys-id))

;;; ------------------------------------------------------------------
;;; the game

(defstruct game
  (cols *cols*) (rows *rows*) terrain (units '()) behemoth rng
  (turn 0) (phase :deploy) (log '()) outcome seed (next-id 0))

(defun note (game control &rest arguments)
  (push (apply #'format nil control arguments) (game-log game)))

(defun terrain-at (game hex)
  (aref (game-terrain game) (hex-col hex) (hex-row hex)))

(defun unit-at (game hex)
  (find-if (lambda (u) (equal (unit-hex u) hex)) (game-units game)))

(defun find-unit (game id)
  (find id (game-units game) :key #'unit-id))

(defun bunker (game)
  (find :bunker (game-units game) :key #'unit-kind))

(defun add-unit (game kind col row)
  (let ((unit (make-unit :id (incf (game-next-id game)) :kind kind :col col :row row
                         :hp (kind-property kind :hp))))
    (setf (game-units game) (append (game-units game) (list unit)))
    unit))

(defun remove-unit (game unit)
  (setf (game-units game) (remove unit (game-units game))))

(defun unit-defense (game unit)
  "Rubble gives one point of cover."
  (+ (kind-property (unit-kind unit) :defense)
     (if (eq (terrain-at game (unit-hex unit)) :rubble) 1 0)))

;;; ------------------------------------------------------------------
;;; terrain

(defun behemoth-passable-p (game hex)
  (not (eq (terrain-at game hex) :ridge)))

(defun behemoth-route (game)
  "The Behemoth's route to the bunker, or NIL if there is none.  It prefers,
among equally short routes, the ones that roll over defenders."
  (let* ((goal (unit-hex (bunker game)))
         (costs (shortest-costs (behemoth-hex (game-behemoth game))
                                (lambda (h) (behemoth-passable-p game h))
                                (lambda (h) (if (unit-at game h) 0.9 1)))))
    (and (gethash goal costs) (path-to costs goal))))

(defun behemoth-distance-to-bunker (game)
  (length (behemoth-route game)))

(defun deploy-layout (bunker-col)
  "Where the defenders start: a line of Lancers, Skimmers ahead of it,
infantry behind, and mortars beside the bunker."
  `((:lancer 1 8) (:lancer 3 8) (:lancer 5 8) (:lancer 7 8)
    (:skimmer 2 7) (:skimmer 4 7) (:skimmer 6 7)
    (:infantry 2 9) (:infantry 4 9) (:infantry 6 9)
    (:mortar ,(1- bunker-col) 10) (:mortar ,(1+ bunker-col) 10)))

(defun scatter-terrain (game)
  (let* ((rng (game-rng game))
         (terrain (make-array (list (game-cols game) (game-rows game)) :initial-element :open)))
    (dotimes (col (game-cols game))
      (dotimes (row (game-rows game))
        (when (< (rng-float rng) 0.15d0)
          (setf (aref terrain col row) :rubble))))
    ;; A few short ridges.
    (dotimes (i 4)
      (let ((hex (hex (rng-below rng (game-cols game)) (+ 2 (rng-below rng 7))))
            (direction (rng-below rng 6)))
        (dotimes (j (+ 2 (rng-below rng 2)))
          (setf (aref terrain (hex-col hex) (hex-row hex)) :ridge)
          (let ((next (nth direction (hex-neighbors hex))))
            (if next (setf hex next) (return))))))
    terrain))

(defun new-game (&key (seed 1))
  "A battle in its deployment phase."
  (let ((game (make-game :rng (make-rng seed) :seed seed)))
    (loop
      (setf (game-units game) '()
            (game-next-id game) 0
            (game-terrain game) (scatter-terrain game))
      (let* ((rng (game-rng game))
             (bunker-col (+ 3 (rng-below rng 3)))
             (start (hex (+ 2 (rng-below rng 5)) 0)))
        (add-unit game :bunker bunker-col (1- (game-rows game)))
        (loop for (kind col row) in (deploy-layout bunker-col)
              do (add-unit game kind col row))
        (dolist (u (game-units game))
          (setf (aref (game-terrain game) (unit-col u) (unit-row u)) :open))
        (setf (aref (game-terrain game) (hex-col start) (hex-row start)) :open)
        (setf (game-behemoth game)
              (make-behemoth :col (hex-col start) :row (hex-row start)
                             :systems (loop with id = 0
                                            for (kind . properties) in *system-kinds*
                                            nconc (loop repeat (getf properties :count)
                                                        collect (make-sys :id (incf id) :kind kind)))))
        ;; A map with no way through for the Behemoth is no battle.
        (when (behemoth-route game) (return))))
    (note game "Deploy your defenders, then start the battle.")
    game))

;;; ------------------------------------------------------------------
;;; deployment

(defun deploy-hex-p (game hex)
  (and (on-map-p hex (game-cols game) (game-rows game))
       (>= (hex-row hex) *deploy-first-row*)
       (not (eq (terrain-at game hex) :ridge))))

(defun redeploy-unit (game id hex)
  "During deployment, put a defender anywhere in the deployment zone."
  (let ((unit (find-unit game id)))
    (when (and (eq (game-phase game) :deploy) unit (combat-unit-p unit)
               (deploy-hex-p game hex)
               (or (not (eq (terrain-at game hex) :ridge)) (kind-property (unit-kind unit) :foot))
               (null (unit-at game hex)))
      (setf (unit-col unit) (hex-col hex) (unit-row unit) (hex-row hex))
      t)))

(defun begin-battle (game)
  "End deployment.  The Behemoth rolls in; then it is the player's turn."
  (when (eq (game-phase game) :deploy)
    (setf (game-phase game) :behemoth)
    (note game "The Behemoth is coming.")
    (behemoth-turn game)))

;;; ------------------------------------------------------------------
;;; defenders' movement

(defun entry-cost (game unit hex)
  "What it costs UNIT to enter HEX, or NIL if it cannot."
  (let ((terrain (terrain-at game hex))
        (kind (unit-kind unit)))
    (cond ((equal hex (behemoth-hex (game-behemoth game))) nil)
          ((eq terrain :ridge) (and (kind-property kind :foot) 2))
          ((eq terrain :rubble) (if (or (kind-property kind :hover) (kind-property kind :foot)) 1 2))
          (t 1))))

(defun reachable (game unit)
  "Hash table of hex -> (cost . previous) for where UNIT can go this turn.
Defenders may pass through each other but not stop together."
  (shortest-costs (unit-hex unit)
                  (lambda (h) (entry-cost game unit h))
                  (lambda (h) (entry-cost game unit h))
                  :limit (unit-move-left unit)))

(defun reachable-hexes (game unit)
  "The hexes UNIT could stop on this turn."
  (let ((out '()))
    (maphash (lambda (hex entry)
               (declare (ignore entry))
               (unless (unit-at game hex) (push hex out)))
             (reachable game unit))
    out))

(defun move-unit (game id hex)
  "Move a defender.  Returns a :MOVE event, or NIL if the move is illegal."
  (let ((unit (find-unit game id)))
    (when (and unit (eq (game-phase game) :player) (combat-unit-p unit)
               (not (equal hex (unit-hex unit))) (null (unit-at game hex)))
      (let* ((costs (reachable game unit))
             (entry (gethash hex costs)))
        (when entry
          (let ((path (path-to costs hex)))
            (decf (unit-move-left unit) (car entry))
            (setf (unit-col unit) (hex-col hex) (unit-row unit) (hex-row hex)
                  (unit-moved unit) t)
            (list :move id path)))))))

;;; ------------------------------------------------------------------
;;; defenders' fire

(defun in-range-p (game unit)
  (<= (hex-distance (unit-hex unit) (behemoth-hex (game-behemoth game)))
      (unit-range unit)))

(defun target-defense (game target)
  (if (eq target :drive)
      *drive-defense*
      (system-property (sys-kind (find-system (game-behemoth game) target)) :defense)))

(defun target-alive-p (game target)
  (let ((b (game-behemoth game)))
    (if (eq target :drive)
        (plusp (behemoth-drive b))
        (let ((sys (find-system b target))) (and sys (sys-alive sys))))))

(defun target-chance (game unit target)
  (hit-chance (unit-attack unit) (target-defense game target)))

(defun fire-at (game id target)
  "A defender fires at TARGET: :DRIVE or a system id.  Returns a list of
events, or NIL if it cannot fire."
  (let ((unit (find-unit game id))
        (b (game-behemoth game)))
    (when (and unit (eq (game-phase game) :player) (combat-unit-p unit)
               (not (unit-fired unit)) (in-range-p game unit)
               (target-alive-p game target))
      (let* ((chance (target-chance game unit target))
             (hit (< (rng-float (game-rng game)) chance))
             (destroyed nil))
        (setf (unit-fired unit) t)
        (unless (kind-property (unit-kind unit) :scoot)
          (setf (unit-move-left unit) 0))
        (when hit
          (if (eq target :drive)
              (progn
                (setf (behemoth-drive b) (max 0 (- (behemoth-drive b) (unit-attack unit))))
                (setf destroyed (zerop (behemoth-drive b))))
              (setf (sys-alive (find-system b target)) nil
                    destroyed t)))
        (note game "~a fires at the ~a: ~a" (unit-name unit)
              (if (eq target :drive) "drive" (sys-name (find-system b target)))
              (cond ((and hit destroyed) (if (eq target :drive) "drive wrecked!" "destroyed!"))
                    (hit (format nil "hit, drive ~d/~d." (behemoth-drive b) *max-drive*))
                    (t "miss.")))
        (cons (list :fire id (if (eq target :drive) :drive (list :system target))
                    chance hit destroyed)
              (check-outcome game))))))

;;; ------------------------------------------------------------------
;;; winning and losing

(defun check-outcome (game)
  "Decide the battle if it is decided.  Returns a list of events."
  (flet ((finish (outcome reason)
           (setf (game-phase game) outcome
                 (game-outcome game) reason)
           (note game "~a" reason)
           (list (list :outcome outcome reason))))
    (let ((b (game-behemoth game))
          (bunker (bunker game)))
      (cond ((member (game-phase game) '(:won :lost)) '())
            ((null bunker)
             (finish :lost "The bunker has fallen."))
            ((notany #'combat-unit-p (game-units game))
             (finish :lost "Every defender is gone."))
            ((and (zerop (behemoth-drive b))
                  (notany (lambda (s)
                            (<= (hex-distance (behemoth-hex b) (unit-hex bunker))
                                (system-property (sys-kind s) :range)))
                          (live-systems b)))
             (finish :won "The Behemoth is stopped short of the bunker."))
            (t '())))))

;;; ------------------------------------------------------------------
;;; the Behemoth's turn

(defun damage-unit (game unit amount)
  "Returns true if UNIT is destroyed."
  (decf (unit-hp unit) amount)
  (when (<= (unit-hp unit) 0)
    (remove-unit game unit)
    t))

(defun shove-hex (game unit)
  "Somewhere free next to UNIT for it to be thrown aside to."
  (find-if (lambda (h)
             (and (null (unit-at game h))
                  (not (equal h (behemoth-hex (game-behemoth game))))
                  (entry-cost game unit h)))
           (hex-neighbors (unit-hex unit))))

(defun crush (game unit)
  "The Behemoth rolls over UNIT.  Infantry always lose a squad; anything
else is hit half the time.  Survivors are thrown aside."
  (let* ((rng (game-rng game))
         (damaged (or (eq (unit-kind unit) :infantry)
                      (eq (unit-kind unit) :bunker)
                      (< (rng-float rng) 0.5d0)))
         (destroyed (cond ((eq (unit-kind unit) :bunker) (remove-unit game unit) t)
                          (damaged (damage-unit game unit 1))))
         (shove nil))
    (unless destroyed
      (setf shove (shove-hex game unit))
      (if shove
          (setf (unit-col unit) (hex-col shove) (unit-row unit) (hex-row shove))
          (progn (remove-unit game unit) (setf destroyed t))))
    (note game "The Behemoth rolls over the ~a~a." (unit-name unit)
          (cond (destroyed ", destroying it") (damaged ", damaging it") (t "")))
    (list :crush (unit-id unit) damaged destroyed shove)))

(defun behemoth-moves (game)
  (let* ((b (game-behemoth game))
         (route (behemoth-route game))
         (steps (subseq route 0 (min (length route) (behemoth-speed b))))
         (events '()))
    (when steps
      (push (list :behemoth-move steps) events)
      (dolist (hex steps)
        (setf (behemoth-col b) (hex-col hex) (behemoth-row b) (hex-row hex))
        (when (eq (terrain-at game hex) :rubble)
          (setf (aref (game-terrain game) (hex-col hex) (hex-row hex)) :open)
          (push (list :flatten hex) events))
        (let ((victim (unit-at game hex)))
          (when victim
            (push (crush game victim) events)))
        (unless (bunker game) (return))))
    (nreverse events)))

(defun target-score (game sys unit)
  "How much the Behemoth wants SYS to fire at UNIT."
  (let* ((chance (hit-chance (system-property (sys-kind sys) :attack) (unit-defense game unit)))
         (damage (system-property (sys-kind sys) :damage))
         (kills (>= damage (unit-hp unit))))
    (* chance (kind-property (unit-kind unit) :value)
       (if kills 1.5 (/ damage (unit-hp unit))))))

(defun behemoth-fires (game)
  (let ((b (game-behemoth game))
        (rng (game-rng game))
        (events '()))
    (dolist (sys (live-systems b))
      (let* ((range (system-property (sys-kind sys) :range))
             (targets (remove-if-not (lambda (u)
                                       (<= (hex-distance (unit-hex u) (behemoth-hex b)) range))
                                     (game-units game)))
             (best (first (sort (copy-list targets) #'>
                                :key (lambda (u) (target-score game sys u))))))
        ;; Rockets are saved for something worth one.
        (when (and best
                   (or (not (system-property (sys-kind sys) :single-use))
                       (>= (target-score game sys best) 2)))
          (let* ((chance (hit-chance (system-property (sys-kind sys) :attack)
                                     (unit-defense game best)))
                 (hit (< (rng-float rng) chance))
                 (name (unit-name best))
                 (destroyed (and hit (damage-unit game best (system-property (sys-kind sys) :damage)))))
            (when (system-property (sys-kind sys) :single-use)
              (setf (sys-alive sys) nil))
            (note game "~a fires at the ~a: ~a" (sys-name sys) name
                  (cond (destroyed "destroyed!") (hit "hit.") (t "miss.")))
            (push (list :fire (list :system (sys-id sys)) (unit-id best) chance hit destroyed)
                  events)
            (unless (bunker game) (return))))))
    (nreverse events)))

(defun behemoth-turn (game)
  "The Behemoth moves, then fires every weapon.  Then a new player turn."
  (let ((events (append (behemoth-moves game)
                        (and (bunker game) (behemoth-fires game)))))
    (setf events (append events (check-outcome game)))
    (when (eq (game-phase game) :behemoth)
      (setf (game-phase game) :player)
      (incf (game-turn game))
      (dolist (u (game-units game))
        (setf (unit-move-left u) (kind-property (unit-kind u) :move)
              (unit-fired u) nil
              (unit-moved u) nil))
      (setf events (append events (list (list :turn (game-turn game))))))
    events))

(defun end-turn (game)
  "The player is done.  Returns the Behemoth's turn as events."
  (when (eq (game-phase game) :player)
    (setf (game-phase game) :behemoth)
    (behemoth-turn game)))
