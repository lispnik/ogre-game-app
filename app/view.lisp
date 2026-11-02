;;;; view.lisp -- Behemoth drawn by SpriteKit.
;;;;
;;;; Everything is laid out in a 390x800 design space and scaled to fit the
;;;; safe area.  The hexes and pieces are vector shapes: UIBezierPath paths
;;;; handed to SKShapeNode.  The rules return events; the view plays them in
;;;; order with SKActions, then re-reads the model so that what is drawn is
;;;; always what the rules say.

(in-package #:behemoth-app)

(defparameter *scene-width* 390d0)
(defparameter *scene-height* 800d0)
(defparameter *radius* 27d0 "Centre to corner of a hex.")
(defparameter *hex-height* (* (sqrt 3d0) *radius*))
(defparameter *map-top* 694d0)
(defparameter *map-left* 6d0)

(defvar *view* nil)
(defvar *scene* nil)
(defvar *world* nil "Everything that scales with the board.")
(defvar *marks* nil "Selection highlights, rebuilt on every change.")
(defvar *game* nil)
(defvar *hex-nodes* nil "Array of the terrain shapes, by column and row.")
(defvar *hex-terrain* nil "The terrain each of those shapes was drawn for.")
(defvar *unit-nodes* (make-hash-table) "Unit id -> (node . hp drawn).")
(defvar *behemoth-node* nil)
(defvar *chips* nil "One (system-id node label percent) per weapon.")
(defvar *drive-segments* nil)
(defvar *drive-label* nil)
(defvar *drive-percent* nil)
(defvar *status-label* nil)
(defvar *log-labels* nil)
(defvar *action-button* nil)
(defvar *action-label* nil)
(defvar *record-label* nil)
(defvar *overlay* nil)
(defvar *help* nil)
(defvar *selected* nil "Id of the selected defender.")
(defvar *busy* nil "True while events are playing.")
(defvar *fitted* nil)

;;; ------------------------------------------------------------------
;;; record, kept in NSUserDefaults

(defun defaults () (objc:invoke "NSUserDefaults" "standardUserDefaults"))
(defun saved (key) (objc:invoke (defaults) "integerForKey:" key))
(defun save (key value) (objc:invoke (defaults) "setInteger:forKey:" value key))

;;; ------------------------------------------------------------------
;;; nodes

(defun point (x y) (vector (float x 1d0) (float y 1d0)))
(defun rgb (r g b &optional (a 1)) (ui:color r g b a))

(defun add (parent child)
  (objc:invoke parent "addChild:" child)
  child)

(defun at (node x y)
  (objc:invoke node "setPosition:" (point x y))
  node)

(defun z (node z)
  (objc:invoke node "setZPosition:" (float z 1d0))
  node)

(defun style (node fill &optional stroke (line-width 1.5d0))
  (objc:invoke node "setFillColor:" (or fill (rgb 0 0 0 0)))
  (objc:invoke node "setStrokeColor:" (or stroke (rgb 0 0 0 0)))
  (objc:invoke node "setLineWidth:" (float line-width 1d0))
  node)

(defun polygon (points fill &optional stroke (line-width 1.5d0))
  "A closed shape through POINTS, a list of (x y)."
  (let ((path (objc:invoke "UIBezierPath" "bezierPath")))
    (loop for (x y) in points
          for first = t then nil
          do (objc:invoke path (if first "moveToPoint:" "addLineToPoint:") (point x y)))
    (objc:invoke path "closePath")
    (style (objc:invoke "SKShapeNode" "shapeNodeWithPath:" (objc:invoke path "CGPath"))
           fill stroke line-width)))

(defun line (x1 y1 x2 y2 colour &optional (width 2d0))
  (let ((path (objc:invoke "UIBezierPath" "bezierPath")))
    (objc:invoke path "moveToPoint:" (point x1 y1))
    (objc:invoke path "addLineToPoint:" (point x2 y2))
    (let ((node (objc:invoke "SKShapeNode" "shapeNodeWithPath:" (objc:invoke path "CGPath"))))
      (objc:invoke node "setStrokeColor:" colour)
      (objc:invoke node "setLineWidth:" (float width 1d0))
      (objc:invoke node "setLineCap:" 1)
      node)))

(defun circle (radius fill &optional stroke (line-width 1.5d0))
  (style (objc:invoke "SKShapeNode" "shapeNodeWithCircleOfRadius:" (float radius 1d0))
         fill stroke line-width))

(defun rounded (width height radius fill &optional stroke (line-width 2))
  (style (objc:invoke "SKShapeNode" "shapeNodeWithRect:cornerRadius:"
                      (vector (- (/ width 2d0)) (- (/ height 2d0))
                              (float width 1d0) (float height 1d0))
                      (float radius 1d0))
         fill stroke line-width))

(defun box (width height fill)
  (objc:invoke "SKSpriteNode" "spriteNodeWithColor:size:" fill (point width height)))

(defun text (string size colour &key (font "AvenirNext-Bold") (align :centre))
  (let ((node (objc:invoke "SKLabelNode" "labelNodeWithFontNamed:" font)))
    (objc:invoke node "setText:" string)
    (objc:invoke node "setFontSize:" (float size 1d0))
    (objc:invoke node "setFontColor:" colour)
    (objc:invoke node "setVerticalAlignmentMode:" 1)
    (objc:invoke node "setHorizontalAlignmentMode:" (ecase align (:centre 0) (:left 1) (:right 2)))
    node))

(defun set-text (label string) (objc:invoke label "setText:" string))
(defun remove-node (node) (objc:invoke node "removeFromParent"))

(defun action (name &rest arguments) (apply #'objc:invoke "SKAction" name arguments))

(defun run (node &rest actions)
  (objc:invoke node "runAction:"
               (if (rest actions) (action "sequence:" (coerce actions 'vector)) (first actions))))

(defun move-to (x y seconds) (action "moveTo:duration:" (point x y) (float seconds 1d0)))
(defun wait (seconds) (action "waitForDuration:" (float seconds 1d0)))
(defun fade-out (seconds) (action "fadeOutWithDuration:" (float seconds 1d0)))

(defun after (seconds thunk)
  (ui:after-every seconds (lambda (timer) (declare (ignore timer)) (funcall thunk)) :repeats nil))

;;; ------------------------------------------------------------------
;;; haptics and sound

(defvar *impacts* (make-hash-table))
(defvar *sounds* (make-hash-table :test 'equal))

(defun haptic (&optional (style 0))
  (when (zerop (saved "no vibration"))
    (objc:invoke (or (gethash style *impacts*)
                     (setf (gethash style *impacts*)
                           (ui:keep (objc:invoke (objc:invoke "UIImpactFeedbackGenerator" "alloc")
                                                 "initWithStyle:" style))))
                 "impactOccurred")))

(defun sound (name)
  (when (zerop (saved "no sound"))
    (objc:invoke *scene* "runAction:"
                 (or (gethash name *sounds*)
                     (setf (gethash name *sounds*)
                           (ui:keep (action "playSoundFileNamed:waitForCompletion:"
                                            (format nil "~a.wav" name) nil)))))))

;;; ------------------------------------------------------------------
;;; the map

(defun hex-x (col) (+ *map-left* *radius* (* 1.5d0 *radius* col)))

(defun hex-y (col row)
  (- *map-top* (/ *hex-height* 2) (* *hex-height* row) (if (oddp col) (/ *hex-height* 2) 0)))

(defun hex-point (hex) (values (hex-x (hex-col hex)) (hex-y (hex-col hex) (hex-row hex))))

(defun hexagon-points (radius)
  (loop for i below 6
        for a = (* i (/ pi 3))
        collect (list (* radius (cos a)) (* radius (sin a)))))

(defun hex-at-point (x y)
  "The hex whose centre is nearest (X, Y), if it is inside the map."
  (let ((best nil) (best-d most-positive-double-float))
    (dotimes (col (game-cols *game*))
      (dotimes (row (game-rows *game*))
        (let ((d (+ (expt (- x (hex-x col)) 2) (expt (- y (hex-y col row)) 2))))
          (when (< d best-d) (setf best (hex col row) best-d d)))))
    (and best (< best-d (expt *radius* 2)) best)))

(defun terrain-fill (terrain row)
  (let ((zone (and (eq (game-phase *game*) :deploy) (>= row *deploy-first-row*))))
    (ecase terrain
      (:open (if zone (rgb 0.24 0.33 0.26) (rgb 0.20 0.27 0.22)))
      (:rubble (if zone (rgb 0.38 0.34 0.26) (rgb 0.33 0.29 0.22)))
      (:ridge (rgb 0.46 0.44 0.41)))))

(defun decorate-hex (node terrain)
  "Rubble gets stones, a ridge a peak."
  (ecase terrain
    (:open)
    (:rubble
     (loop for (x y r) in '((-8 -5 3.5) (5 -8 2.5) (7 5 3) (-3 7 2) (-11 4 1.8))
           do (add node (at (circle r (rgb 0.52 0.46 0.36) (rgb 0.25 0.22 0.17) 1) x y))))
    (:ridge
     (add node (polygon '((-16 -9) (-5 9) (1 1) (6 7) (16 -9)) (rgb 0.60 0.58 0.55)
                        (rgb 0.30 0.29 0.27) 1.5d0)))))

(defun build-map ()
  (setf *hex-nodes* (make-array (list (game-cols *game*) (game-rows *game*)))
        *hex-terrain* (make-array (list (game-cols *game*) (game-rows *game*))))
  (dotimes (col (game-cols *game*))
    (dotimes (row (game-rows *game*))
      (setf (aref *hex-nodes* col row) (make-hex-node col row)))))

(defun make-hex-node (col row)
  (let* ((terrain (terrain-at *game* (hex col row)))
         (node (polygon (hexagon-points (- *radius* 0.8d0)) (terrain-fill terrain row)
                        (rgb 0.08 0.11 0.09) 1.5d0)))
    (decorate-hex node terrain)
    (setf (aref *hex-terrain* col row) terrain)
    (add *world* (z (at node (hex-x col) (hex-y col row)) 1))))

(defun sync-map ()
  (dotimes (col (game-cols *game*))
    (dotimes (row (game-rows *game*))
      (let* ((node (aref *hex-nodes* col row))
             (terrain (terrain-at *game* (hex col row))))
        (if (eq (aref *hex-terrain* col row) terrain)
            (objc:invoke node "setFillColor:" (terrain-fill terrain row))
            (progn (remove-node node)
                   (setf (aref *hex-nodes* col row) (make-hex-node col row))))))))

;;; ------------------------------------------------------------------
;;; pieces

(defun defender-fill () (rgb 0.30 0.52 0.78))
(defun defender-dark () (rgb 0.10 0.20 0.34))
(defun defender-light () (rgb 0.82 0.90 1.0))

(defun pips (node count total y)
  (dotimes (i total)
    (add node (at (circle 2.6d0 (if (< i count) (rgb 0.55 1.0 0.55) (rgb 0.3 0.1 0.1))
                          (rgb 0 0 0 0.6) 1)
                  (* 7 (- i (/ (1- total) 2))) y))))

(defun draw-kind (node kind hp)
  (let ((light (defender-light)) (dark (defender-dark)))
    (ecase kind
      (:lancer
       (add node (polygon '((-14 -8) (14 -8) (12 4) (-12 4)) light dark))
       (add node (polygon '((-6 4) (6 4) (5 10) (-5 10)) light dark))
       (add node (line 4 7 17 7 light 3)))
      (:skimmer
       (add node (polygon '((0 13) (13 -8) (0 -3) (-13 -8)) light dark)))
      (:mortar
       (add node (circle 7 light dark))
       (add node (line 0 0 9 12 light 4)))
      (:infantry
       (loop for i below hp
             for x in '(-8 0 8)
             do (add node (at (circle 4 light dark) x 4))
                (add node (polygon `((,(- x 4) -8) (,(+ x 4) -8) (,(+ x 3) -1) (,(- x 3) -1))
                                   light dark 1))))
      (:bunker
       (add node (polygon '((-15 -8) (15 -8) (11 4) (5 9) (-5 9) (-11 4)) (rgb 0.85 0.85 0.75) dark))
       (add node (polygon '((-5 -1) (5 -1) (5 2) (-5 2)) dark))
       (add node (line 0 9 0 17 light 1.5d0))
       (add node (polygon '((0 17) (8 14.5) (0 12)) (rgb 0.95 0.75 0.25)))))))

(defun make-unit-node (unit)
  (let* ((kind (unit-kind unit))
         (node (polygon (hexagon-points (- *radius* 5))
                        (if (eq kind :bunker) (rgb 0.45 0.40 0.30) (defender-fill))
                        (rgb 0.05 0.08 0.12) 2)))
    (draw-kind node kind (unit-hp unit))
    (when (and (> (unit-max-hp unit) 1) (not (eq kind :infantry)))
      (pips node (unit-hp unit) (unit-max-hp unit) -16))
    node))

(defun unit-node (id) (car (gethash id *unit-nodes*)))

(defun place-unit-node (unit)
  (let ((old (gethash (unit-id unit) *unit-nodes*)))
    (when old (remove-node (car old))))
  (let ((node (make-unit-node unit)))
    (multiple-value-bind (x y) (hex-point (unit-hex unit))
      (add *world* (z (at node x y) 10)))
    (setf (gethash (unit-id unit) *unit-nodes*) (cons node (unit-hp unit)))
    node))

(defun make-behemoth-node ()
  (let ((node (polygon (hexagon-points (+ *radius* 3)) (rgb 0.50 0.10 0.08) (rgb 1 0.45 0.3) 2.5d0)))
    (add node (polygon (hexagon-points 17) (rgb 0.30 0.06 0.05) (rgb 0.10 0.02 0.02) 1.5d0))
    (add node (at (circle 7.5d0 (rgb 0.65 0.15 0.10) (rgb 0.10 0.02 0.02)) 0 0))
    (add node (line 0 0 0 -20 (rgb 0.80 0.25 0.15) 4))
    (loop for (x y) in '((-12 8) (12 8) (-12 -8) (12 -8))
          do (add node (at (circle 3.5d0 (rgb 0.95 0.5 0.3) (rgb 0.10 0.02 0.02) 1) x y)))
    (objc:invoke node "setName:" "behemoth")
    node))

(defun place-behemoth ()
  (multiple-value-bind (x y) (hex-point (behemoth-hex (game-behemoth *game*)))
    (at *behemoth-node* x y)))

(defun sync-units ()
  "Make every piece agree with the model."
  (let ((ids (mapcar #'unit-id (game-units *game*))))
    (loop for id in (loop for k being the hash-keys of *unit-nodes* collect k)
          unless (member id ids)
            do (remove-node (unit-node id)) (remhash id *unit-nodes*)))
  (dolist (unit (game-units *game*))
    (let ((entry (gethash (unit-id unit) *unit-nodes*)))
      (if (and entry (= (cdr entry) (unit-hp unit)))
          (multiple-value-bind (x y) (hex-point (unit-hex unit))
            (at (car entry) x y))
          (place-unit-node unit))))
  (place-behemoth))

(defun sync-all ()
  (sync-map)
  (sync-units)
  (refresh))

;;; ------------------------------------------------------------------
;;; the Behemoth's panel at the top

(defparameter *chip-y* 718d0)
(defparameter *drive-y* 752d0)
(defparameter *chip-width* 44d0)

(defun chip-x (index) (+ 195 (* (- index 3) (+ *chip-width* 5))))

(defun short-name (sys)
  (ecase (sys-kind sys) (:cannon "CAN") (:autocannon "AC") (:rocket "RKT")))

(defun build-panel ()
  (add *world* (at (text "BEHEMOTH" 15 (rgb 1 0.55 0.45) :font "AvenirNext-Heavy" :align :left)
                   14 784))
  (setf *drive-label* (add *world* (at (text "" 13 (rgb 1 1 1 0.85) :font "AvenirNext-DemiBold"
                                                                    :align :right)
                                       376 784)))
  (setf *record-label* (add *world* (at (text "" 11 (rgb 1 1 1 0.45) :font "AvenirNext-Medium")
                                        195 784)))
  ;; The drive: a bar of segments, one per point.
  (add *world* (at (text "DRIVE" 11 (rgb 1 1 1 0.6) :font "AvenirNext-DemiBold" :align :left)
                   14 *drive-y*))
  (setf *drive-segments*
        (loop for i below *max-drive*
              collect (add *world* (at (box 9 12 (rgb 1 1 1 0.2)) (+ 64 (* i 11.6d0)) *drive-y*))))
  (setf *drive-percent* (add *world* (z (at (text "" 13 (rgb 1 1 0.6) :font "AvenirNext-Heavy")
                                            204 *drive-y*)
                                        5)))
  ;; The weapons, as chips.
  (setf *chips*
        (loop for sys in (behemoth-systems (game-behemoth *game*))
              for i from 0
              collect (let ((chip (add *world* (at (rounded *chip-width* 36 7 (rgb 0.3 0.1 0.08))
                                                   (chip-x i) *chip-y*))))
                        (list (sys-id sys) chip
                              (add chip (at (text (short-name sys) 12 (rgb 1 1 1)) 0 6))
                              (add chip (at (text "" 11 (rgb 1 1 0.6) :font "AvenirNext-Heavy")
                                            0 -9)))))))

(defun selected-unit ()
  (and *selected* (find-unit *game* *selected*)))

(defun can-fire-p (unit)
  (and unit (eq (game-phase *game*) :player) (not *busy*)
       (not (unit-fired unit)) (in-range-p *game* unit)))

(defun percent (chance) (format nil "~d%" (round (* 100 chance))))

(defun refresh-panel ()
  (let* ((b (game-behemoth *game*))
         (drive (behemoth-drive b))
         (shooter (let ((u (selected-unit))) (and (can-fire-p u) u))))
    (set-text *drive-label* (format nil "speed ~d" (behemoth-speed b)))
    (set-text *record-label* (format nil "won ~d  lost ~d" (saved "wins") (saved "losses")))
    (loop for segment in *drive-segments*
          for i from 0
          do (objc:invoke segment "setColor:"
                          (cond ((>= i drive) (rgb 1 1 1 0.12))
                                ((> drive (* 2/3 *max-drive*)) (rgb 0.95 0.45 0.25))
                                ((> drive (* 1/3 *max-drive*)) (rgb 0.95 0.70 0.25))
                                (t (rgb 0.95 0.90 0.35)))))
    (set-text *drive-percent* (if (and shooter (plusp drive))
                                  (percent (target-chance *game* shooter :drive))
                                  ""))
    (loop for (id chip label pct) in *chips*
          for sys = (find-system b id)
          do (objc:invoke chip "setFillColor:"
                          (cond ((not (sys-alive sys)) (rgb 0.15 0.15 0.15))
                                (shooter (rgb 0.65 0.20 0.12))
                                (t (rgb 0.35 0.10 0.08))))
             (objc:invoke chip "setStrokeColor:"
                          (if (and shooter (sys-alive sys)) (rgb 1 0.9 0.5) (rgb 0 0 0 0)))
             (objc:invoke label "setFontColor:" (if (sys-alive sys) (rgb 1 1 1) (rgb 1 1 1 0.25)))
             (set-text pct (cond ((not (sys-alive sys)) "×")
                                 (shooter (percent (target-chance *game* shooter id)))
                                 (t ""))))))

;;; ------------------------------------------------------------------
;;; the footer

(defparameter *button-y* 26d0)
(defparameter *action-x* 300d0)

(defun build-footer ()
  (setf *status-label* (add *world* (at (text "" 14 (rgb 1 1 1) :font "AvenirNext-DemiBold" :align :left)
                                        12 94)))
  (setf *log-labels*
        (list (add *world* (at (text "" 12 (rgb 1 1 1 0.75) :font "AvenirNext-Medium" :align :left) 12 74))
              (add *world* (at (text "" 12 (rgb 1 1 1 0.45) :font "AvenirNext-Medium" :align :left) 12 57))))
  (let ((new (add *world* (at (rounded 64 36 10 (rgb 1 1 1 0.1) (rgb 1 1 1 0.3) 1.5d0) 44 *button-y*))))
    (add new (text "New" 15 (rgb 1 1 1) :font "AvenirNext-DemiBold")))
  (let ((help (add *world* (at (rounded 44 36 10 (rgb 1 1 1 0.1) (rgb 1 1 1 0.3) 1.5d0) 108 *button-y*))))
    (add help (text "?" 18 (rgb 1 1 1))))
  (setf *action-button* (add *world* (at (rounded 170 40 12 (rgb 0.8 0.3 0.2) (rgb 1 0.7 0.5) 2)
                                         *action-x* *button-y*)))
  (setf *action-label* (add *action-button* (text "" 16 (rgb 1 1 1) :font "AvenirNext-Heavy"))))

(defun unit-status (unit)
  (format nil "~a~@[ (~d/~d)~]  attack ~d · range ~d · defense ~d~a"
          (unit-name unit)
          (and (> (unit-max-hp unit) 1) (unit-hp unit))
          (unit-max-hp unit)
          (unit-attack unit) (unit-range unit) (unit-defense *game* unit)
          (cond ((eq (game-phase *game*) :deploy) "")
                ((unit-fired unit) "  · fired")
                ((in-range-p *game* unit) "  · in range")
                (t ""))))

(defun refresh-footer ()
  (let ((unit (selected-unit)))
    (set-text *status-label*
              (cond ((and unit (combat-unit-p unit)) (unit-status unit))
                    (unit "Command bunker: protect it at all costs")
                    ((eq (game-phase *game*) :deploy) "Tap a defender, then a hex, to redeploy")
                    ((eq (game-phase *game*) :player) (format nil "Turn ~d: move and fire" (game-turn *game*)))
                    ((eq (game-phase *game*) :behemoth) "The Behemoth advances…")
                    (t ""))))
  (loop for label in *log-labels*
        for line in (append (game-log *game*) '("" ""))
        do (set-text label line))
  (set-text *action-label* (case (game-phase *game*)
                             (:deploy "START BATTLE")
                             (t "END TURN")))
  (objc:invoke *action-button* "setAlpha:"
               (if (and (member (game-phase *game*) '(:deploy :player)) (not *busy*)) 1d0 0.35d0)))

;;; ------------------------------------------------------------------
;;; highlights

(defun mark-hex (hex fill stroke &optional (line-width 2))
  (multiple-value-bind (x y) (hex-point hex)
    (add *marks* (at (polygon (hexagon-points (- *radius* 2.5d0)) fill stroke line-width) x y))))

(defun refresh-marks ()
  (objc:invoke *marks* "removeAllChildren")
  (let ((unit (selected-unit)))
    (when unit
      (mark-hex (unit-hex unit) nil (rgb 1 0.9 0.3) 3)
      (case (game-phase *game*)
        (:deploy
         (when (combat-unit-p unit)
           (dotimes (col (game-cols *game*))
             (dotimes (row (game-rows *game*))
               (let ((h (hex col row)))
                 (when (and (deploy-hex-p *game* h) (null (unit-at *game* h)))
                   (mark-hex h (rgb 0.5 0.8 1 0.18) nil)))))))
        (:player
         (unless *busy*
           (when (and (combat-unit-p unit) (plusp (unit-move-left unit)))
             (dolist (h (reachable-hexes *game* unit))
               (mark-hex h (rgb 0.5 0.8 1 0.25) (rgb 0.6 0.85 1 0.5) 1)))
           (when (can-fire-p unit)
             (mark-hex (behemoth-hex (game-behemoth *game*)) nil (rgb 1 0.9 0.5) 3))))))))

(defun refresh ()
  (refresh-panel)
  (refresh-footer)
  (refresh-marks))

;;; ------------------------------------------------------------------
;;; effects

(defun node-position (node)
  (let ((p (objc:invoke node "position"))) (values (aref p 0) (aref p 1))))

(defun attacker-position (attacker)
  (if (consp attacker)
      (node-position *behemoth-node*)
      (let ((node (unit-node attacker)))
        (if node (node-position node) (node-position *behemoth-node*)))))

(defun target-position (target)
  (cond ((eq target :drive) (node-position *behemoth-node*))
        ((consp target)
         (multiple-value-bind (x y) (node-position *behemoth-node*)
           (values (+ x (- (random 16) 8)) (+ y (- (random 16) 8)))))
        (t (let ((node (unit-node target)))
             (if node (node-position node) (values 195 400))))))

(defun float-text (string x y colour)
  (let ((label (add *world* (z (at (text string 15 colour :font "AvenirNext-Heavy") x (+ y 18)) 60))))
    (run label (action "group:" (vector (action "moveByX:y:duration:" 0d0 22d0 0.8d0)
                                        (action "sequence:" (vector (wait 0.45d0) (fade-out 0.35d0)))))
         (action "removeFromParent"))))

(defun blast (x y &optional (size 1d0))
  (let ((ring (add *world* (z (at (circle (* 10 size) (rgb 1 0.7 0.2 0.85) (rgb 1 0.95 0.6) 2) x y) 50))))
    (run ring (action "group:" (vector (action "scaleTo:duration:" 2.4d0 0.35d0) (fade-out 0.35d0)))
         (action "removeFromParent"))))

(defun tracer (x1 y1 x2 y2 colour)
  (let ((node (add *world* (z (line x1 y1 x2 y2 colour 3) 40))))
    (run node (wait 0.12d0) (fade-out 0.2d0) (action "removeFromParent"))))

(defun banner (string colour)
  (let ((label (add *world* (z (at (text string 30 colour :font "AvenirNext-Heavy") 195 400) 90))))
    (objc:invoke label "setScale:" 0.6d0)
    (run label (action "scaleTo:duration:" 1d0 0.18d0) (wait 0.5d0) (fade-out 0.3d0)
         (action "removeFromParent"))))

;;; ------------------------------------------------------------------
;;; playing events

(defun play-event (event)
  "Show EVENT; return how long to wait before the next."
  (ecase (first event)
    (:move
     (destructuring-bind (id path) (rest event)
       (let ((node (unit-node id)))
         (when node
           (apply #'run node (loop for h in path
                                   collect (multiple-value-bind (x y) (hex-point h)
                                             (move-to x y 0.09d0)))))
         (sound "move")
         (* 0.09d0 (length path)))))
    (:behemoth-move
     (let ((path (second event)))
       (apply #'run *behemoth-node*
              (loop for h in path
                    collect (multiple-value-bind (x y) (hex-point h) (move-to x y 0.32d0))))
       (sound "rumble")
       (haptic 1)
       (+ 0.1d0 (* 0.32d0 (length path)))))
    (:flatten
     (sync-map)
     0d0)
    (:crush
     (destructuring-bind (id damaged destroyed shove) (rest event)
       (let ((node (unit-node id)))
         (when node
           (multiple-value-bind (x y) (node-position node)
             (cond (destroyed
                    (blast x y 1.3d0)
                    (float-text "CRUSHED" x y (rgb 1 0.5 0.4))
                    (sound "boom")
                    (remove-node node)
                    (remhash id *unit-nodes*))
                   (t
                    (float-text (if damaged "RAMMED" "DODGED") x y (rgb 1 0.8 0.5))
                    (when shove
                      (multiple-value-bind (sx sy) (hex-point shove)
                        (run node (move-to sx sy 0.15d0)))))))))
       (haptic 2)
       0.45d0))
    (:fire
     (destructuring-bind (attacker target chance hit destroyed) (rest event)
       (declare (ignore chance))
       (multiple-value-bind (x1 y1) (attacker-position attacker)
         (multiple-value-bind (x2 y2) (target-position target)
           (let ((ours (not (consp attacker))))
             (tracer x1 y1 x2 y2 (if ours (rgb 0.6 0.9 1) (rgb 1 0.5 0.3)))
             (cond (destroyed
                    (blast x2 y2 (if ours 1d0 1.2d0))
                    (float-text (if (eq target :drive) "DRIVE WRECKED" "DESTROYED") x2 y2 (rgb 1 0.9 0.4))
                    (sound "boom")
                    (haptic 2)
                    (when (and (not ours) (unit-node target))
                      (remove-node (unit-node target))
                      (remhash target *unit-nodes*)))
                   (hit
                    (blast x2 y2 0.6d0)
                    (float-text "HIT" x2 y2 (rgb 1 0.85 0.5))
                    (sound "hit")
                    (haptic 0))
                   (t
                    (float-text "MISS" x2 y2 (rgb 1 1 1 0.7))
                    (sound "shot")))
             (unless ours (sync-units))
             (refresh-panel)
             (if ours 0.35d0 0.55d0))))))
    (:turn
     (banner (format nil "TURN ~d" (second event)) (rgb 0.7 0.9 1))
     0.6d0)
    (:outcome
     0d0)))

(defun play-events (events &optional then)
  (cond ((null events)
         (setf *busy* nil)
         (sync-all)
         (when (member (game-phase *game*) '(:won :lost))
           (finish-battle))
         (when then (funcall then)))
        (t
         (setf *busy* t)
         (let ((delay (play-event (first events))))
           (if (plusp delay)
               (after delay (lambda () (play-events (rest events) then)))
               (play-events (rest events) then))))))

;;; ------------------------------------------------------------------
;;; the end of a battle

(defun finish-battle ()
  (let ((won (eq (game-phase *game*) :won)))
    (save (if won "wins" "losses") (1+ (saved (if won "wins" "losses"))))
    (sound (if won "won" "lost"))
    (setf *selected* nil)
    (refresh)
    (let ((panel (z (at (rounded 320 170 18 (rgb 0.06 0.07 0.08 0.94)
                                 (if won (rgb 0.5 1 0.6) (rgb 1 0.5 0.4)) 3)
                        195 420)
                    100)))
      (add panel (at (text (if won "VICTORY" "DEFEAT") 34 (if won (rgb 0.6 1 0.65) (rgb 1 0.55 0.45))
                           :font "AvenirNext-Heavy")
                     0 46))
      (add panel (at (text (game-outcome *game*) 15 (rgb 1 1 1 0.9) :font "AvenirNext-DemiBold") 0 6))
      (add panel (at (text (format nil "~d turn~:p" (game-turn *game*) (game-turn *game*)) 13
                           (rgb 1 1 1 0.6) :font "AvenirNext-Medium")
                     0 -20))
      (add panel (at (text "Tap for a new battle" 14 (rgb 1 1 1 0.75) :font "AvenirNext-Medium") 0 -55))
      (objc:invoke panel "setScale:" 0.6d0)
      (run panel (action "scaleTo:duration:" 1d0 0.2d0))
      (setf *overlay* (add *world* panel)))))

;;; ------------------------------------------------------------------
;;; help

(defparameter *help-lines*
  '(("How to play" 20 "AvenirNext-Heavy")
    ("The Behemoth is rolling for your bunker." 13)
    ("Stop it before it gets there." 13)
    ("" 6)
    ("Tap a defender to select it. Blue hexes" 13)
    ("show where it can move. If it is in range" 13)
    ("of the Behemoth, tap the DRIVE bar or a" 13)
    ("weapon at the top to fire at it." 13)
    ("" 6)
    ("Hitting the drive slows it, then stops it." 13)
    ("You win when it cannot move and no weapon" 13)
    ("it has left can reach the bunker." 13)
    ("" 6)
    ("It crushes what it rolls over. If it reaches" 13)
    ("the bunker, you lose. Rubble is cover." 13)
    ("Ridges block all but infantry." 13)
    ("" 6)
    ("Lancer  heavy tank     Skimmer  fast, can fire then move" 11)
    ("Mortar  long range      Infantry  attack = squads left" 11)
    ("" 8)
    ("Tap to close" 13 "AvenirNext-DemiBold")))

(defun show-help ()
  (let ((panel (z (objc:invoke "SKNode" "node") 200)))
    (add panel (at (box 1200 2400 (rgb 0 0 0 0.5)) 195 400))
    (add panel (at (rounded 360 470 18 (rgb 0.06 0.07 0.08 0.96) (rgb 1 1 1 0.4) 2) 195 410))
    (loop with y = 615
          for (line size font) in *help-lines*
          do (add panel (at (text line size (rgb 1 1 1 0.9) :font (or font "AvenirNext-Medium")) 195 y))
             (decf y (+ size 9)))
    (setf *help* (add *world* panel))))

;;; ------------------------------------------------------------------
;;; taps

(defun within (x y cx cy half-width half-height)
  (and (<= (abs (- x cx)) half-width) (<= (abs (- y cy)) half-height)))

(defun select (id)
  (setf *selected* id)
  (refresh))

(defun try-fire (target)
  (let ((unit (selected-unit)))
    (when (can-fire-p unit)
      (let ((events (fire-at *game* (unit-id unit) target)))
        (when events
          (play-events events))))))

(defun tap-target (x y)
  "A tap on the Behemoth's panel: the drive bar or a weapon chip."
  (cond ((within x y 202 *drive-y* 150 14) (try-fire :drive) t)
        (t (loop for (id) in *chips*
                 for i from 0
                 when (within x y (chip-x i) *chip-y* (/ *chip-width* 2) 20)
                   do (try-fire id) (return t)))))

(defun tap-action ()
  (case (game-phase *game*)
    (:deploy
     (setf *selected* nil)
     (haptic 1)
     (sync-map)
     (play-events (begin-battle *game*)))
    (:player
     (setf *selected* nil)
     (haptic 1)
     (refresh)
     (play-events (end-turn *game*)))))

(defun tap-map (hex)
  (let ((unit (unit-at *game* hex))
        (chosen (selected-unit)))
    (cond ((and unit (not (eq unit chosen)))
           (haptic 0)
           (select (unit-id unit)))
          ((and chosen (null unit) (eq (game-phase *game*) :deploy))
           (if (redeploy-unit *game* (unit-id chosen) hex)
               (progn (sync-units) (refresh) (sound "move"))
               (select nil)))
          ((and chosen (null unit) (eq (game-phase *game*) :player)
                (not (equal hex (behemoth-hex (game-behemoth *game*)))))
           (let ((event (move-unit *game* (unit-id chosen) hex)))
             (if event (play-events (list event)) (select nil))))
          ((and chosen (equal hex (behemoth-hex (game-behemoth *game*))))
           ;; Tapping the Behemoth itself fires at its drive.
           (try-fire :drive))
          (t (select nil)))))

(defun tap-at (x y)
  (cond ((null *game*))
        (*help* (remove-node *help*) (setf *help* nil))
        ((within x y 44 *button-y* 34 20) (haptic 0) (new-battle))
        ((within x y 108 *button-y* 24 20) (show-help))
        (*overlay* (new-battle))
        (*busy*)
        ((within x y *action-x* *button-y* 88 22) (tap-action))
        ((> y (+ *map-top* 4)) (tap-target x y))
        (t (let ((hex (hex-at-point x y)))
             (if hex (tap-map hex) (select nil))))))

(defun tap-view-point (in-view)
  (let ((in-world (objc:invoke *world* "convertPoint:fromNode:"
                               (objc:invoke *scene* "convertPointFromView:" in-view) *scene*)))
    (tap-at (aref in-world 0) (aref in-world 1))))

(defun tapped (recognizer)
  (handler-case (tap-view-point (objc:invoke recognizer "locationInView:" *view*))
    (error (condition)
      (format *error-output* "~&behemoth: tap failed: ~a~%" condition)
      (finish-output *error-output*))))

;;; ------------------------------------------------------------------
;;; a battle

(defun new-battle (&optional seed)
  ;; Nodes are kept alive only by *world*, so forget them while it holds them.
  (clrhash *unit-nodes*)
  (setf *chips* nil *drive-segments* nil *hex-nodes* nil)
  (when *world* (remove-node *world*))
  (setf *game* (new-game :seed (or seed (get-universal-time)))
        *world* (add *scene* (objc:invoke "SKNode" "node"))
        *overlay* nil *help* nil *selected* nil *busy* nil)
  (fit-world t)
  (add *world* (z (at (box 1200 2400 (rgb 0.09 0.11 0.10)) 195 400) -10))
  (build-map)
  (setf *marks* (add *world* (z (objc:invoke "SKNode" "node") 5)))
  (dolist (unit (game-units *game*)) (place-unit-node unit))
  (setf *behemoth-node* (add *world* (z (make-behemoth-node) 20)))
  (place-behemoth)
  (build-panel)
  (build-footer)
  (refresh)
  (format t "BEHEMOTH: new battle, seed ~d~%" (game-seed *game*))
  (finish-output))

;;; ------------------------------------------------------------------
;;; the scene

(objc:define-objc-class behemoth-scene () ()
  (:objc-class-name "LispBehemothScene")
  (:objc-superclass-name "SKScene"))

(defun fit-world (&optional force)
  (when (and *scene* *world*)
    (let* ((size (objc:invoke *scene* "size"))
           (insets (objc:invoke *view* "safeAreaInsets"))
           (left (aref insets 1))
           (bottom (aref insets 2))
           (w (- (aref size 0) left (aref insets 3)))
           (h (- (aref size 1) (aref insets 0) bottom)))
      (when (and (plusp w) (plusp h)
                 (or force (not (equalp (list size insets) *fitted*))))
        (setf *fitted* (list size insets))
        (let ((scale (min (/ w *scene-width*) (/ h *scene-height*))))
          (objc:invoke *world* "setScale:" scale)
          (objc:invoke *world* "setPosition:"
                       (point (+ left (/ (- w (* scale *scene-width*)) 2))
                              (+ bottom (/ (- h (* scale *scene-height*)) 2)))))))))

(objc:define-objc-method ("update:" :void)
    ((self behemoth-scene) (time :double))
  (declare (ignore self time))
  (fit-world))

;;; ------------------------------------------------------------------
;;; autoplay, for watching it play itself:
;;;
;;;   SIMCTL_CHILD_BEHEMOTH_AUTOPLAY=1 xcrun simctl launch booted com.burnsidemk.behemoth

(defun autotap (x y)
  "Tap the screen where the design-space point (X, Y) is, as a finger would."
  (tap-view-point (objc:invoke *scene* "convertPointToView:"
                               (objc:invoke *world* "convertPoint:toNode:" (point x y) *scene*))))

(defvar *overlay-ticks* 0)

(defun autoplay-tick ()
  (cond ((or (null *game*) *busy* *help*))
        (*overlay*
         (when (>= (incf *overlay-ticks*) 5)
           (setf *overlay-ticks* 0)
           (new-battle)))
        ((eq (game-phase *game*) :deploy) (autotap *action-x* *button-y*))
        ((eq (game-phase *game*) :player)
         (let ((events (auto-step *game*)))
           (if events
               (play-events events)
               (autotap *action-x* *button-y*))))))

(defun start ()
  (objc:ensure-objc-initialized)
  (let* ((root (ui:root-view))
         (view (ui:new "SKView"))
         (background (rgb 0.09 0.11 0.10)))
    (objc:invoke root "setBackgroundColor:" background)
    (objc:invoke root "addSubview:" view)
    (ui:pin view "topAnchor" root "topAnchor")
    (ui:pin view "bottomAnchor" root "bottomAnchor")
    (ui:pin view "leadingAnchor" root "leadingAnchor")
    (ui:pin view "trailingAnchor" root "trailingAnchor")
    (let ((scene (objc:invoke (make-instance 'behemoth-scene) "initWithSize:"
                              (point *scene-width* *scene-height*))))
      (objc:invoke scene "setBackgroundColor:" background)
      (objc:invoke scene "setScaleMode:" 3)
      (setf *view* (ui:keep view)
            *scene* (ui:keep scene))
      (objc:invoke (objc:invoke "AVAudioSession" "sharedInstance") "setCategory:error:"
                   "AVAudioSessionCategoryAmbient" nil)
      (new-battle (let ((n (ext:getenv "BEHEMOTH_SEED")))
                    (and n (parse-integer n :junk-allowed t))))
      (when (ext:getenv "BEHEMOTH_HELP") (show-help))
      (objc:invoke view "presentScene:" scene))
    (objc:invoke view "addGestureRecognizer:"
                 (ui:keep (objc:invoke (objc:invoke "UITapGestureRecognizer" "alloc")
                                       "initWithTarget:action:"
                                       (ui:action-target #'tapped) "fire:")))
    (when (ext:getenv "BEHEMOTH_AUTOPLAY")
      (ui:after-every 0.5 (lambda (timer) (declare (ignore timer)) (autoplay-tick))))
    (values)))
