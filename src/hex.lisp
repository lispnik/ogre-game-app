;;;; hex.lisp -- a seeded random stream, and flat-topped hexes.
;;;;
;;;; The board is flat-topped hexes in "odd-q" offset coordinates: columns
;;;; left to right, rows top to bottom, odd columns pushed half a hex down.
;;;; Distances and neighbours go through cube coordinates.

(in-package #:behemoth)

;;; ------------------------------------------------------------------
;;; random numbers
;;;
;;; Park-Miller, so a seed gives the same game under SBCL on the host and
;;; ECL on the phone.

(defstruct (rng (:constructor %make-rng (state)))
  (state 1 :type (integer 1 2147483646)))

(defun make-rng (seed)
  (%make-rng (1+ (mod (abs seed) 2147483646))))

(defun rng-next (rng)
  (setf (rng-state rng) (mod (* (rng-state rng) 48271) 2147483647)))

(defun rng-float (rng)
  "A float in [0, 1)."
  (/ (float (1- (rng-next rng)) 1d0) 2147483646d0))

(defun rng-below (rng n)
  (floor (* (rng-float rng) n)))

;;; ------------------------------------------------------------------
;;; hexes

(defparameter *cols* 9)
(defparameter *rows* 12)

(defun hex (col row) (cons col row))
(defun hex-col (hex) (car hex))
(defun hex-row (hex) (cdr hex))

(defun offset->cube (col row)
  (let ((z (- row (floor (- col (logand col 1)) 2))))
    (values col (- (- col) z) z)))

(defun cube->offset (x z)
  (hex x (+ z (floor (- x (logand x 1)) 2))))

(defun hex-distance (a b)
  (multiple-value-bind (ax ay az) (offset->cube (hex-col a) (hex-row a))
    (multiple-value-bind (bx by bz) (offset->cube (hex-col b) (hex-row b))
      (max (abs (- ax bx)) (abs (- ay by)) (abs (- az bz))))))

(defparameter *cube-directions*
  '((1 . -1) (1 . 0) (0 . 1) (-1 . 1) (-1 . 0) (0 . -1))
  "Each direction as (dx . dz); dy follows.")

(defun on-map-p (hex &optional (cols *cols*) (rows *rows*))
  (and (< -1 (hex-col hex) cols) (< -1 (hex-row hex) rows)))

(defun hex-neighbors (hex &optional (cols *cols*) (rows *rows*))
  "The neighbours of HEX that lie on the map."
  (multiple-value-bind (x y z) (offset->cube (hex-col hex) (hex-row hex))
    (declare (ignore y))
    (loop for (dx . dz) in *cube-directions*
          for n = (cube->offset (+ x dx) (+ z dz))
          when (on-map-p n cols rows) collect n)))

(defun shortest-costs (start passable cost &key (limit most-positive-fixnum))
  "Dijkstra from START.  PASSABLE says whether a hex may be entered, COST what
entering it costs.  Returns a hash table of hex -> (cost . previous hex),
holding only hexes within LIMIT."
  (let ((best (make-hash-table :test 'equal))
        (open (list (cons 0 start))))
    (setf (gethash start best) (cons 0 nil))
    (loop while open
          do (setf open (sort open #'< :key #'car))
             (destructuring-bind (spent . here) (pop open)
               (when (<= spent (car (gethash here best)))
                 (dolist (next (hex-neighbors here))
                   (when (funcall passable next)
                     (let ((total (+ spent (funcall cost next))))
                       (when (and (<= total limit)
                                  (let ((seen (gethash next best)))
                                    (or (null seen) (< total (car seen)))))
                         (setf (gethash next best) (cons total here))
                         (push (cons total next) open))))))))
    best))

(defun path-to (costs goal)
  "The hexes from the start of COSTS to GOAL, start excluded."
  (loop with path = '()
        for hex = goal then (cdr (gethash hex costs))
        while (and hex (cdr (gethash hex costs)))
        do (push hex path)
        finally (return path)))
