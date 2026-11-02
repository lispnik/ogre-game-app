;;;; package.lisp -- the rules package; no Objective-C in it.

(defpackage #:behemoth
  (:use #:cl)
  (:export
   ;; random numbers
   #:make-rng #:rng-next #:rng-float #:rng-below
   ;; hexes
   #:hex #:hex-col #:hex-row #:hex-distance #:hex-neighbors #:on-map-p
   ;; unit kinds and systems
   #:kind-property #:system-property #:hit-chance #:drive-speed
   #:*max-drive* #:*drive-defense* #:*cols* #:*rows* #:*deploy-first-row*
   ;; units
   #:unit #:unit-id #:unit-kind #:unit-col #:unit-row #:unit-hp
   #:unit-move-left #:unit-fired #:unit-moved #:unit-hex #:unit-attack
   #:unit-defense #:unit-range #:unit-max-hp #:unit-name #:unit-glyph
   #:combat-unit-p
   ;; the behemoth
   #:behemoth #:behemoth-col #:behemoth-row #:behemoth-drive
   #:behemoth-systems #:behemoth-hex #:behemoth-speed
   #:sys #:sys-id #:sys-kind #:sys-alive #:sys-fired #:sys-name
   #:find-system #:live-systems
   ;; the game
   #:game #:new-game #:game-cols #:game-rows #:game-terrain #:game-units
   #:game-behemoth #:game-turn #:game-phase #:game-log #:game-outcome
   #:game-seed #:terrain-at #:unit-at #:find-unit #:bunker
   #:deploy-hex-p #:redeploy-unit #:begin-battle
   #:reachable #:reachable-hexes #:move-unit #:in-range-p
   #:target-chance #:target-defense #:fire-at #:end-turn
   #:behemoth-distance-to-bunker
   ;; a computer player, for autoplay and balance runs
   #:auto-step #:auto-play-turn))
