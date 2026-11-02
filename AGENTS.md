# Repository Guidelines

- `src/` holds the rules in package `BEHEMOTH`, with no Objective-C. Keep it that way, so the rules load and test on SBCL.
- `app/` holds the iOS view in package `BEHEMOTH-APP`, which is SpriteKit through `objc`/`objc/uikit`.
- Rules functions return *events* (documented at the top of `src/model.lisp`). The view plays them, then re-syncs from the model.
- Randomness goes through the game's own Park-Miller RNG (`rng-float`), never `cl:random`, so seeds reproduce across SBCL and ECL.
- Don't evaluate Objective-C at load time (for example a `defparameter` holding a `UIColor`). The app is cross-compiled on the Mac, where loading runs that code and fails. Use functions instead.
- `objc:invoke` returns NSStrings as foreign pointers. Keep any state you need to read back in Lisp.
- Test with `sbcl --non-interactive --eval '(asdf:test-system "behemoth")'`. Add a FiveAM test for every rule change. If you change numbers, check balance by running `auto-play-turn` over a few hundred seeds.
- Build with `ecl --eval '(asdf:make "behemoth-app")' --eval '(quit)'`, then check visible changes in the simulator (`BEHEMOTH_AUTOPLAY=1`).
