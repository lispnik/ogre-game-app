# Behemoth

A turn-based hex-map tactics game for iOS, written in Common Lisp and built
with [asdf-ios-app](https://github.com/lispnik/asdf-ios-app) and
[objc](https://github.com/lispnik/objc). Dependencies come from
[ocicl](https://github.com/ocicl/ocicl), and the rules are tested with
[FiveAM](https://github.com/lispci/fiveam). There is no Xcode project: ECL
cross-compiles the Lisp, and SpriteKit draws.

One enormous automated war machine, the **Behemoth**, rolls down the map
toward your command bunker. You command twelve defenders. Each shot picks one
target on the Behemoth: its **drive**, which sets how far it moves, or one of
its **weapons**. It crushes whatever it rolls over. If it reaches the bunker,
or shoots it to pieces, you lose. You win when its drive is wrecked and no
weapon it has left can reach the bunker.

## Rules

Every chance is `attack / (attack + defense)`.

| Defender | Attack | Range | Defense | Move | Hits | Notes |
|---|---|---|---|---|---|---|
| Lancer | 4 | 2 | 3 | 3 | 2 | heavy tank |
| Skimmer | 2 | 2 | 2 | 4 | 1 | hovers over rubble, may move after firing |
| Mortar | 3 | 4 | 1 | 1 | 1 | |
| Infantry | 1 per squad | 1 | 2 | 2 | 3 squads | climbs ridges |
| Bunker | – | – | 4 | – | 5 | lose it and you lose |

| Behemoth | Attack | Range | Damage | Defense | Count |
|---|---|---|---|---|---|
| Drive | – | – | – | 5 | 24 points; speed 3/2/1/0 by thirds |
| Main cannon | 5 | 3 | 2 | 6 | 1 |
| Autocannon | 3 | 2 | 1 | 3 | 4 |
| Rocket | 5 | 5 | 2 | 2 | 2, single use |

A hit on the drive takes off the attacker's attack strength. A hit on a weapon
destroys it. Rubble costs vehicles 2 to enter and gives +1 defense. Ridges
block everything but infantry. The Behemoth flattens rubble as it goes. When
it rolls over a unit, infantry always lose a squad and anything else is hit
half the time. Survivors are thrown aside.

The numbers were tuned so that the built-in naive computer player wins about
half its games. A player who deploys and focuses fire with some thought
should do better.

## Building and testing

```sh
ocicl install                                              # vendor objc, fiveam, ...
sbcl --non-interactive --eval '(asdf:test-system "behemoth")'
ecl --eval '(asdf:make "behemoth-app")' --eval '(quit)'    # => build/iphonesimulator/Behemoth.app
```

From a Lisp: `(asdf:make "behemoth-app")`, then
`(asdf-ios-app:run-in-simulator "behemoth-app")`.

For a device build, set `IOS_SIGNING_IDENTITY`, `IOS_DEVELOPMENT_TEAM` and
`IOS_PROVISIONING_PROFILE` before building. `BEHEMOTH_DISTRIBUTION=1` makes
an App Store build, and `BEHEMOTH_BUILD` sets its build number.

## TestFlight

Signing settings live in `local.env`, which is not committed. The headers of
`tools/testflight.sh` and `tools/asc.py` list what goes in it.

```sh
tools/asc.py setup              # once: App ID and App Store profile, through the API
tools/asc.py check              # is the App Store Connect app record there yet?
tools/testflight.sh --validate   # build, check signature, validate; upload nothing
tools/testflight.sh              # ...and upload; build number from the clock
```

The app record itself must be made once by hand at
<https://appstoreconnect.apple.com/apps> (New App, iOS, bundle ID
`com.burnsidemk.behemoth`), because the API cannot create it.

## Development switches

Set these in the simulator's environment at launch:

```sh
SIMCTL_CHILD_BEHEMOTH_AUTOPLAY=1 xcrun simctl launch booted com.burnsidemk.behemoth
```

- `BEHEMOTH_AUTOPLAY=1`: the computer plays the defenders, pressing the buttons through the real tap path.
- `BEHEMOTH_SEED=n`: a fixed map and dice.
- `BEHEMOTH_HELP=1`: opens the rules panel.

## Files

| | |
|---|---|
| `behemoth.asd` | the rules (`behemoth`) and their FiveAM tests (`behemoth/tests`) |
| `behemoth-app.asd` | the iOS app |
| `src/hex.lisp` | seeded RNG (the same on SBCL and ECL), hex geometry, Dijkstra |
| `src/model.lisp` | units, map generation, movement, fire, the Behemoth's AI, outcomes |
| `src/auto.lisp` | a simple computer player for autoplay and balance runs |
| `tests/tests.lisp` | FiveAM suite |
| `app/view.lisp` | the SpriteKit scene: vector hexes and pieces, event playback, taps |
| `tools/make-icon.py` | draws `res/Behemoth.xcassets` (slow: pure Python) |
| `tools/make-sounds.py` | synthesises `res/sounds/*.wav` |
| `tools/asc.py` | App ID, App Store profile and app-record check, via the App Store Connect API |
| `tools/testflight.sh` | App Store build, check, validate, upload |
