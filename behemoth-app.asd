;;;; behemoth-app.asd -- Behemoth for iOS, built by asdf-ios-app.
;;;;
;;;;   ecl --eval '(asdf:make "behemoth-app")' --eval '(quit)'
;;;;
;;;; The simulator by default; the device too when IOS_SIGNING_IDENTITY says
;;;; how to sign.  BEHEMOTH_DISTRIBUTION=1 makes an App Store build, for the
;;;; device alone, and BEHEMOTH_BUILD is its build number.

(defsystem "behemoth-app"
  :defsystem-depends-on ("asdf-ios-app")
  :class :ios-app-system
  :build-operation "ios-app-op"
  :entry-point "behemoth-app:start"
  :description "Hex-map defence against one giant automated war machine."
  :version #.(or (uiop:getenv "BEHEMOTH_BUILD") "1")
  :bundle-short-version "1.0.0"
  :serial t
  :depends-on ("behemoth" "objc/uikit")
  :pathname "app"
  :components ((:file "package")
               (:file "view"))

  :bundle-identifier "com.burnsidemk.behemoth"
  :bundle-name "Behemoth"
  :bundle-executable "behemoth"
  :bundle-platforms #.(cond ((uiop:getenv "BEHEMOTH_DISTRIBUTION") '(:device))
                            ((uiop:getenv "IOS_SIGNING_IDENTITY") '(:simulator :device))
                            (t '(:simulator)))
  :get-task-allow #.(not (uiop:getenv "BEHEMOTH_DISTRIBUTION"))
  ;; The board is laid out for portrait.
  :bundle-device-family (:iphone)
  :bundle-orientations (:portrait)
  :bundle-status-bar-hidden t
  :bundle-info-plist (("UIViewControllerBasedStatusBarAppearance" . :false)
                      ("ITSAppUsesNonExemptEncryption" . :false)
                      ("LSApplicationCategoryType" . "public.app-category.strategy-games")
                      ("GCSupportsGameMode" . :true))
  ;; tools/make-icon.py draws it; tools/make-sounds.py synthesises the sounds.
  :bundle-icon "res/Behemoth.xcassets"
  :bundle-resources ("res/sounds/move.wav" "res/sounds/rumble.wav" "res/sounds/shot.wav"
                     "res/sounds/hit.wav" "res/sounds/boom.wav"
                     "res/sounds/won.wav" "res/sounds/lost.wav")
  :bundle-frameworks ("UIKit" "Foundation" "CoreGraphics" "SpriteKit" "AVFoundation")

  :code-signing-identity #.(or (uiop:getenv "IOS_SIGNING_IDENTITY") :automatic)
  :development-team #.(uiop:getenv "IOS_DEVELOPMENT_TEAM")
  :provisioning-profile #.(uiop:getenv "IOS_PROVISIONING_PROFILE"))
