;;;; behemoth.asd -- the rules of Behemoth, in plain Lisp, and their tests.
;;;;
;;;; No Objective-C here: the rules load and test on any Lisp.  The iOS app
;;;; is behemoth-app.asd, which depends on this.

(defsystem "behemoth"
  :description "Rules for Behemoth: hex-map defence against one giant war machine."
  :version "1.0.0"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "hex")
               (:file "model")
               (:file "auto"))
  :in-order-to ((test-op (test-op "behemoth/tests"))))

(defsystem "behemoth/tests"
  :description "FiveAM tests for the Behemoth rules."
  :depends-on ("behemoth" "fiveam")
  :serial t
  :pathname "tests"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :behemoth-tests :behemoth/tests))
               (error "Behemoth tests failed."))))
