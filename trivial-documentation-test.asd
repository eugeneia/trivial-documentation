#+asdf3 (in-package :asdf-user)

(defsystem trivial-documentation-test
  :description
  "Tests for trivial-documentation."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
  :components ((:file "test/test-package")
               (:file "test/test" :depends-on ("test/test-package")))
  :depends-on ("trivial-documentation")
  :perform (test-op (o s)
             #+asdf3
             (uiop:symbol-call :trivial-documentation.test :run-tests)))
