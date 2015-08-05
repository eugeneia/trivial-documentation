;;;; System definition for TRIVIAL-DOCUMENTATION.

(defsystem trivial-documentation
  :description
  "Extract documentation and definitions for symbols and packages."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "extract"))
  :depends-on ("closer-mop"))
