;;;; System definition for TRIVIAL-DOCUMENTATION.

(defsystem trivial-documentation
  :description
  "Extract documentation and definitions for symbols and packages."
  :components ((:file "extract"))
  :depends-on ("closer-mop"))
