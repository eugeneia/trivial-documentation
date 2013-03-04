;;;; Package definitions for package-api.

(defpackage :package-api
  (:documentation
   "Extract API from a package.")
  (:use :cl)
  (:export :extract-api))
