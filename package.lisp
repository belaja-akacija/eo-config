(uiop/package:define-package :eo-config/package (:use :common-lisp)
  (:use-reexport :common-lisp))
  ;; CONFIG
  ; (:export
  ;   #:define-allowed-names
  ;   #:load-config
  ;   #:make-config-globals
  ;   #:set-rules
  ;   #:test-rule
  ;   #:with-parameters
  ;   ; special globals
  ;   #:*config-allowed-names*
  ;   #:*config-globals-list*
  ;   #:*config-parameter-rules*
  ;   ))

(uiop/package:define-package :eo-config/utils
  (:use :eo-config/package)
  (:export
   #:truep
   #:valid-symbolp
   #:flush-global
   #:filter-config
   #:globalize-symbol
   #:check-config-mismatch
   ))

(uiop/package:define-package :eo-config (:use :eo-config/package)
  (:use :eo-config/package :eo-config/utils)
  (:export
     #:define-allowed-names
     #:load-config
     #:make-config-globals
     #:set-rules
     #:test-rule
     #:with-parameters
     ; special globals
     #:*config-allowed-names*
     #:*config-globals-list*
     #:*config-parameter-rules*))


;; (uiop/configuration::compute-user-cache) finds where the fasl files are
