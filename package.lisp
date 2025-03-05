(uiop/package:define-package :eo-config/package (:use :common-lisp)
  (:use-reexport :common-lisp))

(uiop/package:define-package :eo-config/utils
  (:use :eo-config/package)
  (:export
   #:truep
   #:valid-symbolp
   #:flush-global
   #:filter-config
   #:globalize-symbol
   #:check-config-mismatch
   #:def-special-global
   #:unintern-symbol-list
   #:flush-globals
   #:load-config
   ))

(uiop/package:define-package :eo-config (:use :eo-config/package)
  (:use :eo-config/package :eo-config/utils)
  (:export
   ;; CLASSES
   #:config-set
   #:name
   #:allowed-names
   #:parameters
   #:parameter-rules
   #:+config-default-class+
   ;; METHODS
   #:define-allowed-names
   #:read-in-config
   #:add-parameter-rule
   ;; FUNCTIONS AND MACROS
   #:with-parameters))

; (uiop/package:define-package :eo-config/testing
;   (:use :eo-config/package :eo-config/utils)
;   (:export
;    ;; CLASSES
;    #:config-set
;    #:name
;    #:allowed-names
;    #:parameters
;    #:parameter-rules
;    #:+config-default-class+
;
;    ;; METHODS
;    #:define-allowed-names
;    #:read-in-config
;    #:add-parameter-rule
;    #:test-rule
;
;    ;; FUNCTIONS AND MACROS
;    #:with-parameters
;    ))


;; (uiop/configuration::compute-user-cache) finds where the fasl files are
