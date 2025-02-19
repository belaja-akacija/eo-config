(defpackage :eo-config
  (:use :cl)
  ;; CONFIG
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
    #:*config-parameter-rules*
    ;; UTILS
    #:check-config-mismatch
    ))
