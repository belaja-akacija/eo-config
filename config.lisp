;;;; My personal configuration file library
;;;; BY: Eliza Oselskyi, 2025

;;; TODO:
;;; - Create a way to save state (allow a program to be persistent by defining
;;; a file to write parameters to)

;; TODO: allow optional defining of allowed names for different configuration
;; sets. Default should just be the normal global one as is now. If user
;; creates a new set, create a global var called
;; *config-globals-list-<set-name>*
;; Likewise, for setting parameter rules -->
;; *config-parameter-rules-<set-name>*
;; The desired format should be:
;; (define-allowed-names ((&rest symbols) &optional &key (set-name 'default)))

(in-package :eo-config)

(defclass config-set ()
  ((name
     :initarg :name
     :accessor name)
   (allowed-names
     :initarg :allowed-names
     :accessor allowed-names)
   (parameters
     :initarg :parameters
     :accessor parameters
     :initform '())
   (parameter-rules
     :initarg :parameter-rules
     :accessor parameter-rules
     :initform '())))

(defconstant +config-default-class+ (make-instance 'config-set :name 'default))

(defgeneric define-allowed-names (obj &rest sym)
  (:documentation "Assigns allowed names to the set")
  (:method (obj &rest sym)
    (declare (ignorable sym))
    (format t "This class cannot be used"))
  (:method ((obj config-set) &rest sym)
    (setf (allowed-names obj) sym)))

(defgeneric read-in-config (obj file)
  (:documentation "Read in the parameter values from a file into set")
  (:method ((obj config-set) file)
    (let ((vals (filter-config (load-config file) (allowed-names obj)))
          (plist '()))
      (mapcar #'(lambda (indicator body)
                  (setf (getf plist indicator) body)) (allowed-names obj) vals)
      (setf (parameters obj) plist))))

(defgeneric add-parameter-rule (obj indicator body)
  (:documentation "Adds a parameter rule to the set")
  (:method ((obj config-set) indicator body)
    (let ((rule (getf (parameter-rules obj) indicator)))
      (cond ((and rule (eql rule body))
             (format t "Nothing to do. Rule ~A :: '~S'~%Already exists." indicator body))
            ((or rule t)
             (if rule
                 (format t "Overriding previous rule: ~A :: '~S' -> '~S'~%" indicator rule body)
                 (format t "Creating new rule: ~A :: '~S'~%" indicator body))
             (setf (getf (parameter-rules obj) indicator) body))))))

(defgeneric test-rule (indicator obj)
  (:documentation "Tests a given indicator's rule-set against the actual value in the parameter")
  (:method (indicator (obj config-set))
    (let ((val (getf (parameters obj) indicator))
          (rule (eval (getf (parameter-rules obj) indicator))))
      (assert (funcall rule val) (val) "Parameter ~S does not satisfy rule: ~S" val rule))))

(defmacro with-parameters (place (&rest params) rule)
  "Sets the same rule for multiple indicators"
  `(dolist (x ',params)
     (add-parameter-rule ,place x ,rule)))
