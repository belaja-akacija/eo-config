;;;; My personal configuration file library
;;;; BY: Eliza Oselskyi, 2025

(in-package :eo-config)

(defclass config-set ()
  ((name
     :initarg :name
     :accessor name)
  (file-location
    :initarg :file-location
    :accessor file-location)
   (allowed-names
     :initarg :allowed-names
     :accessor allowed-names
     :initform nil)
   (parameters
     :initarg :parameters
     :accessor parameters
     :initform '())
   (parameter-rules
     :initarg :parameter-rules
     :accessor parameter-rules
     :initform '())))

;(defconstant +config-default-class+ (make-instance 'config-set :name 'default))

(defgeneric define-allowed-names (obj &rest sym)
  (:documentation "Assigns allowed names to the set")
  (:method (obj &rest sym)
    (declare (ignorable sym))
    (format t "This class cannot be used"))
  (:method ((obj config-set) &rest sym)
    (setf (allowed-names obj) sym)))

(defgeneric read-in-config (obj file)
  (:documentation "Read in the parameter values from a file into set")
  (:method (obj file)
    (warn "Object does not have the required slots for this operation."))
  (:method ((obj config-set) file)
    (let ((ids (every-other (load-config file)))
          (vals (load-config file))
          (plist '()))
      (mapcar #'(lambda (indicator)
                  (setf (getf plist indicator) (getf vals indicator))) ids)
      (setf (parameters obj) plist)))
  (:method (obj file)
    (let ((vals (filter-config (load-config file) (allowed-names obj)))
          (plist '()))
      (mapcar #'(lambda (indicator body)
                  (setf (getf plist indicator) body)) (allowed-names obj) vals)
      (setf (parameters obj) plist)))
  (:method ((obj config-set) (file (eql 'file-location)))
    (if (slot-boundp obj file)
        (let ((vals (filter-config (load-config (file-location obj)) (allowed-names obj)))
              (plist '()))
          (mapcar #'(lambda (indicator body)
                      (setf (getf plist indicator) body)) (allowed-names obj) vals)
          (setf (parameters obj) plist))
        (format t "~A does not have a file specified." obj))))

(defgeneric get-config-param (obj id)
  (:documentation "Gets a configuration parameter from object")
  (:method ((obj config-set) id)
    (getf (parameters obj) id)))

(defgeneric check-valid-config-set-p (obj)
  (:documentation "Checks to make sure the object has all the right slots for a configuration set")
  (:method (obj)
    ;; check slots
    (and (slot-boundp obj 'file-location) (slot-boundp obj 'allowed-names) (slot-boundp obj 'parameters))))

(defgeneric add-parameter-rule (obj indicator body)
  (:documentation "Adds a parameter rule to the set")
  (:method ((obj config-set) indicator body)
    (let ((rule (getf (parameter-rules obj) indicator)))
      (cond ((and rule (equal rule body))
             (format t "Nothing to do. Rule ~A :: '~S' Already exists.~%" indicator body))
            ((or rule t)
             (if rule
                 (format t "Overriding previous rule: ~A :: '~S' -> '~S'~%" indicator rule body)
                 (format t "Creating new rule: ~A :: '~S'~%" indicator body))
             (setf (getf (parameter-rules obj) indicator) body))))))

(defmacro set-rules (obj &rest rules)
  `(mapcar #'(lambda (rule-set)
               (add-parameter-rule ,obj (first rule-set) (getf rule-set (first rule-set)))) (list ,@rules)))

(defgeneric test-rule (indicator obj)
  (:documentation "Tests a given indicator's rule-set against the actual value in the parameter")
  (:method (indicator (obj config-set))
    (let ((val (getf (parameters obj) indicator))
          (rule (eval (getf (parameter-rules obj) indicator))))
      (assert (funcall rule val) (val) "Parameter ~S does not satisfy rule: ~S" val rule))))

(defmacro with-parameters (obj (&rest indicators) rule)
  "Sets the same rule for multiple indicators"
  `(dolist (x ',indicators)
     (add-parameter-rule ,obj x ,rule)))
