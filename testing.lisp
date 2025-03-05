(in-package :eo-config/testing)

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

(declaim (special *config-default-class*))
(setf *config-default-class* (make-instance 'config-set :name 'default))

(defgeneric define-allowed-names (obj &rest sym)
  (:documentation "Assigns allowed names to the set")
  (:method (obj &rest sym)
    (declare (ignorable sym))
    (format t "This class cannot be used"))
  (:method ((obj config-set) &rest sym)
    (setf (allowed-names obj) sym)))

; Should I even create global variables in the first place, when we have a class?
; (let* ((name-list (allowed-names obj)))
;       (mapcar #'(lambda (n)
;                   (def-special-global
;                     (globalize-symbol
;                       (make-symbol (concatenate 'string (symbol-name (name obj)) "-" (symbol-name n))))
;                     nil))
;               name-list))

(define-allowed-names *config-default-class* 'test)

(defgeneric read-in-config (obj file)
  (:documentation "Read in the parameter values from a file into set")
  (:method ((obj config-set) file)
    (let ((vals (filter-config (eo-config:load-config file) (allowed-names obj)))
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

(add-parameter-rule *config-default-class* 'test (lambda (x) (stringp x)))

(read-in-config *config-default-class* "test.config")

(test-rule 'test *config-default-class*)

(defun define-config-set (name)
  (let ((set-list (create-set-name-var-symbol (make-symbol (string-upcase name)))))
    (mapcar #'(lambda (s)
                (format t "~%Creating ~A" s)
                (def-special-global s nil)) set-list)))

(defun define-config-set-test (name)
  (let ((global-set-name (globalize-symbol (make-symbol (symbol-name name)))))
    (def-special-global global-set-name (make-instance 'config-set :name name))))

(defmacro define-allowed-names-test ((&rest symbols) &optional &key (set-name 'default))
  "Creates a global variable of allowed parameter names for the config file."
  (let ((global-set-sym (globalize-symbol set-name)))
    (find-symbol (symbol-name global-set-sym))))
;(flush-global '*config-globals-list*)
;(let ((allowed-names (intern (symbol-name (globalize-symbol 'config-allowed-names)))))
;  `(defparameter ,allowed-names (list ,@symbols))))

(defun create-set-name-var-symbol (sym)
  (let ((global-stubs '(config-globals-list- config-parameter-rules-)))
    (mapcar #'(lambda (x)
                (globalize-symbol (make-symbol (concatenate 'string (symbol-name x) (symbol-name sym))))) global-stubs)))
;(ql:quickload :eo-config)
