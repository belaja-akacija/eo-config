;;;; My personal configuration file library
;;;; BY: Eliza Oselskyi, 2025

;;; TODO:
;;; - Create a way to save state (allow a program to be persistent by defining
;;; a file to write parameters to)

(in-package :eo-config)

(declaim (special *config-globals-list*))
(declaim (special *config-parameter-rules*))

(defun flush-global (global-symbol)
  "Refreshes global symbol back to default (unbound). Returns no value."
  (if (boundp global-symbol)
      (makunbound global-symbol)
      nil)
  (proclaim (list 'special global-symbol)))

(defun load-config (file)
  (with-open-file (stream file :direction :input)
    (read stream t)))

(defun filter-config (file symbol-list)
  (remove-if #'null
             (mapcar #'(lambda (x)
                         (if (member x symbol-list)
                             (getf file x)
                             nil)) file)))

(defun globalize-symbol (symbol)
  "Builds a new symbol in the conventional global variable style. (*example-global-var*)"
  (make-symbol (concatenate 'string "*" (symbol-name symbol) "*")))

(defmacro define-allowed-names (&rest symbols)
  "Creates a global variable of allowed parameter names for the config file."
  (flush-global '*config-globals-list*)
  (let ((allowed-names (intern (symbol-name (globalize-symbol 'config-allowed-names)))))
    `(defparameter ,allowed-names (list ,@symbols))))

(defun make-config-globals (file symbol-list)
  (labels ((helper (sym val)
             "Makes a symbol into a special global variable and sets its value"
             (let ((x (set sym (intern (symbol-name sym)))))
               (proclaim `(special ,x))
               (set x val)
               x)))
    (let ((globals (mapcar #'globalize-symbol symbol-list))
          (vals (filter-config file symbol-list)))
      (setf *config-globals-list* (mapcar #'helper globals vals)))))


(defun define-parameter-rule (indicator body)
  "Creates/updates a definition in *config-parameter-rules*"
  (if (boundp '*config-parameter-rules*)
      nil
      (setf *config-parameter-rules* nil))
  (let ((rule (getf *config-parameter-rules* indicator)))
    (if rule
        (format t "Overriding previous rule: ~A :: '~S' -> '~S'~%" indicator rule body)
        (format t "Creating new rule: ~A :: '~S'~%" indicator body))
    (setf (getf *config-parameter-rules* indicator) body)
    nil))

;;; Example of macro "set-rules"
;;; (set-rules
;;;   '(author (lambda (x) (stringp x))
;;;   '(x (lambda (x) (functionp (eval x))))
;;;  )

;; TODO: Possibly have it check if there is a global variable set with the
;; same name as the indicator and have it give a warning of a mismatch.

(defmacro set-rules (&rest rules)
  `(mapcar #'(lambda (rule-set)
               (define-parameter-rule (first rule-set) (getf rule-set (first rule-set))))
           (list ,@rules)))

(defun test-rule (indicator rule-set)
  "Tests a given indicator's rule-set against the actual value in the global scope"
  ;; TODO: Error handeling of when a symbol is unbound/non-existent
  (let ((global (symbol-value (find-symbol (symbol-name (globalize-symbol indicator)))))
        (rule (eval (getf rule-set indicator))))
    (assert (funcall rule global) (global) "Parameter ~S does not satisfy rule: ~S" global (getf rule-set indicator))))


;(swank:find-definition-for-thing #'car)
