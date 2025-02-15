(in-package :eo-config)

(declaim (special *config-globals-list*))

(defun flush-globals-list ()
  "Refreshes global config list back to default (unbound). Returns no value."
  (if (boundp '*config-globals-list*)
      (makunbound '*config-globals-list*)
      nil)
  (proclaim `(special *config-globals-list*)))

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
  (flush-globals-list)
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

;(swank:find-definition-for-thing #'car)
