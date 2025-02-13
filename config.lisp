(in-package :eo-config)

(defun globalize-symbol (symbol)
  (make-symbol (concatenate 'string "*" (symbol-name symbol) "*")))


(defmacro define-allowed-names (&rest symbols)
  "Creates a global variable of allowed parameter names for the config file. Returns failed object in list."
  (let ((string-list `(mapcar #'(lambda (x)
                                  (symbol-name x))
                              (list ,@symbols)))
        (allowed-names (intern (symbol-name (globalize-symbol 'config-allowed-names)))))
    `(defparameter ,allowed-names ,string-list)))

(defun create-symbols-from-list (lst)
  (mapcar #'(lambda (sym)
              (let ((temp (cond
                            ((symbolp sym)
                             (symbol-name sym))
                            ((stringp sym)
                             sym)
                            (t nil))))
                (if (eql temp nil)
                    sym
                    (progn
                     (setq sym (intern temp))
                     (eval `(defparameter ,sym nil))))))
          lst))

(defun unintern-symbols (symbol-list)
  "Uninterns symbols from a list. Returns failed object in list."
  (mapcar #'(lambda (sym)
              (cond
                ((symbolp sym)
                 (unintern sym))
                ((stringp sym)
                 (unintern (find-symbol sym)))
                (t sym)))
          symbol-list))
