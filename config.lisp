(in-package :eo-config)

(defun globalize-symbol (symbol)
  (make-symbol (concatenate 'string "*" (symbol-name symbol) "*")))


(defmacro define-allowed-names (&rest symbols)
  "Creates a global variable of allowed parameter names for the config file. Returns a list; non-true/false items are objects that couldn't be uninterned."
  (let ((string-list `(mapcar #'(lambda (x)
                                  (symbol-name x))
                              (list ,@symbols)))
        (allowed-names (intern (symbol-name (globalize-symbol 'config-allowed-names)))))
    `(defparameter ,allowed-names ,string-list)))

;; TODO: INTERN AND UNINTERN functions: should they be private?
(defun intern-symbols-from-list (symbol-list)
  "Interns symbols from a list of strings or symbols. Returns a list; non-true/false items are objects that couldn't be uninterned."
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
                     ;(eval `(defparameter ,sym nil)) ;may want to do this elsewhere. Just intern the symbols.
                     t))))
          symbol-list))

(defun unintern-symbols-from-list (symbol-list)
  "Uninterns symbols from a list. Returns a list; non-true/false items are objects that couldn't be uninterned."
  (mapcar #'(lambda (sym)
              (cond
                ((symbolp sym)
                 (unintern sym))
                ((stringp sym)
                 (unintern (find-symbol sym)))
                (t sym)))
          symbol-list))

(defun make-config-globals (symbol-list)
  (unintern-symbols-from-list symbol-list) ; ensure all symbols are uninterned
  ()
  )


(with-open-file (f (merge-pathnames (uiop:getcwd) "configrc.lisp"))
  (let ((contents (make-string (file-length f))))
    (read-sequence contents f)
    contents))
