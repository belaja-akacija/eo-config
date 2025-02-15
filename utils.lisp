(defun truep (x)
  "Convinience function to test if a value is explicitly T"
  (eq x T))

(defun valid-symbolp (obj)
  "Tests if an object is either a symbol or string"
  (cond
    ((symbolp obj))
    ((stringp obj))
    (t nil)))

(defun flush-global (global-symbol)
  "Refreshes global symbol back to default (unbound). Returns no value."
  (if (boundp global-symbol)
      (makunbound global-symbol)
      nil)
  (proclaim (list 'special global-symbol)))

(defun filter-config (file symbol-list)
  (remove-if #'null
             (mapcar #'(lambda (x)
                         (if (member x symbol-list)
                             (getf file x)
                             nil)) file)))

(defun globalize-symbol (symbol)
  "Builds a new symbol in the conventional global variable style. (*example-global-var*)"
  (make-symbol (concatenate 'string "*" (symbol-name symbol) "*")))


; (defun intern-symbols-from-list (obj)
;   "Interns symbols from a list of strings or symbols, or a singular string or symbol. Returns a list of successful symbols that were interned."
;   (labels ((helper (sym)
;              (let ((temp (cond
;                            ((symbolp sym)
;                             (symbol-name sym))
;                            ((stringp sym)
;                             sym)
;                            (t nil))))
;                (if (eql temp nil)
;                    nil
;                    (progn (setf sym (intern temp))
;                           sym)))))
;     (if (listp obj)
;         (remove-if #'null (mapcar #'helper obj))
;         (if (valid-symbolp obj)
;             (list (helper obj))
;             nil))))
;
; (defun unintern-symbols-from-list (obj)
;   "Uninterns symbols from a list or single valid object. Returns a nil if successful and a list of objects that couldn't be uninterned, on failure."
;   (labels ((helper (sym)
;              (cond
;                ((symbolp sym)
;                 (unintern sym))
;                ((stringp sym)
;                 (unintern (find-symbol sym)))
;                (t sym))))
;     (if (listp obj)
;         (remove-if #'truep (mapcar #'helper obj))
;         (if (valid-symbolp obj)
;             (progn (helper obj) nil)
;             (list obj)))))
;
