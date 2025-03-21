(in-package :eo-config/utils)

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

 (defun check-config-mismatch (file symbol-list)
   "Checks if number of parameters in config match number of parameters in global-symbol-list. Returns T on success."
   (= (/ (length file) 2)  (length symbol-list) ))

(defun def-special-global (sym val)
  "Makes a symbol into a special global variable and sets its value"
  (let ((x (set sym (intern (symbol-name sym)))))
    (proclaim `(special ,x))
    (set x val)
    x))

(defun unintern-symbol-list (lst)
  "Uninterns a list of symbols"
  (dolist  (x lst)
    (unintern x)))

(defun flush-globals (lst)
  "Flushes each symbol in a list"
  (dolist (y lst) (flush-global y)))

(defun load-config (file)
  (with-open-file (stream file :direction :input)
    (read stream t)))

(defun every-other (lst &optional (offset 'odd))
  "Gets every other element in a list. Setting offset to 'even starts on the second element of the list."
  (let ((condition (if (eql offset 'even) 0 1)))
    (labels ((parse-el (l e)
               (if (and (= (mod e 2) condition)
                        (< e (1+ (length lst))))
                   (car l)))
             (helper (l e)
               (if (null l)
                   '()
                   (cons (parse-el l e) (helper (cdr l) (1+ e))))))
      (remove-if #'null (helper lst 1)))))

(defun flatten (lst &aux (result '()))
  (labels ((rflatten (lst1)
             (dolist (el lst1 result)
               (if (listp el)
                   (rflatten el)
                   (push el result)))))
    (nreverse (rflatten lst))))

