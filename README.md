# eo-config
Personal Configuration File Library

A simple library to manage configuration files of tools I write.


It reads in a file, assuming that the first line of the file is a plist.

First, you should create an instance of a configuration set, using the class `config-set`:
```lisp
(defconstant +default-config+ (make-instance 'config-set :name 'default :file-location "main.config"))
```

Then, you should define the allowed parameter names in your configuration file,
using the method `define-allowed-names`. This list gets stored in the slot `allowed-names`.


```lisp
(define-allowed-names +default-config+
    'foo-dir
    'bar-value
    'baz-toggle)
```
The general form of this is`(define-allowed-names object &rest symbols)`

If you haven't done so yet, you should assign the slot `'file-location` to
wherever the configuration file is for that particular set. Or, you may just
pass the location as the parameter for the method `read-in-config`. This method
binds the `parameters` slot with the IDs and values of the parameters. It first
filters the file contents, ensuring that only the parameters with the allowed
names are selected.

For the first case, you simply pass the symbol `'file-location` to the method,
to signal to it that it should read from the slot.

```lisp
(read-in-config +default-config+ "main.config")
;; -OR-
(read-in-config +default-config+ 'file-location)
```

Optionally, you may also set rules the configuration parameters must adhere to:

__STOPPED HERE__ FINISH THIS

```lisp
(set-rules
    '(foo-dir (lambda (x) (pathnamep x)))
    '(bar-value (lambda (x) (numberp x)))
    '(baz-toggle (lambda (x) (or (eq x T) (null x))))
    )
```

`set-rules` takes in a list, with the first value being the symbol you want to apply the rule on, the second value a form.
Currently, it only works if you encapsulate the test in an anonymous function.

It assigns the rule list to the special variable `*config-parameter-rules*`


Then, you can test your config against the rule list:

```lisp
(test-rule 'foo-dir *config-parameter-rules*)
```

It takes in the symbol you want to test and a plist of rules.


So far, that's pretty much it, for now! It does the very bare minimum I need to get a tool up and running quickly.
I probably will periodically add features and convenience functions to make it easier to use.
