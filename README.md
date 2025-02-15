# eo-config
Personal Configuration File Library

A simple library to manage configuration files of tools I write.


It reads in a file, assuming that the first line of the file is a plist.

First, you should define the allowed parameter names in your configuration file,
using the macro `define-allowed-names`. This list gets stored in the special variable `*config-allowed-names*`.


```lisp
(define-allowed names
    'foo-dir
    'bar-value
    'baz-toggle)
```

The function `make-config-globals` takes in the configuration file and a list of symbols.
Here, you can pass in the special variable `*config-allowed-names*`.
It creates the global variables and sets the value associated to it in the configuration file.

```lisp
(make-config-globals (load-config "example.conf") *config-allowed-names*)
```

You can also optionally set rules the configuration parameters must adhere to:

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
