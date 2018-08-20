#lang vernacular/lang/sweet-exp vernacular/simple-module

:export #'fact

defun fact (n)
  if {n <= 1}
    1
    {n * fact{n - 1}}
