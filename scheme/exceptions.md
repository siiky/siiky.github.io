# Exceptions (aka Conditions) in Scheme

**DISCLAIMER**: I'm not a very advanced Schemer, and conditions in Scheme are
especially confusing to me. Please excuse any possibly wrong terminology or
claims. If you have any comments, shoot me a message.

Exceptions are (in general) very weird things, but even more so in Scheme. I
have little (close to none) experience with them, and the little that I have
was attained from need (i.e., "works? great!").

## Conditions

Conditions are objects that represent an exception (like instances of a
subclass of `Exception` in Java, maybe?), and in Scheme you can use them just
like any other object. You can create one, pass it around, and not even throw
it. It's also possible to throw non-condition objects (maybe for non-local or
early return?).

## Exception Handling

### Catching

A neat way to _catch_ exceptions is with [`condition-case`][0]. You give it an
expression to evaluate, give it the kinds of exceptions you are expecting and
how to deal with them, and voilà:

```scm
; General usage
(condition-case expression
  ((kind1 kind2) (print "kind1 kind2") ...)
  ((kind) (print "kind") ...)
  ; con may not be a condition
  (con () (print "some other kind: " con) ...))

; Concrete example
(condition-case
  (begin
    (print "This is part of the expression")
    (car (/ 42 0)))
  ((exn type)
    (print "Wrong type")
    #f)
  ((exn arithmetic)
    (print "Some arithmetic error")
    0)
  (con ()
    (print con " ¯\\_(ツ)_/¯")
    con))

;; prints:
;; This is part of the expression
;; Some arithmetic error
;; => 0
```

If no exception is thrown when evaluating the expression, the value of the
`condition-case` block is that of the expression. If an exception is thrown,
then the value is that of the last of the expressions associated with the first
matching branch. If there is no matching branch, the exception is propagated,
until a handler for it is found (or none, in which case the program crashes).

The condition cases should be ordered from more specific to less specific. For
example, a condition of kind `(exn type)` is also of kind `(exn)`, so the
former should be above the latter.

```scm
(condition-case (car 'some-val)
  ((exn)      (print "Wrong type"))
  ((exn type) (print "This will never happen")))
```

### Throwing

If you want to throw something, use [`signal`][1]. There are also [`abort`][2]
and [`raise`][3], but I don't know why/when to use them. In CHICKEN, SRFI-18's
`raise` is just `signal`, but this may change in the future and might not even
be the case in other implementations.

## Related Topics of Interest

 * Continuable and non-continuable exceptions
 * How is all this implemented?
 * How to make a condition and how to get things out of a condition object

[0]: https://api.call-cc.org/5/doc/chicken/condition/condition-case
[1]: https://api.call-cc.org/5/doc/chicken/condition/signal
[2]: https://api.call-cc.org/5/doc/chicken/condition/abort
[3]: https://api.call-cc.org/5/doc/srfi-18/raise
