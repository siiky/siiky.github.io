% `kless`
% siiky
% 2019/10/13

I read recently a blog post called [_The Lisp
Curse_](https://www.winestockwebdesign.com/Essays/Lisp_Curse.html) (not yet
HTTPS enabled, but hoping it will be in the future). In this post, the author,
Rudolf Winestock, says that "Making Scheme object-oriented is a sophomore
homework assignment"; and so, for fun, I tried hacking an abstraction for
defining classes in Scheme, with the little knowledge of macros that I have.

# Implementation

Below is the definition of the `kless` macro.

```scm
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!! Read identifiers with Metalocalypse Pickles' accent !!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(define-syntax kless
  ; `meth` is a reserved keyword inside the macro, i.e., it has a special
  ; meaning and you can't name a kless or method "meth"
  (syntax-rules (meth)

    ;; TEMPLATE
    ((kless
       (kless-name ver-name ...)
       (meth (meth-name meth-args ...)
             meth-body ...) ...)

     ;; RESULT
     (begin
       ; If your Scheme of choice doesn't support curried definitions
       ; use lambda instead:
       ;   (define (kless-name ver-name ...)
       ;     (lambda (method self . args)
       (define ((kless-name ver-name ...) method self . args)
         (define (err sym)
           (error (string-append "'" (symbol->string sym) "'? dat shiet dun exist yo")))

         (define (getter ver)
           (case ver
             ((ver-name) ver-name) ...
             (else (err ver))))

         (define (setter ver val)
           (case ver
             ((ver-name) (set! ver-name val)) ...
             (else (err ver))))

         ;; Dispatch
         (case method
           ((get) (apply getter args))
           ((set) (apply setter args))
           ((meth-name)
            (apply (lambda (meth-args ...)
                     meth-body ...)
                   args)) ...
           (else (err method))))

       ;; [GS]etters
       (define (ver-name self . val)
         (if (null? val)
             (self 'get self 'var-name)
             (self 'set self 'var-name (car val)))) ...

       ;; Custom methods
       (define (meth-name self meth-args ...)
         (self 'meth-name self meth-args ...)) ...))))
```

With `kless` you can specify instance variables, with getters and destructive
setters automatically defined for you, and instance (non-static) methods.
Because the methods' bodies are inserted into the object itself, instance
variables are in scope, and no extra magic is needed for making them available.

Having two classes with methods (or variables, for that matter) of the same
name results in two functions with exactly the same names and bodies being
defined. By defining the two classes above (`dek` and `blenk-dek`), `show-me`
is defined twice, like so:

```scm
(define (show-me self)
  (self 'show-me self))
```

This is OK in Scheme, so dick-typing (a la Python) is supported.

# Example `kless`es

The following example shows that/how `kless` works, and what OO is good for.

Defining two classes:

```scm
;; Very important dek class! It is used to represent various types of deks
(kless (dek x y)
       (meth (show-me)
             (print "8" (make-string x #\=) "D" (make-string y #\~))))

(kless (blenk-dek x)
       (meth (show-me)
             (print "8" (make-string x #\=) "D")))
```

Inspecting generated procedures and trying things out with `csi` (the CHICKEN
Scheme Interpreter):

```
#;1> dek
#<procedure (dek x y)>
#;2> blenk-dek
#<procedure (blenk-dek x)>
#;3> x
#<procedure (x self270 . val282)>
#;4> y
#<procedure (y self204 . val216)>
#;5> show-me
#<procedure (show-me self270)>
#;6> (show-me (dek 2 4))
8==D~~~~
#;7> (show-me (blenk-dek 4))
8====D
```

# Thorns

`self` is not in scope (or rather, `self`, the object itself, is in scope, but
is not called `self` because of `syntax-rules`{.scm} magic). Recursive methods
are still possible, with a named-`let`{.scm} or `define`{.scm}. Just don't use
other methods.

No type predicate is defined, but can be easily implemented.

No hierarchy, inheritance, no nothing!

No "static" class methods. Because, what? Just make a function, prefix it with
the class name and be done with it.

No extending already defined classes, a la Haskell's type classes (with
`instance ... where`{.hs}), or Rust's traits (with `impl`{.rs}). Put everything
inside `kless` and dick-typing does the rest.

Method overriding on a per object basis is not possible. This may be easy to
implement. One way is to put a table inside the object, from method name to
procedure (`Symbol -> Method`{.hs}), and arrange a way to get and set that
procedure from outside the object. A problem arises then: this new method
doesn't have the instance variables in scope. One could be tempted to think
that something like the following solves the problem.

```scm
(define (meth-setter meth-name meth-maker)
  (update-meth-table meth-name (meth-maker ver-name ...)))
```

This would indeed bring the current variables into scope, but not future
updates, as this new method's scope is not the same as the object's scope
itself. (There was also something similar on [_Let Over
Lambda_](https://letoverlambda.com) IIRC)

Another option would be to have another indirection for the actual method's
code, such that the current instance variables are given on each call.
Calling one such method would be something like:

```scm
(((lambda (ver-name ...)    ; instance variables
    (lambda (meth-args ...) ; method parameters
      meth-body ...))       ; method body
  ver-name ...)             ; get the method procedure, with instance variables in scope
 meth-args ...)             ; call the methodd
```
