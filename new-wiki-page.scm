#!/usr/bin/env -S csi -s

(import
  (chicken irregex)

  srfi-19

  gmi
  optimism
  )

(define-constant wiki-root "docs/wiki/")

(define-syntax define-parameter
  (syntax-rules ()
    ((define-parameter name init guard?)
     (define name (make-parameter init guard?)))
    ((define-parameter name init)
     (define name (make-parameter init)))
    ((define-parameter name)
     (define name (make-parameter #f)))))

(define today (date->string (current-date) "~Y/~m/~d"))

(define string-null? (o zero? string-length))

(define (tags-guard tags)
  (let ((regex "[a-zA-Z0-9_.-]+(,[a-zA-Z0-9_.-]+)*"))
    (unless (or (string-null? tags)
                (irregex-match? regex tags))
      (error (string-append "tags don't match the regex " regex)))
    tags))

(define-parameter filename)
(define-parameter title "")
(define-parameter author "siiky")
(define-parameter created today)
(define-parameter updated today)
(define-parameter edited today)
(define-parameter tags "" tags-guard)

(define options
  (parse-command-line
    `((-f . ,filename)
      (-t . ,title)
      (-a . ,author)
      (-c . ,created)
      (-u . ,updated)
      (-e . ,edited)
      (-T . ,tags))))

(define page
  `(
    ,(gmi:header 1 (title))
    ,(author)
    ,(created)
    ,(updated)
    ,(edited)
    ,(tags)
    ))

(gmi:write page) ; TODO: Write to given filename
