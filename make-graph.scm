#!/usr/bin/env -S csi -s
(import
  (chicken file)
  (chicken irregex)
  (chicken pathname)
  (chicken process-context)
  (chicken sort)
  (chicken string)

  srfi-1
  srfi-13
  (rename
    (only srfi-197
	  chain
	  chain-lambda)
    (chain =>)
    (chain-lambda ->))

  gmi
  )

(define args (command-line-arguments))
(define root (car args))
(define files (cdr args))

(change-directory root)

(define ((split f g) x)
  (cons (f x) (g x)))

(define (>< f g) (split (o f car) (o g cdr)))

(define (relative-to to path)
  (normalize-pathname (make-pathname (pathname-directory to) path)))

(define relative-link?
  (let ((regex (irregex "^[a-z]+:"))) ; URLs w/ scheme
    (lambda (link)
      (not (or (string-prefix? "/" link)
	       (irregex-search regex link))))))

(define ((relative-link->absolute-link from) link)
  (if (relative-link? link) (relative-to from link) link))

(define remove-root
  (-> (string-split _ "/" #f)
      ((lambda (l) (if (string=? (car l) root) (cdr l) l)) _)
      (string-join _ "/")))

(define read-links
  (-> (with-input-from-file _ gmi:read #:text)
      (filter gmi:link? _)
      (map gmi:link:uri _)))

(define (writeln x)
  (write x)
  (newline))

(print "(")

(=> files
    (filter (-> (pathname-extension _) (string=? _ "gmi")) _)
    (map remove-root _)
    (sort _ string<?)
    (map (split identity read-links) _)
    (map (lambda (kv)
	   ((>< identity
		;(-> (filter relative-link? _)
		(-> (map (relative-link->absolute-link (car kv)) _)
		    (delete-duplicates _ string=?)
		    (sort _ string<?)))
	    kv))
	 _)
    (for-each writeln _))

(print ")")
