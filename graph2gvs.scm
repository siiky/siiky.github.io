#!/usr/bin/env -S csi -s
(import
  (chicken file)
  (chicken irregex)
  (chicken pathname)
  (chicken process-context)
  (chicken string)

  srfi-1
  srfi-13
  srfi-42
  (rename
    (only srfi-197
	  chain
	  chain-lambda)
    (chain =>)
    (chain-lambda ->))
  )

(define args (command-line-arguments))
(define root (car args))
(define graph.scm (cadr args))

(define edges
  (list-ec
    (:list kv (with-input-from-file graph.scm read))
    (:let from (car kv))
    (:list to (cdr kv))
    `(-> ,from ,to)))

(define graph
  `(digraph links
	    (settings
	      (graph
		(layout sfdp)
		(splines curved)))
	    ,@edges))

(write graph)
