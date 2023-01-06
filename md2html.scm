#!/usr/bin/env -S csi -s
(import
  (chicken process-context)
  lowdown
  )

(with-output-to-file
  (car (command-line-arguments))
  (lambda ()
    (markdown->html)
    ))
