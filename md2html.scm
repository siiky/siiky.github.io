#!/usr/bin/env -S csi -s
(import
  (chicken process-context)
  lowdown
  )

(define-constant before
#<<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="" xml:lang="">
<head><meta charset="utf-8"/></head>
<body>
EOF
)

(define-constant after
#<<EOF

</body>
</html>
EOF
)

(with-output-to-file
  (car (command-line-arguments))
  (lambda ()
    (print before)
    (markdown->html)
    (print after)))
