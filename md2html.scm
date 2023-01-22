#!/usr/bin/env -S csi -s

(import
  (chicken process-context)
  lowdown
  )

(define argv (command-line-arguments))
(define standalone?
  (and (not (null? argv))
       (string=? (car argv) "standalone")))

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

(when standalone? (print before))
(markdown->html)
(when standalone? (print after))
