(import gmi)

(define (edit-gmi gmi options)
  gmi)

(let ((options '())
      file (open-rw-file path))
  (=> file
      (gmi:read _)
      (edit-gmi _ options)
      (gmi:write _ file))
  (port-close file))
