(import
  srfi-1
  srfi-197
  (chicken process-context)
  gmi)

(chain
  (command-line-arguments)
  (member "--" _)
  (cdr _)
  (filter
    (cute call-with-input-file
          <>
          (chain-lambda
            (gmi:read _)
            (any gmi:link? _)
            (not _)
            )
          )
    _)
  (print _)
  )
