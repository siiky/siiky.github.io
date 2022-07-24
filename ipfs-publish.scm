(import
  chicken.file
  chicken.pathname
  chicken.process-context

  srfi-1
  srfi-13
  srfi-197

  (prefix ipfs |ipfs:|)
  )

(define ((ipfs-file? root) path)
  (define suffix (cute string-append root "/ipfs." <>))
  (define pred
    (chain-lambda (suffix _)
                  (string-suffix? _ path)))

  ; Don't include the CID of the previous site version
  (not (any pred '("scm" "txt" "json" "html"))))


(let ((root (car (command-line-arguments))))
  ;(change-directory root)
  (chain (ipfs:writer/filesystem root test: (ipfs-file? root))
         (ipfs:add
           cid-version: 1
           pin: #f
           raw-leaves: #t
           ; TODO: Did the behaviour of the `silent` flag change? Seems to
           ; return all the entries...
           ;#:reader ipfs:reader/json
           silent: #t
           wrap-with-directory: #f
           writer: _)
         (print _)
         ))
