(import
  chicken.base
  chicken.file
  chicken.port
  srfi-1
  srfi-197
  medea
  (prefix ipfs |ipfs:|))

(chain
  (read)

  (append-map
    (lambda (p)
      (cond
        ((directory-exists? p)
         (ipfs:writer/filesystem p #:dotfiles #t))
        ((file-exists? p)
         (ipfs:writer/file p #:name p))
        (else (error "Path doesn't exist" p))))
    _)

  (ipfs:add #:cid-version 1
            #:pin #f
            #:raw-leaves #t
            #:silent #t
            #:wrap-with-directory #t
            ;#:reader ipfs:reader/json
            #:writer _)

  (find (chain-lambda
          (alist-ref 'Name _ eq? "/")
          (string=? _ ""))
        _)

  ((lambda (alist)
     (let* ((hash (alist-ref 'Hash alist eq? ""))
            (size (alist-ref 'Size alist eq? ""))
            (name (alist-ref 'Name alist eq? ""))
            (name (if (string=? name "") "/" name))
            (scm `(("ipfs" ,hash)))
            (json `((ipfs . ,hash))))

       ; Machine-readable Scheme alist
       (with-output-to-file "ipfs.scm" (cute write scm))

       ; Machine-readable JSON
       (with-output-to-file "ipfs.json" (cute write-json json))

       ; Human-readable, to stderr
       (with-output-to-port
         (current-error-port)
         (cute print "Hash" #\tab "Name" #\tab "Size"
               #\newline hash #\tab name #\tab size))))
   _))
