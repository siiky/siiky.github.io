(import
  chicken.base
  chicken.file
  chicken.port
  srfi-1
  srfi-197
  medea
  (prefix ipfs |ipfs:|))

(define-syntax
  eprint
  (syntax-rules ()
    ((eprint arg ...)
     (with-output-to-port (current-error-port) (cute print arg ...)))))

(define-syntax
  spy
  (syntax-rules ()
    ((spy arg ...)
     (lambda (x)
       (eprint arg ... x)
       x))))

(chain
  (read)

  (append-map
    (lambda (p)
      (cond
        ((directory-exists? p) (ipfs:writer/filesystem p #:dotfiles #t))
        ((file-exists? p) (ipfs:writer/file p #:name p))
        (else (error "Path doesn't exist" p))))
    _)

  (ipfs:add
    #:cid-version 1
    #:pin #f ; ipfs:pin/update pins the new CID
    #:raw-leaves #t
    ; TODO: Did the behaviour of the `silent` flag change? Seems to
    ; return all the entries...
    ;#:reader ipfs:reader/json
    #:silent #t
    #:wrap-with-directory #t
    #:writer _)

  ((spy "Added entries: ") _)

  (find (chain-lambda
          (alist-ref 'Name _ eq? "/")
          (string=? _ ""))
        _)

  ((lambda (alist)
     (let* ((CID (alist-ref 'Hash alist eq? ""))
            (size (alist-ref 'Size alist eq? ""))
            (name (alist-ref 'Name alist eq? ""))
            (name (if (string=? name "") "/" name))
            (scm `(("ipfs" ,CID)))
            (json `((ipfs . ,CID)))
            (old-CID
              (chain
                (with-input-from-file "ipfs.scm" read)
                (alist-ref "ipfs" _ string=? "" '(""))
                (car _))))

       (if (string=? old-CID CID)
           (eprint "CID hasn't changed -- nothing else to do!")
           (begin
             ; Replace old CID with new. Errors with HTTP 500 if the old CID is
             ; not pinned on the node.
             (eprint "Replacing old CID (" old-CID ") with new (" CID ")")
             (ipfs:pin/update #:old-path old-CID #:new-path CID #:unpin #t)
             (eprint "Replaced old CID with new.")

             ; Machine-readable Scheme alist
             (with-output-to-file "ipfs.scm" (cute write scm))

             ; Machine-readable JSON
             (with-output-to-file "ipfs.json" (cute write-json json))

             ; Human-readable, to stderr
             (eprint "CID" #\tab "Name" #\tab "Size" #\newline
                     CID #\tab name #\tab size)))))
   _))
