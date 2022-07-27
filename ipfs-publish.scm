(import
  chicken.file
  chicken.io
  chicken.pathname
  chicken.port
  chicken.process-context

  srfi-1
  srfi-13
  (rename
    (only srfi-197
          chain
          chain-lambda)
    (chain =>)
    (chain-lambda ->))

  (prefix ipfs |ipfs:|)
  )

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

(define root (car (command-line-arguments)))
(define ipfs-nodes (cdr (command-line-arguments)))

(define make-ipfs-pathname
  (cute make-pathname root "ipfs" <>))

(define (replace-ipfs.gmi cid)
  (let ((filepath (make-ipfs-pathname "gmi")))
    (with-output-to-file
      filepath
      (cute
        print
        "=> ipfs://" cid "\n"
        "=> https://" cid ".ipfs.dweb.link\n"
        "=> ipfs.txt"
        ))))

(define (replace-ipfs.txt cid)
  (let* ((filepath (make-ipfs-pathname "txt"))
         (old-cid (and (file-exists? filepath)
                       (with-input-from-file filepath read-line)))
         (same? (and old-cid (string=? cid old-cid))))
    (unless same? (with-output-to-file filepath (cute print cid)))
    (values old-cid same?)))


(define (ipfs-file? path)
  (define pred (-> (make-ipfs-pathname _) (string-suffix? _ path)))

  ; Don't include the CID of the previous site version
  (not (any pred '("txt" "gmi" "html"))))


;(change-directory root)
(=> root
    (ipfs:writer/filesystem _ test: ipfs-file?)
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
    ((spy "IPFS add result: ") _)

    (find (-> (alist-ref 'Name _ eq? "/")
              (string=? _ root))
          _)
    ((spy "Root entry: ") _)

    ((lambda (entry)
       (let* ((cid (alist-ref 'Hash entry eq? ""))
              (size (alist-ref 'Size entry eq? ""))
              (name (alist-ref 'Name entry eq? ""))
              (name (if (string=? name "") "/" name)))

         ; Check the CID saved in ipfs.txt, if any, and write the new one
         (receive (old-cid same?) (replace-ipfs.txt cid)
           (when same? (eprint "CID hasn't changed: " cid))

           (unless same?
             ; Write ipfs.gmi
             (replace-ipfs.gmi cid)

             ; Pin the new CID to all hosts
             (for-each (lambda (host)
			 (parameterize ((ipfs:*host* host))
			   (if old-cid
			       (ipfs:pin/update old-path: old-cid new-path: cid unpin: #t)
			       (ipfs:pin/add path: cid recursive: #t)))
			 (eprint "Pinned the CID in " host "."))
		       ipfs-nodes)

             (eprint "CID" #\tab "Name" #\tab "Size" #\newline
                     cid #\tab name #\tab size)))))
     _))
