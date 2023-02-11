#!/usr/bin/env -S csi -s
(import
  (chicken file)
  (chicken irregex)
  (chicken pathname)
  (chicken port)
  (chicken process-context)

  (srfi 1)
  (srfi 13)
  (srfi 197)

  doctype
  geminih
  html-parser
  sxml-transforms
  )

(define-constant source-extensions '("gmi" "md" "org"))
(define-constant image-extensions '("svg" "png" "jpg" "jpeg" "webp"))
(define-constant asset-extensions '("patch" "txt" "xml" "gp"))
(define-constant gemini:// "gemini://")
(define-constant http:// "http://")
(define-constant https:// "https://")
(define-constant ipfs:// "ipfs://")
(define-constant magnet "magnet:")

(define-syntax eprint
  (syntax-rules ()
    ((eprint expr ...)
     (with-output-to-port (current-error-port) (cute print expr ...)))))

(define (thing-uri? extensions) (o (cute member <> extensions) pathname-extension))
(define source-uri? (thing-uri? source-extensions))
(define image-uri?  (thing-uri? image-extensions))
(define asset-uri?  (thing-uri? asset-extensions))

(define gemini-uri? (cute string-prefix? gemini:// <>))
(define wikipedia-uri (cute string-append https:// <> ".wikipedia.org/wiki/" <>))
(define local-file-exists? (compose file-exists? make-absolute-pathname))
(define extension/gmi->html (cute pathname-replace-extension <> ".html"))
(define (external-uri? uri)
  (any (cute string-prefix? <> uri) `(,http:// ,https:// ,magnet ,ipfs://)))

(define gemipedia-uri?
  (let ((re (irregex "gemini://gemi\\.dev/cgi-bin/wp\\.cgi/view/([a-z]{2})\\?(.*)" 'utf8)))
    (cute irregex-match re <>)))

(define gemini->portal
  (chain-lambda
    (substring/shared _ (string-length gemini://))
    (string-append "https://portal.mozz.us/gemini/" _)))

(define (rewrite-uri directory input-filename uri)
  (define image? (image-uri? uri))
  (define asset? (asset-uri? uri))
  (define new-uri
    (cond
      ((gemipedia-uri? uri)
       => (lambda (match)
            (let ((lang (irregex-match-substring match 1))
                  (article (irregex-match-substring match 2)))
              (wikipedia-uri lang article))))

      ; Full gemini:// URI (assume external)
      ((gemini-uri? uri) (gemini->portal uri))

      ; Full non-gemini:// URI (assume external)
      ((external-uri? uri) uri)

      ; Local relative URI
      ((source-uri? uri)
       (if (local-file-exists? directory uri)
         (extension/gmi->html uri)
         (begin
           (eprint "ERROR: " input-filename ": URI seems to be local but there's no such file: " uri)
           (exit 1)
           uri)))

      ; Anything else... This branch shouldn't be reached
      (else
        (unless (or image? asset?)
          (eprint "Unexpected URI: " uri))
        uri)))

  (values new-uri image?))

(define (sxml-rules directory input-filename)
  (define (make-a-tag uri attrs text)
    (let* ((attrs (alist-update 'href `(,uri) attrs))
           (attrs `(@ . ,attrs)))
      `(a ,attrs . ,text)))

  (define (make-img-tag uri attrs text)
    ;(eprint "make-img-tag:attrs: " attrs)
    ;(eprint "make-img-tag:text: " text)
    (let ((src `(src ,uri))
          (alt `(alt . ,text)))
      (let ((ret `(img (@ ,src ,alt))))
        ;(eprint "make-img-tag:ret: " ret)
        ret)))

  (define (tag/li? x)
    (and (list? x) (eq? (car x) 'img)))

  (define (*text* _ str) str)
  (define (*default* . x) x)
  (define (a _ attrs . text)
    (let* ((attrs (cdr attrs))
           (uri (car (alist-ref 'href attrs))))
      (receive (uri image?) (rewrite-uri directory input-filename uri)
        ((if image? make-img-tag make-a-tag) uri attrs text))))

  (define (li _ item)
    ;(eprint "item: " item)
    (if (tag/li? item) item `(li ,item)))

  (define (ul _ . items)
    (if (every tag/li? items)
      (intersperse items '(br))
      `(ul . ,items)))

  `((a . ,a)
    (ul . ,ul)
    (li . ,li)
    (*text* . ,*text*)
    (*default* . ,*default*)))

(define ((convert lang directory input-filename))
  (let* ((sxml (geminih))
         (sxml (pre-post-order sxml (sxml-rules directory input-filename)))
         (sxml `(html (@ (lang ,lang) (xml:lang ,lang))
                      (head (meta (@ (charset "utf-8"))))
                      (body ,@sxml))))
    (display doctype-xhtml-1.0-strict)
    (sxml-display-as-html sxml)))

(define get-path-things
  (o decompose-pathname (cute make-absolute-pathname (current-directory) <>)))

(define (main args)
  (let ((input-filename (car args))
        (lang "en")) ; TODO
    (receive (directory _filename _extension) (get-path-things input-filename)
      (with-input-from-file input-filename (convert lang directory input-filename)))))

(main (command-line-arguments))
