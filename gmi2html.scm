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
(define-constant gemini:// "gemini://")
(define-constant http:// "http://")
(define-constant https:// "https://")
(define-constant ipfs:// "ipfs://")
(define-constant magnet "magnet:")

(define-syntax eprint
  (syntax-rules ()
    ((eprint expr ...)
     (with-output-to-port (current-error-port) (cute print expr ...)))))


(define (gemini-uri? uri)
  (string-prefix? gemini:// uri))


(define gemipedia-uri?
  (let ((re (irregex "gemini://gemi\\.dev/cgi-bin/wp\\.cgi/view/([a-z]{2})\\?(.*)" 'utf8)))
    (cute irregex-match re <>)))


(define (wikipedia-uri lang article)
  (string-append https:// lang ".wikipedia.org/wiki/" article))


(define (gemini->portal uri)
  (chain uri
         (substring/shared _ (string-length gemini://))
         (string-append "https://portal.mozz.us/gemini/" _)))


(define (local-file-exists? base file)
  (file-exists? (make-absolute-pathname base file)))


(define (convert? directory uri)
  (member (pathname-extension uri) source-extensions))


(define (extension/gmi->html uri)
  (pathname-replace-extension uri ".html"))


(define (external-uri? uri)
  (any (cute string-prefix? <> uri)
       `(,http:// ,https:// ,magnet ,ipfs://)))


(define (rewrite-uri directory input-filename uri)
  (cond
    ((gemipedia-uri? uri)
     => (lambda (match)
          (let ((lang (irregex-match-substring match 1))
                (article (irregex-match-substring match 2)))
            (wikipedia-uri lang article))))

    ; Full gemini:// URI (assume external)
    ((gemini-uri? uri)
     (gemini->portal uri))

    ; Full non-gemini:// URI (assume external)
    ((external-uri? uri)
     uri)

    ; Local relative URI
    ((convert? directory uri)
     (cond
       ((local-file-exists? directory uri)
        (extension/gmi->html uri))

       (else
         (eprint "ERROR: " input-filename ": URI seems to be local but there's no such file: " uri)
         uri)))

    ; Anything else... This branch shouldn't be reached
    (else
      (eprint "This URI was unexpected: " uri)
      uri)))


(define (sxml-rules directory input-filename)
  (define (*text* _ str) str)
  (define (*default* . x) x)
  (define (a _ attrs . text)
    ;(eprint "<a> attrs (before): " attrs)
    (let* ((attrs (cdr attrs))
           (uri (rewrite-uri directory input-filename (car (alist-ref 'href attrs))))
           (attrs (alist-update 'href `(,uri) attrs))
           (attrs `(@ . ,attrs)))
      ;(eprint "<a> attrs (after): " attrs)
      `(a ,attrs . ,text)))

  `((a . ,a)
    (*text* . ,*text*)
    (*default* . ,*default*)))


(define ((convert lang directory input-filename))
  (let* ((sxml (geminih))
         (sxml (pre-post-order sxml (sxml-rules directory input-filename)))
         (sxml `(html (@ (lang ,lang) (xml:lang ,lang))
                      (body ,@sxml))))
    (display doctype-xhtml-1.0-strict)
    (sxml-display-as-html sxml)))


(define (get-path-things input-filename)
  (decompose-pathname (make-absolute-pathname (current-directory) input-filename)))


(define (main args)
  (let ((input-filename (car args))
        (lang "en"))
    (receive (directory _filename _extension) (get-path-things input-filename)
      (with-input-from-file input-filename (convert lang directory input-filename)))))


(main (command-line-arguments))
