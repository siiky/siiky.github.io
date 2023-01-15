#!/usr/bin/env -S csi -s
(import
  (chicken file)
  (chicken io)
  (chicken irregex)
  (chicken pathname)
  (chicken port)
  (chicken process-context)
  (srfi 1)
  (srfi 13)
  (srfi 197)
  gmi)

(define-syntax eprint
  (syntax-rules ()
    ((eprint expr ...)
     (with-output-to-port (current-error-port) (cute print expr ...)))))

(define-constant source-extensions '("gmi" "md" "org"))
(define-constant image-extensions '("svg" "png" "jpg" "jpeg" "webp"))
(define-constant gemini:// "gemini://")
(define-constant http:// "http://")
(define-constant https:// "https://")
(define-constant ipfs:// "ipfs://")
(define-constant magnet "magnet:")

(define ((? p? f g) x) ((if (p? x) f g) x))
(define phi (cute ? <> <> identity))

(define (gemini-link? l)
  (and (gmi:link? l)
       (string-prefix? gemini:// (gmi:link:uri l))))

(define gemipedia-link?
  (let ((re (irregex "gemini://gemi\\.dev/cgi-bin/wp\\.cgi/view/([a-z]{2})\\?(.*)" 'utf8)))
    (lambda (l)
      (and (gmi:link? l)
           (irregex-match re (gmi:link:uri l))))))

(define (wikipedia-url lang article)
  (string-append https:// lang ".wikipedia.org/wiki/" article))


(define (gemini->portal l)
  (gmi:link (chain (gmi:link:uri l)
                   (substring/shared _ (string-length gemini://))
                   (string-append "https://portal.mozz.us/gemini/" _))
            ((phi (o not string-null?)
                  (cute string-append "(Gemini Portal) " <>))
             (gmi:link:text l))))


(define (local-file-exists? base file)
  (file-exists? (make-absolute-pathname base file)))


(define (convert? directory l)
  (and (gmi:link? l)
       (let ((uri (gmi:link:uri l)))
         (and (member (pathname-extension uri) source-extensions)
              (local-file-exists? directory uri)))))


(define (extension/gmi->html link)
  (let* ((uri (gmi:link:uri link))
         (uri (pathname-replace-extension uri ".html"))
         (alt-text (gmi:link:text link)))
    (gmi:link uri alt-text)))


(define concatenate (cute apply append <>))
(define string-null? (chain-lambda (string-length _) (zero? _)))

(define (links? l) (and (list? l) (eq? (car l) 'links)))
(define (quotes? l) (and (list? l) (eq? (car l) 'quotes)))


(define (group-inner-loop outer-loop gmi:*? head-sym ret gmi)
  (let iloop ((ret ret)
              (gmi (cdr gmi))
              (group `(,(car gmi) ,head-sym)))
    (cond
      ((null? gmi)
       (reverse (cons (reverse group) ret)))
      ((gmi:*? (car gmi))
       (iloop ret (cdr gmi) (cons (car gmi) group)))
      (else (outer-loop (cons (reverse group) ret) gmi)))))

(define (group-elements gmi)
  (let oloop ((ret '())
              (gmi gmi))
    (cond
      ((null? gmi)
       (reverse ret))

      ((gmi:link? (car gmi))
       (group-inner-loop oloop gmi:link? 'links ret gmi))

      ((gmi:blockquote? (car gmi))
       (group-inner-loop oloop gmi:blockquote? 'quotes ret gmi))

      (else (oloop (cons (car gmi) ret) (cdr gmi))))))


(define (gmi:link->md:link l)
  (let* ((text (gmi:link:text l))
         (uri (gmi:link:uri l))
         (image? (member (pathname-extension uri) image-extensions)))
    ; TODO: Escape characters?
    (list
      (cond
        (image? (string-append "\n![" text "](" uri ")\n"))
        ((string-null? text) (string-append " * <" uri ">"))
        (else (string-append " * [" text "](" uri ")"))))))

(define (grouped-gmi-element->md-element elem)
  (cond
    ((gmi:text? elem) (list elem))

    ((gmi:header? elem)
     (list (string-append (make-string (gmi:header:level elem) #\#)
                          " "
                          (gmi:header:text elem))))

    ; Grouped list of links
    ((links? elem)
     `("" ,@(concatenate (map gmi:link->md:link (cdr elem))) ""))

    ((quotes? elem)
     `("\n"
       ,@(map (lambda (qt)
                (string-append "> " (gmi:blockquote:text qt)))
              (cdr elem))))

    ((gmi:link? elem)
     (gmi:link->md:link elem))

    ((gmi:list? elem)
     `("" ,@(map (cute string-append " * " <>) (gmi:list:items elem)) ""))

    ((gmi:blockquote? elem)
     `(,(string-append "> " (gmi:blockquote:text elem))))

    ((gmi:code? elem)
     `(""
       ; NOTE: lowdown doesn't support ``` code blocks
       ;,(string-append "```" (gmi:code:text elem))
       ;,@(gmi:code:lines elem)
       ;"```"
       ,@(map (cute string-append "\t" <>) (gmi:code:lines elem))
       ""))
    ))


(define (external-link? l)
  (any (cute string-prefix? <> (gmi:link:uri l))
       `(,http:// ,https:// ,magnet ,ipfs://)))


(define ((rewrite-links directory input-filename) l)
  (cond
    ; If it's not a link, do nothing to it
    ((not (gmi:link? l)) l)

    ((gemipedia-link? l)
     => (lambda (match)
          (let ((lang (irregex-match-substring match 1))
                (article (irregex-match-substring match 2)))
            (gmi:link (wikipedia-url lang article) (gmi:link:text l)))))

    ; External full gemini://... link?
    ((gemini-link? l) (gemini->portal l))

    ; "Local" link with local file?
    ((convert? directory l) (extension/gmi->html l))

    ; "Local" link but no file?
    ((and (gmi:link? l)
          (not (external-link? l))
          (not (local-file-exists? directory (gmi:link:uri l))))
     (eprint "ERROR: " input-filename ": Link seems to be local but there's no such file: " (gmi:link:uri l))
     l)

    ; Any other link
    (else l)))

(define (gmi2md input-filename)
  (let ((this-file (make-absolute-pathname (current-directory) input-filename)))
    (receive (directory _filename _extension) (decompose-pathname this-file)
      (chain (gmi:read)
             (map (rewrite-links directory input-filename) _)
             (group-elements _)
             (map grouped-gmi-element->md-element _)
             (concatenate _)
             (for-each write-line _)
             ))))

; ./gmi2md.scm docs/directory/file.gmi
(define (main args)
  (let ((input-filename (car args)))
    (with-input-from-file (car args) (cute gmi2md input-filename))))

(main (command-line-arguments))
