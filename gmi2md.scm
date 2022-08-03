#!/usr/bin/env -S csi -s
(import
  chicken.file
  chicken.io
  chicken.pathname
  chicken.process-context
  srfi-197
  gmi)

(define-constant source-extensions '("gmi" "md" "org"))
(define-constant image-extensions '("svg" "png" "jpg" "jpeg" "webp"))

(define ((? p? f g) x) ((if (p? x) f g) x))
(define phi (cute ? <> <> identity))


(define ((convert? gemini-root) l)
  (and (gmi:link? l)
       (let ((uri (gmi:link:uri l)))
         (and (member (pathname-extension uri) source-extensions)
              (file-exists? (make-absolute-pathname gemini-root uri))))))


(define (extension/gmi->html link)
  (let* ((uri (gmi:link:uri link))
         (uri (pathname-replace-extension uri ".html"))
         (alt-text (gmi:link:text link)))
    (gmi:link uri alt-text)))


(define concatenate (cute apply append <>))
(define string-null? (chain-lambda (string-length _) (zero? _)))

(define (links? l) (and (list? l) (eq? (car l) 'links)))


(define (group-links gmi)
  (let oloop ((ret '())
              (gmi gmi))
    (cond
      ((null? gmi)
       (reverse ret))

      ((gmi:link? (car gmi))
       (let iloop ((ret ret)
                   (gmi (cdr gmi))
                   (links `(,(car gmi) links)))
         (cond
           ((null? gmi)
            (reverse (cons (reverse links) ret)))
           ((gmi:link? (car gmi))
            (iloop ret (cdr gmi) (cons (car gmi) links)))
           (else (oloop (cons (reverse links) ret) gmi)))))

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


(define (main args)
  (let ((gemini-root (make-absolute-pathname (current-directory) (car args))))
    (chain (gmi:read)
           (map (phi (convert? gemini-root) extension/gmi->html) _)
           (group-links _)
           (map grouped-gmi-element->md-element _)
           (concatenate _)
           (for-each write-line _)
           )))


(main (command-line-arguments))
