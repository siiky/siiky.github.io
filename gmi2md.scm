(import
  chicken.file
  chicken.io
  chicken.pathname
  chicken.process-context
  srfi-197
  gmi)

(define ((? p? f g) x) ((if (p? x) f g) x))
(define phi (cute ? <> <> identity))


(define ((convert? gemini-root) l)
  (and (gmi:link? l)
       (let ((uri (gmi:link:uri l)))
         (and (member (pathname-extension uri) '("gmi"))
              (file-exists? (make-absolute-pathname gemini-root uri))))))


(define (extension/gmi->html link)
  (let* ((uri (gmi:link:uri link))
         (uri (pathname-replace-extension uri ".html"))
         (alt-text (gmi:link:text link)))
    `(link ,uri ,alt-text)))


(define concatenate (cute apply append <>))
(define string-null? (chain-lambda (string-length _) (zero? _)))


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

           (else
             (oloop (cons (reverse links) ret) gmi))
           )))

      (else
        (oloop (cons (car gmi) ret) (cdr gmi))))))


(define (gmi:link->md:link l)
  (let* ((text (gmi:link:text l))
         (uri (gmi:link:uri l))
         (link (if (string-null? text)
                   (string-append "<" uri ">")
                   (string-append "[" text "](" uri ")"))))
    ; TODO: Escape characters?
    (list (string-append " * " link))))

(define (grouped-gmi-element->md-element elem)
  (cond
    ((gmi:text? elem) (list elem))

    ((gmi:header? elem)
     (list (string-append (make-string (gmi:header:level elem) #\#)
                          " "
                          (gmi:header:text elem))))

    ; Grouped list of links
    ((and (list? elem) (eq? (car elem) 'links))
     `("" ,@(concatenate (map gmi:link->md:link (cdr elem))) ""))

    ((gmi:link? elem)
     (gmi:link->md:link elem))

    ((gmi:list? elem)
     `("" ,@(map (cute string-append " * " <>) (gmi:list:items elems)) ""))

    ((gmi:code? elem)
     `(""
       ;"```" ; NOTE: lowdown doesn't support alt-text for code blocks
       ,(string-append "```" (gmi:code:text elem))
       ,@(gmi:code:lines elem)
       "```"
       ""))
    ))


(define (main args)
  (let ((gmi-file (car args))
        (gemini-root (make-absolute-pathname (current-directory) (cadr args))))
    (call-with-input-file
      gmi-file
      (chain-lambda (gmi:read _)
                    (map (phi (convert? gemini-root) extension/gmi->html) _)
                    (group-links _)
                    (map grouped-gmi-element->md-element _)
                    (concatenate _)
                    (for-each write-line _)
                    ))))


(main (command-line-arguments))
