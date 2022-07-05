(import
  chicken.io
  chicken.pathname
  srfi-197
  gmi)

(define ((? p? f g) x) ((if (p? x) f g) x))
(define phi (cute ? <> <> identity))


(define (absolute-own-link? l)
  (and (gmi:link? l)
       (eq? (string-ref (cadr l) 0) #\/)))

(define (gmi-link? l)
  (equal? (pathname-extension (gmi:link:uri l)) "gmi"))

(define (convert? l)
  (and (absolute-own-link? l) (gmi-link? l)))


(define (extension/gmi->html link)
  (let* ((uri (gmi:link:uri link))
         (uri (pathname-replace-extension uri ".html"))
         (alt-text (gmi:link:text link)))
    `(link ,uri ,alt-text)))


(define concatenate (cute apply append <>))


(define (gmi-element->md-element elem)
  (cond
    ((gmi:text? elem) (list elem))

    ((gmi:header? elem)
     (list (string-append (make-string (gmi:header:level elem) #\#)
                          " "
                          (gmi:header:text elem))))

    ((gmi:link? elem)
     (list (string-append "* [" (gmi:link:text elem) "](" (gmi:link:uri elem) ")")))

    ((gmi:list? elem)
     (map (cute string-append "* " <>) (gmi:list:items elems)))

    ((gmi:code? elem)
     `(,(string-append "```" (gmi:code:text elem))
        ,@(gmi:code:lines elem)
        "```"))
    ))


(chain (gmi:read)
       (map (phi absolute-own-link? extension/gmi->html) _)
       (map gmi-element->md-element _)
       (concatenate _)
       (for-each write-line _))
