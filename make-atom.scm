#!/usr/bin/env -S csi -s

(import
  (chicken io)
  (chicken pathname)
  (chicken process-context)
  (chicken string)

  (srfi 1)
  (rename
    (only (srfi 197)
          chain
          chain-lambda)
    (chain =>)
    (chain-lambda ->))

  (prefix atom |atom:|)
  )

(define-constant host "siiky.srht.site/")
(define-constant title "nothing interesting here")

(define root (car (command-line-arguments)))

(define (make-uri scheme path)
  (string-append scheme "://" host path))
(define date->datetime (cute string-append <> "T00:00:00Z"))

(define author (atom:make-author name: "siiky" email: "~siiky/public@lists.sr.ht"))
(define id (make-uri "gemini" "atom.xml"))

(define (make-entry cdate udate title uri.gmi lang)
  (define (make-link uri type)
    (atom:make-link uri: uri type: type title: title uri-language: lang))

  (let ((uri.html (pathname-replace-extension uri.gmi "html")))
    (atom:make-entry
      id: (make-uri "gemini" uri.gmi)
      title: (atom:make-title title)
      updated: (date->datetime udate)
      published: (date->datetime cdate)
      links: `(,(make-link (make-uri "https" uri.html) 'html)
                ,(make-link (make-uri "gemini" uri.gmi) "text/gemtext"))
      content: (=> uri.html
                   (string-append root "/" _)
                   (with-input-from-file _ read-string)
                   (atom:make-content _ type: 'html)))))

(define (make-atom entries)
  (let ((udate (atom:entry-updated (car entries))))
    (atom:make-atom-doc
      (atom:make-feed
        authors: `(,author)
        id: id
        title: (atom:make-title title)
        updated: udate
        entries: entries
        links: `(,(atom:make-link uri: (make-uri "https" "atom.xml") type: 'atom relation: "self" title: title uri-language: "en"))))))

;; Assume entries are already sorted by udate, first being the lastest
(define atom
  (=> (read-lines)
      (map (-> (string-split _ "\t")
               (append _ '("en")) ; default language to EN
               (take _ 5)
               (apply make-entry _))
           _)
      (make-atom _)
      ))

(atom:write-atom-doc atom)
