#!/usr/bin/env -S csi -s
(import
  (chicken process-context)
  (only srfi-13 string-join)
  (only srfi-19 current-date date->string)
  typed-records
  cling
  gmi)

(defstruct options
  (help #f)
  (rest '())
  (title "")
  (author "siiky")
  (created (date->string (current-date) "~Y/~m/~d"))
  (updated created)
  (mupdate updated)
  (tags '())
  (urls '()))

(define (page-template #!key title author created updated mupdate tags urls)
  `(,(gmi:header 1 title)
     ,author
     ,created
     ,updated
     ,mupdate
     ,(string-join tags ",")
     ""
     ,@(map (cute gmi:link <> "") urls)))

(define opts
  (cling
    (lambda (ret _ rest) (update-options ret rest: rest))
    (arg '((-h --help))
         help: "Show this help text"
         kons: (lambda (ret _ _) (update-options ret help: #t)))
    (arg '((-T --title) . title)
         help: "Page title"
         kons: (lambda (ret _ title) (update-options ret title: title)))
    (arg '((-a --author) . author)
         help: "Page author (default: siiky)"
         kons: (lambda (ret _ author) (update-options ret author: author)))
    (arg '((-c --created) . created)
         help: "Created date (default: today)"
         kons: (lambda (ret _ created) (update-options ret created: created)))
    (arg '((-U --updated) . updated)
         help: "Updated date (default: created)"
         kons: (lambda (ret _ updated) (update-options ret updated: updated)))
    (arg '((-m --mupdate) . mupdate)
         help: "Minor update date (default: updated)"
         kons: (lambda (ret _ mupdate) (update-options ret mupdate: mupdate)))
    (arg '((-t --tag) . tag)
         help: "Tag(s)"
         kons: (lambda (ret _ tag) (update-options ret tags: (cons tag (options-tags ret)))))
    (arg '((-u --url) . url)
         help: "URL(s)"
         kons: (lambda (ret _ url) (update-options ret urls: (cons url (options-urls ret)))))))

(let ((options (process-arguments opts (make-options) (command-line-arguments))))
  (when (options-help options)
    (help opts (program-name))
    (exit 1))
  (gmi:write
    (page-template
      title: (options-title options)
      author: (options-author options)
      created: (options-created options)
      updated: (options-updated options)
      mupdate: (options-mupdate options)
      tags: (reverse (options-tags options))
      urls: (reverse (options-urls options)))))
