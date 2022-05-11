#!/usr/bin/env -S csi -s

(import
  (only chicken.base alist-ref)
  (only chicken.keyword keyword?)
  (only chicken.process-context command-line-arguments)

  (only srfi-1 any assoc)
  (only srfi-13 string-any string-concatenate)
  (only matchable match-lambda)

  (rename
    (only ssg ssg)
    (ssg ssg:build))

  (prefix
    (only ssg.css
          css-content
          css-file)
    |ssg:|)

  (prefix
    (only ssg.index
          dir
          ent
          idx
          idx-file-input-filename)
    |ssg:|)

  (prefix
    (only ssg.converters.lowdown
          idx->html
          md->html)
    |ssg:lowdown:|)

  (prefix
    (only ssg.converters.pandoc
          append-default-extra-options!
          md->html
          ->)
    |ssg:pandoc:|)

  (prefix
    (only ssg.feed
          feed-options
          feed-entry-path)
    |ssg:|)

  (prefix
    (only ssg.site
          make-converter-table
          site
          site-index
          index-entries-for-feed)
    |ssg:|)

  (prefix
    (only ssg.result
          handle-result)
    |ssg:|)
  )

(define-constant feed-output-path "atom.xml")

(define ->bool (o not not))

; NOTE: For use with ssg.converters.lowdown.
; Read http://www.more-magic.net/docs/scheme/sxslt.pdf for more info
(define (make-sxml-custom-rules footer)
  (define (page _ title css . content)
    (let ((css (and css `(style ,(ssg:css-content css)))))
      `(html (@ (lang "en"))
             (head (meta (@ (charset "UTF-8"))) "\n"
                   ,css ; it seems #f is ignored
                   (title ,title))
             (body ,content)
             (footer
               "\nplaces:\n"
               (a (@ (href "/")) "Go home!") "\n"
               (a (@ (href "https://github.com/siiky")) "GitHub") "\n"
               (a (@ (href ,(string-append "/" feed-output-path))) "Atom Feed")))))

  (define (*text* _ str) str)
  (define (*default* . x) x)
  (define (h1 _ title) `(h1 "# " ,title "\n"))
  (define (h2 _ title) `(h2 "## " ,title "\n"))
  (define (h3 _ title) `(h3 "### " ,title "\n"))
  (define (h4 _ title) `(h4 "#### " ,title "\n"))
  (define (h5 _ title) `(h5 "##### " ,title "\n"))
  (define (h6 _ title) `(h6 "###### " ,title "\n"))
  (define (p _ . content) `(,@content "\n\n"))
  (define (l _ ref) `(a (@ (href ,ref)) ,ref "\n"))
  (define (toc _ . entries) `(ul ,entries))
  (define (img _ attrs)
    (let ((attrs (cdr attrs)))
      (let((src (cadr (assoc 'src attrs eq?)))
           (alt (cdr  (assoc 'alt attrs eq?))))
        (let ((alt (string-concatenate
                     `("![" ,@(or alt '("")) "](" ,@(or src '("")) ")"))))
          `(img (@ (src ,src) (alt ,alt)))))))
  (define (code _ . content)
    (if (and (not (null? content))
             (any (lambda (str) (string-any #\newline str)) content))
      `(code "```" ,@content "```\n")
      `(code "`" ,@content "`")))
  (define (strong _ . content)
    `(strong "**" ,@content "**"))
  (define (em/i tag . content)
    `(,tag "_" ,@content "_"))

  `((h1 . ,h1)
    (h2 . ,h2)
    (h3 . ,h3)
    (h4 . ,h4)
    (h5 . ,h5)
    (h6 . ,h6)
    (code . ,code)
    (p . ,p)
    (l . ,l)
    (toc . ,toc)
    (strong . ,strong)
    (em . ,em/i)
    (i . ,em/i)
    (img . ,img)
    (page . ,page)
    (*text* . ,*text*)
    (*default* . ,*default*)))

(define index
  (let ((wip 'wip))
    (ssg:idx "nothing interesting here"
             (ssg:dir "algebra"
                      (ssg:ent "./groups.md" "2019/01/01" "A quick intro to Group Theory")
                      (ssg:ent "./cat_theory_perf.md" "2019/10/15" "Category Theory and Performance")
                      (ssg:ent wip "./functors.md" "2019/10/15" "Functors")
                      (ssg:ent "./faucets.md" "2020/11/20~2021/07/23" "Faucets")
                      (ssg:ent wip "./posets.md" "2021/01/25" "POSets")
                      )

             (ssg:dir "books"
                      (ssg:ent "./list.md" "2020/12/21" "Books list")
                      (ssg:ent "./bill_gates.how_to_avoid_a_climate_disaster.org" "2022/04/09" "/How to Avoid a Climate Disaster/, Bill Gates")
                      )

             (ssg:dir "functional_programming"
                      (ssg:ent wip "./immutability.md" "2020/04/10" "Immutability")
                      (ssg:ent "./curriculum.org" "2021/05/25" "[WIP] Programming Curriculum")
                      (ssg:ent "./naming.org" "2021/12/15" "Naming Operations rather than Variables")
                      )

             (ssg:dir "osm"
                      (ssg:ent "./cheatsheet.md" "2021/07/12" "OpenStreetMap Cheat Sheet")
                      (ssg:ent "./prettymaps.org" "2022/01/22" "prettymaps")
                      (ssg:ent "./overpass.org" "2022/03/26" "Overpass")
                      )

             (ssg:dir "kB45oC"
                      (ssg:ent "./seamless_updates.md" "2020/03/13" "Seamless Updates")
                      (ssg:ent "./gnuplot.org" "2022/01/12" "Gnuplot Quickstart")
                      (ssg:ent "./water_stations.org" "2022/05/11" "Water Stations")
                      )

             (ssg:dir "scheme"
                      (ssg:ent "./exceptions.md" "2019/08/13" "Exceptions in Scheme")
                      (ssg:ent "./gv-dsl.md" "2019/10/01" "GraphViz DOT Scheme DSL")
                      (ssg:ent "./kless.md" "2019/10/13" "kless")
                      (ssg:ent "./pipes.md" "2022/01/09" "Pipes")
                      (ssg:ent "./reader-syntax.org" "2022/03/15" "Reader Syntax")
                      )

             (ssg:dir "care"
                      (ssg:ent "./list.md" "2022/01/09" "Care list")
                      (ssg:ent "./ipfs.org" "2022/04/05" "IPFS")
                      )

             (ssg:dir "philosophy"
                      (ssg:ent "./links.md" "2020/09/07" "Links")
                      (ssg:ent "./donating.org" "2022/02/01" "Donating")
                      (ssg:ent "./butterfly_effect.org" "2022/02/17" "Butterfly Effect")
                      (ssg:ent wip "./descartes.discourse_of_a_method_for_the_well_guiding_of_reason_and_the_discovery_of_truth_in_the_sciences.md" "" "_A Discourse of a Method for the Well Guiding of Reason and the Discovery of Truth in the Sciences_, by Descartes")
                      (ssg:ent wip "./descartes.meditations_on_first_philosophy.md" "" "_Meditations on First Philosophy_, by Descartes")
                      (ssg:ent wip "./plato.alcibiades_2.md" "" "_Alcibiades II_, by Plato")
                      (ssg:ent wip "./identity.md" "2020/06/28" "Identity")
                      (ssg:ent wip "./utilitarianism.md" "" "Utilitarianism")
                      (ssg:ent wip "./hard_determinism.md" "" "Hard Determinism")
                      )

             (ssg:dir "crypto"
                      (ssg:ent "./double_transposition.org" "2022/02/13" "Double Transposition Cipher")
                      )

             (ssg:dir "words"
                      (ssg:ent "./slpod.md" "2017/03/04" "SLPOD - a simple, suckless podcatcher (idea)")
                      (ssg:ent "./quotes.md" "2020/05/26" "just quotes")
                      )

             (ssg:dir "psychology"
                      (ssg:ent "./links.md" "2020/09/07" "Links")
                      (ssg:ent "./fagot.org" "2022/05/05" "Fagot")
                      )

             (ssg:dir "ssg"
                      (ssg:ent "./page.md" "2017/03/06" "simple static site generator (idea)")
                      (ssg:ent wip "./lib.md" "2019/10/18" "SSG, the Library")
                      (ssg:ent wip "./site.md" "2018/01/01" "SSG, Your own, personal, Static Site Generator")
                      )

             (ssg:dir "work"
                      (ssg:ent "./credit.md" "2020/04/30" "Credit")
                      (ssg:ent "./CVE-2020-26935.md" "2020/11/06" "CVE-2020-26935")
                      )

             (ssg:dir wip "todo"
                      (ssg:ent "./sbn.md" "2019/10/01" "Small Big Numbers")
                      )
             )
    )
  )

(define converter-table (ssg:make-converter-table ("md" "html" ssg:pandoc:md->html)
                                                  ("org" "html" (ssg:pandoc:-> "org" "html"))))
(define css (ssg:css-file "assets/monokai.css"))
(define-constant header "header.html")
(define-constant footer "footer.html")
(define (index-maker . args)
  (define update-css-key
    (match-lambda
      (() '())

      ((k v . rest)
       (if (and (keyword? k) (eq? k #:css))
         `(,k ,(ssg:css-file "assets/index.css") . ,rest)
         (cons k (update-css-key (cons v rest)))))))

  (apply ssg:lowdown:idx->html (update-css-key args)))

(define feed (ssg:feed-options
               #:authors "siiky"
               #:id "https://siiky.github.io"
               #:path feed-output-path
               #:type 'atom))

(define force-redo? (->bool (member "--force-redo" (command-line-arguments))))
(define site
  (ssg:site
    #:converter-table converter-table
    #:css css
    #:feed feed
    #:force-redo? force-redo?
    #:index index
    #:index-maker index-maker
    #:metafile "siiky.github.io.meta.scm"
    #:sxml-custom-rules (make-sxml-custom-rules footer)
    ))

(define (build site)
  (ssg:pandoc:append-default-extra-options! '("--mathml"))
  (ssg:pandoc:append-default-extra-options! `("-B" ,header))
  (ssg:pandoc:append-default-extra-options! `("-A" ,footer))
  (ssg:build site))

(define list-files
  (o (cute for-each print <>)
     ;(cute map ssg:idx-file-input-filename <>)
     (cute map ssg:feed-entry-path <>)
     ssg:index-entries-for-feed
     ssg:site-index
     (cute ssg:handle-result <> identity error)))

(define command (car (command-line-arguments)))

((alist-ref
   command
   `(("build" . ,build)
     ("list-files" . ,list-files))
   string=?
   (lambda _ (error "Command must be either build or list-files" (command-line-arguments))))
 site)
