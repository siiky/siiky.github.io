(import
  (only srfi-1 assoc)
  (prefix ssg |ssg:|)
  (prefix ssg.css |ssg:|)
  (prefix ssg.index |ssg:|)
  (prefix ssg.md2html |ssg:|)
  (prefix ssg.site |ssg:|)
  )

; http://www.more-magic.net/docs/scheme/sxslt.pdf
(define (make-sxml-custom-rules)
  (define (page _ title css . content)
    (let ((css (ssg:css-content css)))
      `(html (@ (lang "en"))
             (head ,css ; it seems #f is ignored
                   (meta (@ (charset "UTF-8")))
                   (title ,title))
             (body ,content)
             (footer
               "\nplaces:\n"
               (a (@ (href "https://siiky.github.io")) "Go home!") "\n"
               (a (@ (href "https://github.com/siiky")) "GitHub")))))

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
    (let* ((attrs (cdr attrs))
           (src (cadr (assoc 'src attrs eq?)))
           (alt (cdr  (assoc 'alt attrs eq?)))
           (alt (string-concatenate
                  `("![" ,@(or alt '("")) "](" ,@(or src '("")) ")"))))
      `(img (@ (src ,src) (alt ,alt)))))
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
  (ssg:idx "nothing interesting here"
           (ssg:dir "algebra"
                    (ssg:ent "./groups.md" "" "A quick intro to Group Theory")
                    (ssg:ent "./cat_theory_perf.md" "2019/10/15 22:00" "Category Theory for performance optimization")
                    )

           (ssg:dir "scheme"
                    (ssg:ent "./exceptions.md" "2019/08/13 20:40" "Exceptions in Scheme")
                    (ssg:ent "./kless.md" "2019/10/13 23:00" "kless")
                    )

           (ssg:dir "server_stuffs"
                    (ssg:ent "./seamless_updates.md" "2020/03/13 23:30" "Seamless Updates")
                    )

           (ssg:dir "ssg"
                    (ssg:ent "./page.md" "2017/03/06 15:30" "simple static site generator (idea)")
                    (ssg:ent 'wip "./lib.md" "2019/10/18 23:00" "SSG, the Library")
                    (ssg:ent 'wip "./site.md" "" "SSG, Your own, personal, Static Site Generator")
                    )

           (ssg:dir "words"
                    (ssg:ent "./slpod.md" "2017/03/04 22:00" "SLPOD - a simple, suckless podcatcher (idea)")
                    (ssg:ent "./quotes.md" "2020/05/26 17:00" "just quotes")
                    )

           (ssg:dir "philosophy"
                    (ssg:ent 'wip "descartes.discourse_of_a_method_for_the_well_guiding_of_reason_and_the_discovery_of_truth_in_the_sciences.md" "" "_A Discourse of a Method for the Well Guiding of Reason and the Discovery of Truth in the Sciences_, by Descartes")
                    (ssg:ent 'wip "descartes.meditations_on_first_philosophy.md" "" "_Meditations on First Philosophy_, by Descartes")
                    (ssg:ent 'wip "plato.alcibiades_2.md" "" "_Alcibiades II_, by Plato")
                    (ssg:ent 'wip "identity.md" "2020/06/28" "Identity")
                    )

           (ssg:dir 'wip "work"
                    (ssg:ent "./credit.md" "2020/04/30" "Credit")
                    (ssg:ent "./to_do_or_not_to_do.md" "2020/04/30" "To do or not to do... That is the Question")
                    )

           (ssg:dir 'wip "functional_programming"
                    (ssg:ent "./immutability.md" "2020/04/10 18:30" "Immutability")
                    )

           (ssg:dir 'wip "todo"
                    (ssg:ent "./sbn.md" "2019/10/01" "Small Big Numbers")
                    (ssg:ent "./gv-dsl.md" "2019/10/01" "GraphViz Scheme DSL")
                    )
           )
  )

(define converter-table (ssg:make-converter-table ("md" "html" ssg:md->html)))
(define css (ssg:css-file "assets/monokai.css"))
(define index-maker ssg:idx->html)

(ssg:ssg
  (ssg:site
    #:converter-table converter-table
    #:css css
    #:index index
    #:index-maker index-maker
    #:sxml-custom-rules (make-sxml-custom-rules)
    )
  )
