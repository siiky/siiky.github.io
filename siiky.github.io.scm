(import ssg)

(define css-file "assets/monokai.css")
(define directories
  `(
    "algebra/"
    "scheme/"
    "ssg/"
    "todo/"
    "words/"
    )
  )

(define index
  (idx "nothing interesting here"
       (dir "algebra"
            (ent "./groups.md"
                 #:create-date ""
                 #:title "A quick intro to Group Theory")
            (ent "./cat_theory_perf.md"
                 #:create-date "2019/10/15 22:00"
                 #:title "Category Theory for performance optimization")
            )

       (dir "scheme"
            (ent "./exceptions.md"
                 #:create-date "2019/08/13 20:40"
                 #:title "Exceptions in Scheme")
            (ent "./kless.md"
                 #:create-date "2019/10/13 23:00"
                 #:title "kless")
            )

       (dir "ssg"
            (ent "./page.md"
                 #:create-date "2017/03/06 15:30"
                 #:title "simple static site generator (idea)")
            (ent "./lib.md"
                 #:create-date "2019/10/18 23:00"
                 #:title "SSG, the Library"
                 #:wip? #t)
            (ent "./site.md"
                 #:create-date ""
                 #:title "SSG, Your own, personal, Static Site Generator"
                 #:wip? #t)
            )

       (dir "words"
            (ent "./slpod.md"
                 #:create-date "2017/03/04 22:00"
                 #:title "SLPOD - a simple, suckless podcatcher (idea)")
            )

       (dir "todo"
            (ent "./sbn.md"
                 #:create-date ""
                 #:title "Small Big Numbers"
                 #:wip? #t)
            (ent "./gv-dsl.md"
                 #:create-date ""
                 #:title "GraphViz Scheme DSL"
                 #:wip? #t)
            )
       ))

(ssg #:index index #:css-file css-file)
