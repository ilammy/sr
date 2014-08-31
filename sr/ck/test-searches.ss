(import (scheme base)
        (sr ck)
        (sr ck kernel)
        (sr ck searches)
        (sr ck predicates)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-list-searches:span "CK search functions for lists: $span")

  (define-test ("$span empty")
    (assert-equal '(() ())
      ($ ($quote ($span '$symbol? '()))) ) )

  (define-test ("$span normal")
    (assert-equal '((a b c) (1 2 d e))
      ($ ($quote ($span '$symbol? '(a b c 1 2 d e)))) ) )

  (define-test ("$span no first")
    (assert-equal '(() (1 2 3 4))
      ($ ($quote ($span '$symbol? '(1 2 3 4)))) ) )

  (define-test ("$span no second")
    (assert-equal '((a b c) ())
      ($ ($quote ($span '$symbol? '(a b c)))) ) )
)
(verify-test-case! ck-list-searches:span)
