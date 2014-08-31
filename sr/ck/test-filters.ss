(import (scheme base)
        (sr ck)
        (sr ck kernel)
        (sr ck filters)
        (sr ck predicates)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-list-filters:filter "CK filter functions for lists: $filter")

  (define-test ("$filter empty")
    (assert-equal '()
      ($ ($quote ($filter '$symbol? '()))) ) )

  (define-test ("$filter 1")
    (assert-equal '((1) (2) (3))
      ($ ($quote ($filter '$list? '((1) a (2) #f (3) "9")))) ) )

  (define-test ("$filter 2")
    (assert-equal '(#t #f)
      ($ ($quote ($filter '$bool? '(#t a (#t) #f)))) ) )

  (define-test ("$filter 3")
    (assert-equal '(1 1 1)
      ($ ($quote ($filter '($same? '1) '(1 2 3 2 1 2 3 4 1)))) ) )
)
(verify-test-case! ck-list-filters:filter)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-list-filters:partition "CK filter functions for lists: $partition")

  (define-test ("empty list")
    (assert-equal '(()())
      ($ ($quote ($partition '$bool? '()))) ) )

  (define-test ("only true")
    (assert-equal '((#t #t #f) ())
      ($ ($quote ($partition '$bool? '(#t #t #f)))) ) )

  (define-test ("only false")
    (assert-equal '(() (3 8 x #t))
      ($ ($quote ($partition '$list? '(3 8 x #t)))) ) )

  (define-test ("partial")
    (assert-equal '((1 1 1) (a b c d))
      ($ ($quote ($partition '($same? '1) '(a 1 b 1 c 1 d)))) ) )
)
(verify-test-case! ck-list-filters:partition)
