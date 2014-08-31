(import (scheme base)
        (sr ck)
        (sr ck kernel)
        (sr ck lists)
        (sr ck predicates)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:cons "CK functions for lists: $cons")

  (define-test ("$cons pair")
    (assert-equal '(1 . 2)
      ($ ($quote ($cons '1 '2))) ) )

  (define-test ("$cons list")
    (assert-equal '(a b c)
      ($ ($quote ($cons 'a ($cons 'b ($cons 'c '()))))) ) )
)
(verify-test-case! ck-lists:cons)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:list "CK functions for lists: $list")

  (define-test ("$list one")
    (assert-equal '(1)
      ($ ($quote ($list '1))) ) )

  (define-test ("$list some")
    (assert-equal '(1 foo (3))
      ($ ($quote ($list '1 'foo ($list '3)))) ) )

  (define-test ("$list none")
    (assert-equal '()
      ($ ($quote ($list))) ) )
)
(verify-test-case! ck-lists:list)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:append "CK functons for lists: $append")

  (define-test ("$append simple")
    (assert-equal '(1 2 3 4 5)
      ($ ($quote ($append '(1 2 3) '(4 5)))) ) )

  (define-test ("$append to empty")
    (assert-equal '(1 2 3)
      ($ ($quote ($append '() '(1 2 3)))) ) )

  (define-test ("$append an empty")
    (assert-equal '(1 2 3)
      ($ ($quote ($append '(1 2 3) '()))) ) )

  (define-test ("$append two empties")
    (assert-equal '()
      ($ ($quote ($append '() '()))) ) )
)
(verify-test-case! ck-lists:append)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:attach "CK functons for lists: $attach")

  (define-test ("$attach one")
    (assert-equal '(1 2 3 4)
      ($ ($quote ($attach '(1 2 3) '4))) ) )

  (define-test ("$attach several")
    (assert-equal '(1 2 3 4 5 6)
      ($ ($quote ($attach '(1 2 3) '4 '5 '6))) ) )

  (define-test ("$attach none")
    (assert-equal '(1 2 3)
      ($ ($quote ($attach '(1 2 3)))) ) )

  (define-test ("$attach to an empty list")
    (assert-equal '(1 2)
      ($ ($quote ($attach '() '1 '2))) ) )
)
(verify-test-case! ck-lists:attach)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:map "CK functions for lists: $map")

  (define-test ("$map empty")
    (assert-equal '()
      ($ ($quote ($map '$symbol? '()))) ) )

  (define-test ("$map non-empty")
    (assert-equal '((1 2) (x y) (r))
      ($ ($quote ($map '$reverse '((2 1) (y x) (r))))) ) )

  (define-test ("$map partial")
    (assert-equal '((x 1) (x 2) (x 3))
      ($ ($quote ($map '($cons 'x) '((1) (2) (3))))) ) )
)
(verify-test-case! ck-lists:map)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:reverse "CK functions for lists: $reverse")

  (define-test ("$reverse empty")
    (assert-equal '()
      ($ ($quote ($reverse '()))) ) )

  (define-test ("$reverse one")
    (assert-equal '(1)
      ($ ($quote ($reverse '(1)))) ) )

  (define-test ("$reverse")
    (assert-equal '(x y z)
      ($ ($quote ($reverse '(z y x)))) ) )
)
(verify-test-case! ck-lists:reverse)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:span "CK functions for lists: $span")

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
(verify-test-case! ck-lists:span)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:filter "CK functions for lists: $filter")

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
(verify-test-case! ck-lists:filter)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:partition "CK functions for lists: partition")

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
(verify-test-case! ck-lists:partition)
