(import (scheme base)
        (sr ck)
        (sr ck kernel)
        (sr ck lists)
        (sr ck predicates)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-lists:cons "CK functions for lists: $cons, $car, $cdr")

  (define-test ("$cons pair")
    (assert-equal '(1 . 2)
      ($ ($quote ($cons '1 '2))) ) )

  (define-test ("$cons list")
    (assert-equal '(a b c)
      ($ ($quote ($cons 'a ($cons 'b ($cons 'c '()))))) ) )

  (define-test ("$car of a pair")
    (assert-equal '1
      ($ ($quote ($car ($cons '1 '2)))) ) )

  (define-test ("$cdr of a pair")
    (assert-equal '2
      ($ ($quote ($cdr ($cons '1 '2)))) ) )

  (define-test ("$car of a list")
    (assert-equal 'a
      ($ ($quote ($car '(a b c)))) ) )

  (define-test ("$cdr of a list")
    (assert-equal '(b c)
      ($ ($quote ($cdr '(a b c)))) ) )
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

  (define-test ("$append none")
    (assert-equal '()
      ($ ($quote ($append))) ) )

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

  (define-test ("$append several lists")
    (assert-equal '(1 2 3 4 5 6)
      ($ ($quote ($append '(1 2) '(3 4 5) '() '(6)))) ) )
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

(define-test-case (ck-lists:concatenate "CK functions for lists: $concatenate")

  (define-test ("$concatenate empty")
    (assert-equal '()
      ($ ($quote ($concatenate '()))) ) )

  (define-test ("$concatenate one")
    (assert-equal '(1)
      ($ ($quote ($concatenate '((1))))) ) )

  (define-test ("$concatenate more")
    (assert-equal '(x y z)
      ($ ($quote ($concatenate '((x) (y z) ())))) ) )
)
(verify-test-case! ck-lists:concatenate)
