(import (scheme base)
	      (sr ck)
        (sr ck kernel)
        (sr ck predicates)
        (sr ck zippers)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-zippers:conversion "CK list zippers: construction/destruction")

  (define-test ("zipper is not a list")
    (assert-false
      ($ ($list? ($make-zipper '(1 2 3)))) ) )

  (define-test ("no actions leave list intact")
    (assert-equal '(1 2 3)
      ($ ($quote ($zipper->list ($make-zipper '(1 2 3))))) ) )

  (define-test ("moving forward leaves list intact")
    (assert-equal '(1 2 3)
      ($ ($quote ($zipper->list
                   ($zipper-fwd ($make-zipper '(1 2 3))) ))) ) )

  (define-test ("moving backward leaves list intact")
    (assert-equal '(1 2 3)
      ($ ($quote ($zipper->list
                   ($zipper-bwd
                     ($zipper-fwd
                       ($zipper-fwd ($make-zipper '(1 2 3))) ) ) ))) ) )
)
(verify-test-case! ck-zippers:conversion)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-zippers:movement "CK list zippers: focus movement")

  (define-test ("Cannot move backward at start")
    (assert-false
      ($ ($zipper-bwd? ($make-zipper '(1 2 3)))) ) )

  (define-test ("Can move forward at start")
    (assert-true
      ($ ($zipper-fwd? ($make-zipper '(1 2 3)))) ) )

  (define-test ("Cannot move forward at end")
    (assert-false
      ($ ($zipper-fwd?
           ($zipper-fwd ($zipper-fwd ($make-zipper '(1 2 3)))) )) ) )

  (define-test ("Can move backward at end")
    (assert-true
      ($ ($zipper-bwd?
           ($zipper-fwd ($zipper-fwd ($make-zipper '(1 2 3)))) )) ) )

  (define-test ("Can move anywhere in the middle")
    (assert-true
      (and ($ ($zipper-fwd? ($zipper-fwd ($make-zipper '(1 2 3)))))
           ($ ($zipper-bwd? ($zipper-fwd ($make-zipper '(1 2 3))))) ) ) )

  (define-test ("Cannot move anywhere in one-element list")
    (assert-false
      (or ($ ($zipper-fwd? ($make-zipper '(x))))
          ($ ($zipper-bwd? ($make-zipper '(x)))) ) ) )
)
(verify-test-case! ck-zippers:movement)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-zippers:getters "CK list zippers: accessing values")

  (define-test ("Focused on the first element at start")
    (assert-equal '1
      ($ ($quote ($zipper-ref ($make-zipper '(1 2 3))))) ) )

  (define-test ("Moving focus does move focus")
    (assert-equal '2
      ($ ($quote ($zipper-ref ($zipper-fwd ($make-zipper '(1 2 3)))))) ) )

  (define-test ("No, really, it does")
    (assert-equal '3
      ($ ($quote ($zipper-ref
        ($zipper-bwd ($zipper-fwd ($zipper-fwd ($zipper-fwd
          ($make-zipper '(1 2 3 4 5)) )))) ))) ) )
)
(verify-test-case! ck-zippers:getters)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-zippers:setters "CK list zippers: modifying values")

  (define-test ("Set values appear in resulting list")
    (assert-equal '(1 2 9)
      ($ ($quote ($zipper->list
        ($zipper-set
          ($zipper-fwd ($zipper-fwd ($make-zipper '(1 2 3))))
          '9 ) ))) ) )

  (define-test ("Can set several values")
    (assert-equal '(-1 2 -3)
      ($ ($quote ($zipper->list
        ($zipper-set
          ($zipper-fwd ($zipper-fwd
            ($zipper-set ($make-zipper '(1 2 3)) '-1) ))
          '-3 ) ))) ) )

  (define-test ("Can set value several times")
    (assert-equal '(bar 2 3)
      ($ ($quote ($zipper->list
        ($zipper-set ($zipper-set
          ($make-zipper '(1 2 3))
          'foo ) 'bar) ))) ) )

  (define-test ("New values are visible to getters")
    (assert-equal 'foo
      ($ ($quote ($zipper-ref
        ($zipper-set ($make-zipper '(1 2 3)) 'foo) ))) ) )
)
(verify-test-case! ck-zippers:setters)
