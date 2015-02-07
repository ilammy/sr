(import (scheme base)
        (sr assert internal)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (msg-split "$split-by-msg")

  (define-test ("Empty list")
    (assert-equal '(() #f)
      ($ ($quote ($split-by-msg '()))) ) )

  (define-test ("No msg")
    (assert-equal '((1 2 3) #f)
      ($ ($quote ($split-by-msg '(1 2 3)))) ) )

  (define-test ("Only msg")
    (assert-equal '(() (1 2 3))
      ($ ($quote ($split-by-msg '(msg: 1 2 3)))) ) )

  (define-test ("Full content")
    (assert-equal '((1 2 3) (4 5 6 7))
      ($ ($quote ($split-by-msg '(1 2 3 msg: 4 5 6 7)))) ) )

  (define-test ("Empty msg")
    (assert-equal '((1 2) ())
      ($ ($quote ($split-by-msg '(1 2 msg:)))) ) )
)
(verify-test-case! msg-split)
