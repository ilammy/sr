(import (scheme base)
        (sr ck)
        (sr ck kernel)
        (sr ck predicates)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-kernel:quote "Kernel CK functions: $quote")

  (define-test ("$quote simple value")
    (assert-equal 'foobar
      ($ ($quote 'foobar)) ) )

  (define-test ("$quote special forms")
    (assert-equal '(begin failing)
      ($ ($quote '(begin failing))) ) )

  (define-test ("$quote $quote")
    (assert-equal '$quote
      ($ ($quote '$quote)) ) )
)
(verify-test-case! ck-kernel:quote)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-kernel:if "Kernel CK functions: $if")

  (define-test ("$if <- #t")
    (assert-equal 1
      ($ ($if '#t ''1 ''2)) ) )

  (define-test ("$if <- #f")
    (assert-equal 'foo
      ($ ($if '#f ''1 '($quote 'foo))) ) )

  (define-test ("$if <- expr")
    (assert-equal 1
      ($ ($if ($symbol? 'foo) ''1 ''2)) ) )

  (define-test ("$if <- $if")
    (assert-equal #f
      ($ ($if ($if '#t ''#f ''#t) ''#t ''#f)) ) )
)
(verify-test-case! ck-kernel:if)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-kernel:eval "Kernel CK functions: $eval")

  (define-test ("$eval values")
    (assert-equal '(1 2 3)
      ($ ($quote ($eval ''(1 2 3)))) ) )

  (define-test ("$eval calls 1")
    (assert-equal #t
      ($ ($eval '($symbol? 'foo))) ) )

  (define-test ("$eval calls 2")
    (assert-equal 2
      ($ ($eval '($if '#f ''1 ''2))) ) )

  (define-test ("$eval self")
    (assert-equal 'bar
      ($ ($quote
        ($eval '($eval '($eval
         '($if '#f ''1 '($eval ''bar)) ))) )) ) )
)
(verify-test-case! ck-kernel:eval)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-kernel:apply "Kernel CK functions: $apply")

  (define-test ("$apply simple 1")
    (assert-equal '#t
      ($ ($quote ($apply '$list? '((1 2 3))))) ) )

  (define-test ("$apply simple 2")
    (assert-equal '#f
      ($ ($quote ($apply '$same? '(1 2)))) ) )

  (define-test ("$apply no args")
    (define-syntax $true
      (syntax-rules (quote)
        ((_ s) ($ s '#t)) ) )
    (assert-equal '#t
      ($ ($quote ($apply '$true '()))) ) )

  (define-test ("$apply partial")
    (assert-equal '#t
      ($ ($quote ($apply '($same? 'foo) '(foo)))) ) )

  (define-test ("$apply self")
    (assert-equal '#t
      ($ ($quote ($apply '$apply '($bool? (#f))))) ) )

  (define-test ("$apply eval")
    (assert-equal '1
      ($ ($quote ($apply '$eval '(($if '#t ''1 ''2))))) ) )
)
(verify-test-case! ck-kernel:apply)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-kernel:call "Kernel CK functions: $call")

  (define-test ("$call simple 1")
    (assert-equal '#t
      ($ ($quote ($call '$list? '(1 2 3)))) ) )

  (define-test ("$call simple 2")
    (assert-equal '#f
      ($ ($quote ($call '$same? '1 '2))) ) )

  (define-test ("$call no args")
    (define-syntax $true
      (syntax-rules (quote)
        ((_ s) ($ s '#t)) ) )
    (assert-equal '#t
      ($ ($quote ($call '$true))) ) )

  (define-test ("$call partial")
    (assert-equal '#t
      ($ ($quote ($call '($same? 'foo) 'foo))) ) )

  (define-test ("$call self")
    (assert-equal '#t
      ($ ($quote ($call '$call '$bool? '#f))) ) )
)
(verify-test-case! ck-kernel:call)
