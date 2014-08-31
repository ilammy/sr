(import (scheme base)
        (sr ck)
        (sr ck kernel)
        (sr ck lists)
        (sr ck maps)
        (sr ck predicates)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-list-maps:map "CK list mapping and folding: $map")

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
(verify-test-case! ck-list-maps:map)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-list-maps:fold "CK list mapping and folding: $fold")

  (define-test ("folding empty list returns nil")
    (assert-equal 'nil
      ($ ($quote ($fold '$cons 'nil '()))) ) )

  (define-test ("folding non-empty list produces a tail-recursion")
    (assert-equal
      ($ ($quote ($fold '$cons '(1)       '(2 . (3)))))
      ($ ($quote ($fold '$cons ($cons '2 '(1)) '(3)))) ) )

  (define-test ("together this gives an example: $reverse implementation")
    (assert-equal '(3 2 1)
      ($ ($quote ($fold '$cons '() '(1 2 3)))) ) )
)
(verify-test-case! ck-list-maps:fold)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (ck-list-maps:fold-right "CK list mapping and folding: $fold-right")

  (define-test ("$fold-right is $fold done right")
    (assert-equal '(1 2 3)
      ($ ($quote ($fold-right '$cons '() '(1 2 3)))) ) )

  (define-test ("$fold-right can handle empty list too")
    (assert-equal 'nil
      ($ ($quote ($fold-right '$cons 'nil '()))) ) )

  (define-test ("it's pretty much a weird $map")
    (define-syntax $append-cons
      (syntax-rules (quote)
        ((_ s 'v 'a 'd) ($ s ($cons '(a . v) 'd))) ) )

    (assert-equal '((1 . x) (2 . x) (3 . x) . end)
      ($ ($quote ($fold-right '($append-cons 'x) 'end '(1 2 3)))) ) )
)
(verify-test-case! ck-list-maps:fold-right)
