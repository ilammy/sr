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

(define-test-case (ck-list-maps:map "CK mapping functions for lists: $map")

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
