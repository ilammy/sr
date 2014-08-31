(define-library (sr ck zippers)

  (export $make-zipper $zipper->list
          $zipper-ref  $zipper-set
          $zipper-fwd  $zipper-fwd?
          $zipper-bwd  $zipper-bwd?)

  (import (scheme base)
          (sr ck)
          (sr ck lists))

  (begin

    (define-syntax $make-zipper
      (syntax-rules (quote zipper)
        ((_ s '(x . xs)) ($ s '#(zipper () x xs))) ) )

    (define-syntax $zipper->list
      (syntax-rules (quote zipper)
        ((_ s '#(zipper << v >>))
         ($ s ($append ($reverse ($cons 'v '<<)) '>>))) ) )

    (define-syntax $zipper-ref
      (syntax-rules (quote zipper)
        ((_ s '#(zipper << v >>)) ($ s 'v)) ) )

    (define-syntax $zipper-set
      (syntax-rules (quote zipper)
        ((_ s '#(zipper << v >>) 'w)
         ($ s '#(zipper << w >>))) ) )

    (define-syntax $zipper-fwd
      (syntax-rules (quote zipper)
        ((_ s '#(zipper < << (v . >>)))
         ($ s '#(zipper (<< . <) v >>))) ) )

    (define-syntax $zipper-bwd
      (syntax-rules (quote zipper)
        ((_ s '#(zipper (v . <<) > >>))
         ($ s '#(zipper << v (> . >>)))) ) )

    (define-syntax $zipper-fwd?
      (syntax-rules (quote zipper)
        ((_ s '#(zipper << v ())) ($ s '#f))
        ((_ s '#(zipper << v >>)) ($ s '#t)) ) )

    (define-syntax $zipper-bwd?
      (syntax-rules (quote zipper)
        ((_ s '#(zipper () v >>)) ($ s '#f))
        ((_ s '#(zipper << v >>)) ($ s '#t)) ) )

) )
