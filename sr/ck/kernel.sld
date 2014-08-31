(define-library (sr ck kernel)

  (export $quote $eval $call $apply $if)

  (import (scheme base)
          (sr ck))

  (begin

    (define-syntax $quote
      (syntax-rules (quote)
        ((_ s 'x) ($ s ''x)) ) )

    (define-syntax $eval
      (syntax-rules (quote)
        ((_ s 'x) ($ s x)) ) )

    (define-syntax $call
      (syntax-rules (quote)
        ; need to know the internals of $
        ((_ s '(f ...) 'args ...) ($ s #f (f ... 'args ...)))
        ((_ s 'f       'args ...) ($ s ($call '(f) 'args ...))) ) )

    (define-syntax $apply
      (syntax-rules (quote)
        ((_ s 'f '(args ...)) ($ s ($call 'f 'args ...))) ) )

    (define-syntax $if
      (syntax-rules (quote)
        ((_ s '#t 't 'f) ($ s ($eval 't)))
        ((_ s '#f 't 'f) ($ s ($eval 'f))) ) )

) )
