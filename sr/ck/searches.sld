(define-library (sr ck searches)

  (export $span)

  (import (scheme base)
          (sr ck)
          (sr ck kernel))

  (begin

    (define-syntax $span
      (syntax-rules (quote)
        ((_ s 'pred 'list)                ($ s ($span 'pred 'list '())))
        ((_ s 'pred '()      'head)       ($ s '(head ())))
        ((_ s 'pred '(a . d) '(head ...)) ($ s ($if ($call 'pred 'a)
                                                   '($span 'pred 'd '(head ... a))
                                                   ''((head ...) (a . d)) ))) ) )

) )
