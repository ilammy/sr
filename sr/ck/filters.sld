(define-library (sr ck filters)

  (export $filter $partition)

  (import (scheme base)
          (sr ck)
          (sr ck kernel))

  (begin

    (define-syntax $filter
      (syntax-rules (quote)
        ((_ s 'pred 'list)                  ($ s ($filter 'pred 'list '())))
        ((_ s 'pred '()      'result)       ($ s 'result))
        ((_ s 'pred '(a . d) '(result ...)) ($ s ($filter 'pred 'd
                                                   ($if ($call 'pred 'a)
                                                       ''(result ... a)
                                                       ''(result ...) ) ))) ) )
    (define-syntax $partition
      (syntax-rules (quote)
        ((_ s 'pred 'list)     ($ s ($partition 'pred 'list '() '())))
        ((_ s 'pred '() 't 'f) ($ s '(t f)))
        ((_ s 'pred '(a . d) '(t ...) '(f ...))
         ($ s ($if ($call 'pred 'a)
                  '($partition 'pred 'd '(t ... a) '(f ...))
                  '($partition 'pred 'd '(t ...) '(f ... a)) )) ) ) )

) )
