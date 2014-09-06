(define-library (sr ck filters)

  (export $filter $partition $multi-partition)

  (import (scheme base)
          (sr ck)
          (sr ck kernel)
          (sr ck lists)
          (sr ck maps))

  (begin

    (define-syntax $filter-cons
      (syntax-rules (quote)
        ((_ s 'pred 'x 'xs) ($ s ($if ($call 'pred 'x)
                                     '($attach 'xs 'x)
                                     ''xs ))) ) )
    (define-syntax $filter
      (syntax-rules (quote)
        ((_ s 'pred 'list) ($ s ($fold '($filter-cons 'pred) '() 'list))) ) )

    (define-syntax $partition-cons
      (syntax-rules (quote)
        ((_ s 'pred 'x '(ts fs)) ($ s ($if ($call 'pred 'x)
                                          '($list ($attach 'ts 'x) 'fs)
                                          '($list 'ts ($attach 'fs 'x)) ))) ) )
    (define-syntax $partition
      (syntax-rules (quote)
        ((_ s 'pred 'list) ($ s ($fold '($partition-cons 'pred) '(() ()) 'list))) ) )

    (define-syntax $mp-cons
      (syntax-rules (quote)
        ((_ s 'pred '(rest . done))
         ($ s ($mp-cons 'pred 'done ($partition 'pred 'rest))))
        ((_ s 'pred 'done '(true false))
         ($ s '(false true . done))) ) )

    (define-syntax $multi-partition
      (syntax-rules (quote)
        ((_ s 'preds 'list) ($ s ($reverse ($fold '$mp-cons ($list 'list) 'preds)))) ) )

) )
