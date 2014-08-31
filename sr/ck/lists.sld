(define-library (sr ck lists)

  (export $cons $list $append $attach $map $reverse $span $filter $partition)

  (import (scheme base)
          (sr ck)
          (sr ck kernel)
          (sr ck predicates))

  (begin

    (define-syntax $cons
      (syntax-rules (quote)
        ((_ s 'a 'd) ($ s '(a . d))) ) )

    (define-syntax $list
      (syntax-rules (quote)
        ((_ s 'xs ...) ($ s '(xs ...))) ) )

    (define-syntax $append
      (syntax-rules (quote)
        ((_ s '(xs ...) '(ys ...)) ($ s '(xs ... ys ...))) ) )

    (define-syntax $attach
      (syntax-rules (quote)
        ((_ s '(xs ...) 'ys ...) ($ s '(xs ... ys ...))) ) )

    (define-syntax $map
      (syntax-rules (quote)
        ((_ s 'proc 'list)                ($ s ($map 'proc 'list '())))
        ((_ s 'proc '()      'result)     ($ s 'result))
        ((_ s 'proc '(a . d) 'result)     ($ s ($map 'proc 'd 'result ($call 'proc 'a))))
        ((_ s 'proc 'd '(result ...) 'aa) ($ s ($map 'proc 'd '(result ... aa)))) ) )

    (define-syntax $reverse
      (syntax-rules (quote)
        ((_ s 'list)            ($ s ($reverse 'list '())))
        ((_ s '() 'result)      ($ s 'result))
        ((_ s '(a . d) 'result) ($ s ($reverse 'd ($cons 'a 'result)))) ) )

    (define-syntax $span
      (syntax-rules (quote)
        ((_ s 'pred 'list)                ($ s ($span 'pred 'list '())))
        ((_ s 'pred '()      'head)       ($ s '(head ())))
        ((_ s 'pred '(a . d) '(head ...)) ($ s ($if ($call 'pred 'a)
                                                   '($span 'pred 'd '(head ... a))
                                                   ''((head ...) (a . d)) ))) ) )
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
