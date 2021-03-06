(define-library (sr ck lists)

  (export $cons $car $cdr $list $append $attach $reverse $concatenate)

  (import (scheme base)
          (sr ck)
          (sr ck kernel))

  (begin

    (define-syntax $cons
      (syntax-rules (quote)
        ((_ s 'a 'd) ($ s '(a . d))) ) )

    (define-syntax $car
      (syntax-rules (quote)
        ((_ s '(a . d)) ($ s 'a)) ) )

    (define-syntax $cdr
      (syntax-rules (quote)
        ((_ s '(a . d)) ($ s 'd)) ) )

    (define-syntax $list
      (syntax-rules (quote)
        ((_ s 'xs ...) ($ s '(xs ...))) ) )

    (define-syntax $append
      (syntax-rules (quote)
        ((_ s) ($ s '()))
        ((_ s '(xs ...)) ($ s '(xs ...)))
        ((_ s '(xs ...) '(ys ...) 'other ...)
         ($ s ($append '(xs ... ys ...) 'other ...))) ) )

    (define-syntax $attach
      (syntax-rules (quote)
        ((_ s '(xs ...) 'ys ...) ($ s '(xs ... ys ...))) ) )

    (define-syntax $reverse
      (syntax-rules (quote)
        ((_ s 'list)            ($ s ($reverse 'list '())))
        ((_ s '() 'result)      ($ s 'result))
        ((_ s '(a . d) 'result) ($ s ($reverse 'd ($cons 'a 'result)))) ) )

    (define-syntax $concatenate
      (syntax-rules (quote)
        ((_ s 'lists) ($ s ($apply '$append 'lists))) ) )

) )
