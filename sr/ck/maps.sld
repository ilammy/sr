(define-library (sr ck maps)

  (export $map)

  (import (scheme base)
          (sr ck)
          (sr ck kernel))

  (begin

    (define-syntax $map
      (syntax-rules (quote)
        ((_ s 'proc 'list)                ($ s ($map 'proc 'list '())))
        ((_ s 'proc '()      'result)     ($ s 'result))
        ((_ s 'proc '(a . d) 'result)     ($ s ($map 'proc 'd 'result ($call 'proc 'a))))
        ((_ s 'proc 'd '(result ...) 'aa) ($ s ($map 'proc 'd '(result ... aa)))) ) )

) )
