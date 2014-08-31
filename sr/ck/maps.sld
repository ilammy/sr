(define-library (sr ck maps)

  (export $map $fold $fold-right)

  (import (scheme base)
          (sr ck)
          (sr ck kernel)
          (sr ck lists))

  (begin

    (define-syntax $fold
      (syntax-rules (quote)
        ((_ s 'cons 'nil '()     ) ($ s 'nil))
        ((_ s 'cons 'nil '(a . d)) ($ s ($fold 'cons ($call 'cons 'a 'nil) 'd))) ) )

    (define-syntax $fold-right
      (syntax-rules (quote)
        ((_ s 'cons 'nil '()     ) ($ s 'nil))
        ((_ s 'cons 'nil '(a . d)) ($ s ($call 'cons 'a ($fold-right 'cons 'nil 'd)))) ) )

    (define-syntax $map-cons
      (syntax-rules (quote)
        ((_ s 'proc 'x 'xs) ($ s ($attach 'xs ($call 'proc 'x)))) ) )

    (define-syntax $map
      (syntax-rules (quote)
        ((_ s 'proc 'list) ($ s ($fold '($map-cons 'proc) '() 'list))) ) )

) )
