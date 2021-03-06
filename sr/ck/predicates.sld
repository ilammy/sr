(define-library (sr ck predicates)

  (export $symbol? $bool? $list? $same?
          $or $and
          $every? $any?)

  (import (scheme base)
          (sr ck)
          (sr ck kernel))

  (begin

    (define-syntax ?symbol
      (syntax-rules ()
        ((_  (a . b) t f) f)
        ((_ #(x ...) t f) f)
        ((_ a-symbol t f)
          (let-syntax
            ((? (syntax-rules ()
                  ((_ a-symbol  tt ff) tt)
                  ((_ else      tt ff) ff))))
            (? ! t f)))))

    (define-syntax $symbol?
      (syntax-rules (quote)
        ((_ s 'x) (?symbol x ($ s '#t) ($ s '#f))) ) )

    (define-syntax $bool?
      (syntax-rules (quote)
        ((_ s '#t) ($ s '#t))
        ((_ s '#f) ($ s '#t))
        ((_ s '::) ($ s '#f)) ) )

    (define-syntax $list?
      (syntax-rules (quote)
        ((_ s '())      ($ s '#t))
        ((_ s '(a . d)) ($ s ($list? 'd)))
        ((_ s 'other)   ($ s '#f)) ) )

    (define-syntax ?id-eq
      (syntax-rules ()
        ((_ a b t f)
         (let-syntax
           ((a (syntax-rules () ((a) f)))
            (c (syntax-rules () ((c) t))))
           (let-syntax
             ((? (syntax-rules () ((? b) (a)))))
             (? c) ) )) ) )

    (define-syntax ?same
      (syntax-rules ()
        ((_ (a . aa) (b . bb) t f) (?same a b (?same aa bb t f) f))
        ((_ #(a aa ...) #(b bb ...) t f) (?same a b (?same #(aa ...) #(bb ...) t f) f))
        ((_ a b t f)
         (?symbol a (?symbol b (?id-eq a b t f) f)
           (let-syntax ((? (syntax-rules ()
                             ((_ a tt ff) tt)
                             ((_ c tt ff) ff) )))
             (? b t f) ) ) ) ) )

    (define-syntax $same?
      (syntax-rules (quote)
        ((_ s 'a 'b) (?same a b ($ s '#t) ($ s '#f))) ) )

    (define-syntax $or
      (syntax-rules (quote)
        ((_ s 'expr)            ($ s ($eval 'expr)))
        ((_ s 'expr 'other ...) ($ s ($if ($eval 'expr)
                                         ''#t
                                         '($or 'other ...) ))) ) )
    (define-syntax $and
      (syntax-rules (quote)
        ((_ s 'expr)            ($ s ($eval 'expr)))
        ((_ s 'expr 'other ...) ($ s ($if ($eval 'expr)
                                         '($and 'other ...)
                                         ''#f ))) ) )
    (define-syntax $every?
      (syntax-rules (quote)
        ((_ s 'pred '())         ($ s '#t))
        ((_ s 'pred '(x xs ...)) ($ s ($and '($call 'pred 'x)
                                            '($every? 'pred '(xs ...)) ))) ) )

    (define-syntax $any?
      (syntax-rules (quote)
        ((_ s 'pred '())         ($ s '#f))
        ((_ s 'pred '(x xs ...)) ($ s ($or '($call 'pred 'x)
                                           '($any? 'pred '(xs ...)) ))) ) )
) )
