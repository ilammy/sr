(define-library (sr assert)
  ;;;
  ;;; Assertion macro for checking invariants
  ;;;
  ;;; SYNTAX
  ;;;
  ;;;     (assert <test-expression>+ [msg: <message-expression>+]?)
  ;;;
  ;;; SEMANTICS
  ;;;
  ;;;     <test-expressions> are sequentially evaluated. If each and every
  ;;;     expression yields a true value then assertion passes and nothing
  ;;;     happens. Otherwise an error is signaled.
  ;;;
  ;;;     If the assertion fails and the "msg:" part is specified,
  ;;;     <message-expressions> are evaluated and printed out together
  ;;;     with the failing subassertion.
  ;;;
  ;;;     The value of an assertion expression is unspecified.
  ;;;
  ;;; EXAMPLE
  ;;;
  ;;;     (assert (= 4 (+ 2 2)))
  ;;;
  ;;;     (assert (eqv? 9 9) (eq? 'foo 'foo))
  ;;;
  ;;;     (assert (bar args)
  ;;;        msg: "bar is not holding" args)
  ;;;
  ;;; CONFIGURATION
  ;;;
  ;;;     Parameters:
  ;;;
  ;;;       - (current-assertion-port)
  ;;;
  ;;;         Failure messages are printed out into this port.
  ;;;         Defaults to (current-error-port) if not specified.
  ;;;
  ;;;     Features:
  ;;;
  ;;;       - assertions-silent
  ;;;
  ;;;         Failure messages will not be printed in case of failures.
  ;;;
  ;;;       - assertions-nonfatal
  ;;;
  ;;;         Error conditions will not be signaled in case of failures.
  ;;;
  ;;;       - assertions-disabled
  ;;;
  ;;;         Assertion expressions will not be evaluated and checked.
  ;;;
  (export assert current-assertion-port)

  (import (scheme base)
          (sr ck)
          (sr assert internal)
          (rename (only (sr assert output-port) *current-assertion-port*)
            (*current-assertion-port* current-assertion-port) ))

  (begin

    (define-syntax assert
      (syntax-rules ()
        ((_ . expressions) ($ ($assert 'expressions))) ) )

) )
