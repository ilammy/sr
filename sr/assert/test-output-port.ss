(import (scheme base)
        (sr assert output-port)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (assert-output-port "Assertion output port tests")
  ;;
  ;; The behavior of equivalence predicates is not defined for ports so
  ;; there is limited amount of things that we can test in a portable way
  ;;
  (define (assert-port port)
    (assert-true (output-port? port))
    (assert-true (textual-port? port))
    (assert-true (output-port-open? port)) )

  (define-test ("Actual default value")
    (assert-false (*current-assertion-port*)) )

  (define-test ("Visible default value")
    (assert-port (current-assertion-port)) )

  (define-test ("Actual port is parameterizable")
    (call-with-port (open-output-string)
      (lambda (port)
        (parameterize ((*current-assertion-port* port))
          (assert-port (*current-assertion-port*)) ) ) ) )

  (define-test ("Visible port is parameterizable")
    (call-with-port (open-output-string)
      (lambda (port)
        (parameterize ((*current-assertion-port* port))
          (assert-port (current-assertion-port)) ) ) ) )

  #;; This test may fail as textual and binary ports may not be distinct
  ;;; so that textual-port? may return #t for a binary port.
  (define-test ("Binary ports are not valid")
    (call-with-port (open-output-bytevector)
      (lambda (port)
        (assert-raises error-object?
          (lambda ()
            (parameterize ((*current-assertion-port* port))
              (current-assertion-port) ) ) ) ) ) )

  (define-test ("Input ports are not valid")
    (call-with-port (open-input-string "123")
      (lambda (port)
        (assert-raises error-object?
          (lambda ()
            (parameterize ((*current-assertion-port* port))
              (current-assertion-port) ) ) ) ) ) )

  (define-test ("Closed ports are not valid")
    (call-with-port (open-output-string)
      (lambda (port)
        (assert-raises error-object?
          (lambda ()
            (close-port port)
            (parameterize ((*current-assertion-port* port))
              (current-assertion-port) ) ) ) ) ) )

  (define-test ("Non-ports are not valid")
    (assert-raises error-object?
      (lambda ()
        (parameterize ((*current-assertion-port* "port"))
          (current-assertion-port) ) ) ) )

  (define-test ("#f is valid though")
    (parameterize ((*current-assertion-port* #f))
      (assert-port (current-assertion-port)) ) )
)
(verify-test-case! assert-output-port)
