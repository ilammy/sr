(define-library (sr assert output-port)
  ;;;
  ;;; Assertion output port
  ;;;
  (export current-assertion-port
          *current-assertion-port*)

  (import (scheme base))

  (begin

    (define (valid-assertion-port? port)
      (and (output-port? port)
           (textual-port? port)
           (output-port-open? port) ) )

    (define *current-assertion-port*
      (make-parameter #f
        (lambda (port)
          (if (and port (not (valid-assertion-port? port)))
              (error "Assertion port must be an open textual output port")
              port ) ) ) )

    (define (current-assertion-port)
      (let ((port (*current-assertion-port*)))
        (if port port
            (current-error-port) ) ) )

) )
