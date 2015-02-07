(define-library (sr assert internal)
  ;;;
  ;;; Assertion implementation
  ;;;
  (export $assert
          $split-by-msg)

  (import (scheme base)
          (scheme write)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr assert output-port))

  (begin

    (define-syntax $assert
      (syntax-rules (quote)
        ((_ s '(clauses ...))
         ($ s ($assert:verify-syntax ($split-by-msg '(clauses ...)))))

        ((_ s _) (syntax-error "Invalid assertion syntax")) ) )

    (define-syntax $split-by-msg
      (syntax-rules (quote msg:)
        ((_ s 'clauses) ($ s ($split-by-msg '() '#f 'clauses)))

        ((_ s 'assertions 'messages '()) ($ s '(assertions messages)))

        ((_ s 'assertions '#f '(msg: . rest))
         ($ s ($split-by-msg 'assertions '() 'rest)))

        ((_ s 'assertions 'messages '(msg: . rest))
         (syntax-error "Invalid assertion syntax: multiple message clauses"))

        ((_ s '(assertions ...) '#f '(clause . rest))
         ($ s ($split-by-msg '(assertions ... clause) '#f 'rest)))

        ((_ s 'assertions '(messages ...) '(clause . rest))
         ($ s ($split-by-msg 'assertions '(messages ... clause) 'rest))) ) )

    (define-syntax $assert:verify-syntax
      (syntax-rules (quote)
        ((_ s '(() _))
         (syntax-error "Invalid assertion syntax: must have at least one assertion"))

        ((_ s '(_ ()))
         (syntax-error "Invalid assertion syntax: empty message clause"))

        ((_ s '(assertions messages))
         ($ s ($check-assertions 'assertions 'messages))) ) )

    (define-syntax $check-assertions
      (syntax-rules (quote)
        ((_ s 'assertions 'messages)
         (cond-expand
           (assertions-disabled ($ s '#f))
           (else
             ($ s ($cons 'begin
               ($map '($check-assertion 'messages) 'assertions) )) ) )) ) )

    (define-syntax $check-assertion
      (syntax-rules (quote)
        ((_ s 'messages 'assertion)
         ($ s ($append '(unless assertion)
           ($list ($print-failure-message 'assertion 'messages)
                  ($signal-failure-condition 'assertion) ) ))) ) )

    (define-syntax $print-failure-message
      (syntax-rules (quote)
        ((_ s 'failed-assertion 'messages)
         (cond-expand
           (assertions-silent ($ s '#f))
           (else
             ($ s ($append '(parameterize ((current-output-port (current-assertion-port))))
               ($failure-message 'failed-assertion 'messages) )) ) )) ) )

    (define-syntax $failure-message
      (syntax-rules (quote)
        ((_ s 'failed-assertion '#f)
         ($ s '((newline)
                (display "***** ASSERTION FAILED *****") (newline)
                (display "Assertion: ") (display 'failed-assertion) (newline))))

        ((_ s 'failed-assertion '(additional-messages ...))
         ($ s '((newline)
                (display "***** ASSERTION FAILED *****") (newline)
                (display "Assertion: ") (display 'failed-assertion) (newline)
                (display "  ")
                (begin
                  (display additional-messages) (display " ") ) ... (newline)))) ) )

    (define-syntax $signal-failure-condition
      (syntax-rules (quote)
        ((_ s 'failed-assertion)
         (cond-expand
           (assertions-nonfatal ($ s '#f))
           (else
             ($ s '(error "Failed assertion" 'failed-assertion)) ) )) ) )

) )
