
#| 
 | has-value? :: Connector -> Bool
 |
 | True when the connector is bound to a value
 |#
(define (has-value? connector)
  (connector 'has-value?))

#|
 | get-value :: Connector -> Any
 | 
 | Retrieves the value bound to the connector. Gets initialized to `#f`
 |#
(define (get-value connector)
  (connector 'value))

#|
 | set-value! :: (Connector, Any, Atom) -> 'done
 | 
 | Mutator that updates the internal state of the passed-in Connector procedure. The 
 | third argument is an atom that identifies what constraint made this change
 |# 
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

#|
 | forget-value! :: (Connector, Atom) -> 'done
 | 
 | Mutator that updates the internal state of the passed-in Connector procedure. The
 | second argument is an atom that identifies what constraint made this change
 |# 
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

#|
 | connect :: (Connector, Constraint) -> 'done
 | 
 | Mutator that updates the internal state of the passed-in Connector procedure
 |# 
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

#|
 | for-each-except :: (Constraint, Proc, Constraints) -> unit
 |
 | Notifies the Constraints in the list except for the first Constraint of the 
 | first argument. Used internally by `set-my-value` and `forget-my-value`
 |#
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

#|
 | make-connector :: unit -> (request -> )
 |
 | A Connector is represented as a procedure with local state variables `value`,
 | the current value of the Connector; `informant`, the object that set the
 | Connector's value; and `constraints`, a list of constraints in which the connector participates.
 |#
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    #|
     | set-my-value :: internal procedure called when there's a request to set the Connector's value.
     | Notifies constraints that the value has been set, except the one that set the value to begin with.
     |#
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    #|
     | forget-my-value :: internal procedure called when there's a request to unset the Connector's value.
     | if the retractor is not the same as the one who set the constraint, then the request is ignored.
     | Otherwise, it notifies its constraints (except for the `retractor`) of the change.
     |#
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    #|
     | connect :: Adds a constraint to the list associated with this Connector
     |#
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (when (has-value? me)
          (inform-about-value new-constraint))
      'done)
    #|
     | The API of the Connector. Based on the first argument atom, the appropriate procedure is invoked.
     |#
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation - CONNECTOR"
                         request))))
    me))