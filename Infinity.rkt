;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

(define-struct varexp (name))
(define-struct appexp (fn args))
(define-struct fn-and-lof (fnname lof visited?))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A Variable is a Symbol.

#|make-def
make-varexp
make-appexp|#

#|any-loops? : Program -> Boolean
GIVEN: a valid SGS program p (that is, a GS program that obeys the
restrictions listed above).
RETURNS: true iff there is some function f in p that calls itself
either directly or indirectly, as in the example above.|#

(define (any-loops? prog)
  (cond
    [(empty? prog) #false]
    [else (handle-graphs(cp-into-graph prog) (cp-into-graph prog))]))
(define (check p)
  p)
(define (handle-graphs g g1)
  (cond
    [(empty? g) false]
    [else (or (check-cycle? (first g) (toggle-visited g1)) (handle-graphs (rest g) g1))]))
(define (toggle-visited g)
  (cond
    [(empty? g) '()]
    [else (cons (change (first g)) (toggle-visited (rest g)))]))

(define (change e)
  (make-fn-and-lof (fn-and-lof-fnname e) (fn-and-lof-lof e) #false))

(define (check-cycle? e g)
  (if (equal? (fn-and-lof-visited? e) #true)
      #true
       (if (member (fn-and-lof-fnname e) (fn-and-lof-lof e))
           #true
           (search-lof (fn-and-lof-lof  e) (update-ele-visited e g)))))

(define (update-ele-visited e g)
  (cond
    [(empty? g) '()]
    [(equal?(first g) e) (cons (update-true (first g)) (update-ele-visited e (rest g)))] 
    [else (cons (first g) (update-ele-visited  e (rest g)))]))

(define (update-true e)
  (make-fn-and-lof (fn-and-lof-fnname e) (fn-and-lof-lof e) #true))
 
(define (mark-visited e g)
  (make-fn-and-lof (fn-and-lof-fnname e) (fn-and-lof-lof e) #true))
      
(define (search-lof  c g)
  (cond
    [(empty? c) #false]
    [else (or(check-cycle? (call-of (first c) g) g) (search-lof (rest c) g))]))

(define (call-of c g)
  (cond
  [(empty? g) #false]
  [(equal? (fn-and-lof-fnname (first g)) c) (first g)]
  [else (call-of c (rest g))]))
  
          
(define (cp-into-graph prog)
  (map convert prog))

(define (convert d)
  (make-fn-and-lof (def-name d)(if (not(appexp? (def-body d))) '()
                                              (extract-calls (def-body d)))false))

(define (extract-calls b)
  (cons (appexp-fn b) (handle-list (appexp-args b))))

(define (handle-list loa)
  (cond
    [(empty? loa) '()]
    [(appexp? (first loa)) (append (extract-calls (first loa)) (handle-list (rest loa)))]
    [else (handle-list (rest loa))]))
