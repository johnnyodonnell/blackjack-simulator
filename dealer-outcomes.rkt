#lang racket

(define num-of-simulations 10000)

(define outcomes (vector 'bust 17 18 19 20 21))

(define outcome->string
  (lambda (outcome)
    (cond
      [(symbol? outcome) (symbol->string outcome)]
      [else (number->string outcome)])))

(define run-simulation
  (lambda ()
    'bust))

(define run-simulations
  (lambda (outcomes-table [i 0])
    (cond
      [(>= i num-of-simulations) (displayln "Simulations complete...")]
      [else
        (begin
          (let ([outcome (run-simulation)])
            (hash-set! outcomes-table
                       outcome
                       (add1 (hash-ref outcomes-table outcome))))
          (run-simulations outcomes-table (add1 i)))])))

(define print-results
  (lambda (outcomes-table)
    (for-each
      (lambda (outcome)
        (displayln
          (string-append
            (outcome->string outcome)
            "\t"
            (number->string
              (hash-ref outcomes-table outcome))
            "\t"
            (number->string
              (exact->inexact
                (/ (hash-ref outcomes-table outcome)
                   num-of-simulations))))))
      (vector->list outcomes))))

(define main
  (lambda ()
    (let ([outcomes-table
            (make-hash
              (build-list
                (vector-length outcomes)
                (lambda (i)
                  (cons (vector-ref outcomes i) 0))))])
      (displayln "Running simulations...")
      (run-simulations outcomes-table)
      (print-results outcomes-table))))

(main)

