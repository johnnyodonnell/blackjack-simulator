#lang racket

(require java-array-list)
(require commatize-number)


(define num-of-simulations 1000000)

(define outcomes (vector 17 18 19 20 21 'bust))

(define outcome->string
  (lambda (outcome)
    (cond
      [(symbol? outcome) (symbol->string outcome)]
      [else (number->string outcome)])))

(define card%
  (class* object%
          ()
          (super-new)
          (init-field vals)
          (define/public get-values (lambda () vals))))

(define cards
  (vector
    (make-object card% '(11 1)) ; Ace
    (make-object card% '(2)) ; Two
    (make-object card% '(3)) ; Three
    (make-object card% '(4)) ; Four
    (make-object card% '(5)) ; Five
    (make-object card% '(6)) ; Six
    (make-object card% '(7)) ; Seven
    (make-object card% '(8)) ; Eight
    (make-object card% '(9)) ; Nine
    (make-object card% '(10)) ; Ten
    (make-object card% '(10)) ; Jack
    (make-object card% '(10)) ; Queen
    (make-object card% '(10)) ; King
    ))

(define draw-card
  (lambda ()
    (vector-ref cards
                (random (vector-length cards)))))

(define score-hand
  (lambda (hand [i 0] [curr-score 0])
    (cond
      [(or (> curr-score 21)
           (>= i (send hand size))) curr-score]
      [else
        (let* ([card (send hand get i)]
               [vals (send card get-values)])
          (foldl
            (lambda (val best-score)
              (cond
                [(and (> best-score 0)
                      (<= best-score 21)) best-score]
                [else (score-hand hand (add1 i) (+ curr-score val))]))
            0
            vals))])))

(define draw-again?
  (lambda (hand)
    (< (score-hand hand) 17)))

(define run-simulation
  (lambda ([hand (make-object array-list%)])
    (cond
      [(draw-again? hand)
       (begin
         (send hand add (draw-card))
         (run-simulation hand))]
      [else
        (let ([outcome (score-hand hand)])
          (cond
            [(> outcome 21) 'bust]
            [else outcome]))])))

(define run-simulations
  (lambda (outcomes-table [i 0])
    (cond
      [(>= i num-of-simulations) (void)]
      [else
        (begin
          (let ([outcome (run-simulation)])
            (hash-set! outcomes-table
                       outcome
                       (add1 (hash-ref outcomes-table outcome))))
          (run-simulations outcomes-table (add1 i)))])))

(define print-results
  (lambda (outcomes-table)
    (displayln "Outcome\tCount\tPct.")
    (displayln "---\t---\t---")
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
      (displayln
        (string-append
          "Running "
          (commatize num-of-simulations)
          " simulations...\n"))
      (run-simulations outcomes-table)
      (print-results outcomes-table))))

(main)

