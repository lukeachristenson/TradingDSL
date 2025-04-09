#lang racket

;; Trading DSL - Macro Expansions
;; This file contains the macro expansions for the DSL forms

#|
Macro Expansion: define/strategy
-------------------------------

Source:
(define/strategy annual-momentum 
  (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

Expansion:
1. Compile-time:
   - Check that "2024-01-05" is before "2024-06-05"
   - Store the period information in symbol table

2. Runtime:
   (define annual-momentum (top-performer #:period 1y))


Macro Expansion: define/combined-strategy
---------------------------------------

Source:
(define/combined-strategy combined-strategy
  annual-momentum
  biannual-momentum
  #:mid "2024-06-03")

Expansion:
1. Compile-time:
   - Check that periods can be combined
   - Store combined period in symbol table

2. Runtime:
   (define combined-strategy
     (lambda (date)
       (if (date-before? date (string->date "2024-06-03"))
           (annual-momentum date)
           (biannual-momentum date))))


Macro Expansion: compose-strategies
---------------------------------

Source:
(compose-strategies annual-momentum biannual-momentum
                   #:weights (0.7 0.3))

Expansion:
(lambda (date)
  (let* ([s1-result (annual-momentum date)]
         [s2-result (biannual-momentum date)]
         [combined (compose-strategy-results 
                    s1-result s2-result 0.7 0.3)])
    combined))


Macro Expansion: backtest
-----------------------

Source:
(backtest combined-strategy 
         "2024-01-05"
         "2024-12-20" 
         5)

Expansion:
1. Compile-time:
   - Check that dates are valid
   - Check that period is within strategy's active period

2. Runtime:
   (run-backtest combined-strategy
                "2024-01-05"
                "2024-12-20"
                5)
|#

(provide)