#lang racket

;; Trading DSL Grammar
;; This file contains a formal BNF description of our trading DSL grammar

#|
Grammar of Trading DSL:

<strategy-definition> ::= (define/strategy ID STRATEGY-EXPR
                            #:from DATE-STRING
                            #:to DATE-STRING)

<combined-strategy> ::= (define/combined-strategy ID 
                           STRATEGY-ID
                           STRATEGY-ID
                           #:mid DATE-STRING)

<backtest> ::= (backtest STRATEGY-ID
                         DATE-STRING
                         DATE-STRING
                         TOP-N)

<strategy-expr> ::= (top-performer #:period PERIOD)
                  | (compose-strategies STRATEGY-EXPR STRATEGY-EXPR
                     [#:weights (NUMBER NUMBER)])

<period> ::= 1y | 6m | 3m | 1m | 2w | 1w | 5d | 1d

Example usage:

1. Define momentum strategy:
(define/strategy annual-momentum 
  (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

2. Combine strategies with switchover date:
(define/combined-strategy combined-strategy
  annual-momentum
  biannual-momentum
  #:mid "2024-06-03")

3. Compose strategies with weights:
(define/strategy weighted-strategy
  (compose-strategies annual-momentum biannual-momentum
                     #:weights (0.7 0.3))
  #:from "2024-01-05"
  #:to "2024-12-20")

4. Backtest a strategy:
(backtest combined-strategy 
         "2024-01-05"
         "2024-12-20" 
         5)
|#

(provide)