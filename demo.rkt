#lang racket

(require "trading-dsl.rkt")

;; =================================================
;; 1. Individual Strategies
;; =================================================

;; Annual momentum strategy (top performers over 1 year)
(define/strategy annual-momentum (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

;; 6-month momentum strategy
(define/strategy biannual-momentum (top-performer #:period 6m)
  #:from "2024-06-01"
  #:to "2024-12-20")

;; 1-month momentum strategy
(define/strategy monthly-momentum (top-performer #:period 1m)
  #:from "2024-01-05"
  #:to "2024-12-20")

;; 2-week momentum strategy
(define/strategy short-term-momentum (top-performer #:period 2w)
  #:from "2024-01-05"
  #:to "2024-12-20")

;; =================================================
;; 2. Time-Based Strategy Combination
;; =================================================

;; Combined strategy with mid-point switchover
(define/combined-strategy seasonal-strategy
  annual-momentum      ;; Start with annual
  biannual-momentum    ;; Switch to biannual
  #:mid "2024-06-03")  ;; Switch date



;; =================================================
;; 3. Demonstrate Weighted Strategy Composition 
;; =================================================

;; Compose strategies with weights
(define/strategy weighted-long-term
  (compose-strategies annual-momentum biannual-momentum
                     #:weights (0.7 0.3))
  #:from "2024-01-05"
  #:to "2024-06-05")

;; Compose strategies with different weights
(define/strategy weighted-short-term
  (compose-strategies monthly-momentum short-term-momentum
                     #:weights (0.3 0.7))
  #:from "2024-01-05"
  #:to "2024-12-20")

;; =================================================
;; 4. Backtesting
;; =================================================
(displayln "\n=== Backtesting Results ===")

;; Backtest the annual momentum strategy
(displayln "Annual Momentum Strategy:")
(displayln (backtest annual-momentum 
                     "2024-01-05"
                     "2024-06-05" 
                     5))

;; Backtest the combined strategy
(displayln "\nSeasonal Strategy:")
(displayln (backtest seasonal-strategy
                     "2024-01-05"
                     "2024-12-20" 
                     5))

;; Backtest weighted strategy
(displayln "\nWeighted Long-Term Strategy:")
(displayln (backtest weighted-long-term
                     "2024-01-05"
                     "2024-06-05" 
                     5))

;; =================================================
;; 5. Strategy Display
;; =================================================
(displayln "\n=== Strategy Analysis ===")

;; Display allocation for annual momentum strategy
(displayln "\nPortfolio Allocation for Annual Momentum Strategy:")
(display-strategy-allocation annual-momentum (reduced-date 2024 1 16) 10)

;; Compare strategies 
(displayln "\nStrategy Comparison:")
(display-strategy-comparison 
  (list annual-momentum weighted-long-term monthly-momentum short-term-momentum)
  (list "Annual" "Weighted Long-Term" "Monthly" "Short-Term")
  (reduced-date 2024 1 16)
  5)

;; =================================================
;; 6. Error Handling
;; =================================================

#|
;; This would fail with a compile-time error:
;; Invalid date range (end date before start date)
(define/strategy invalid-dates-strategy
  (top-performer #:period 1y)
  #:from "2024-06-05"
  #:to "2024-01-05")

;; This would fail with a compile-time error:
;; Non-overlapping strategy periods
(define/strategy strategy-a
  (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-03-05")

(define/strategy strategy-b
  (top-performer #:period 1y)
  #:from "2024-04-05"
  #:to "2024-06-05")

;; Would fail: no overlap between strategy periods
(define/strategy invalid-composition
  (compose-strategies strategy-a strategy-b)
  #:from "2024-01-05"
  #:to "2024-06-05")
 
;; This would fail with a compile-time error:
;; Backtesting outside strategy's active period
(backtest annual-momentum
          "2023-01-05"  ;; Before strategy's active period
          "2024-02-05"
          5)
|#


#| DSL GRAMMAR

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


|#