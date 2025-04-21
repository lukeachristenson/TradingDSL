#lang racket

(require "../functionDSL.rkt")

;; =================================================
;; 1. Individual Strategies
;; =================================================

;; Annual momentum strategy (top performers over 1 year)
(define annual-momentum 
  (strategy (top-performer #:period 1y)
            #:from "2024-01-05"
            #:to "2024-06-05"))

;; 6-month momentum strategy
(define biannual-momentum 
  (strategy (top-performer #:period 6m)
            #:from "2024-06-01"
            #:to "2024-12-20"))

;; 1-month momentum strategy
(define monthly-momentum 
  (strategy (top-performer #:period 1m)
            #:from "2024-01-05"
            #:to "2024-12-20"))

;; 2-week momentum strategy
(define short-term-momentum 
  (strategy (top-performer #:period 2w)
            #:from "2024-01-05"
            #:to "2024-12-20"))

;; =================================================
;; 2. Time-Based Strategy Combination
;; =================================================

;; Combined strategy with mid-point switchover
(define seasonal-strategy
  (combined-strategy annual-momentum
                     biannual-momentum
                     #:mid "2024-06-03"))

;; =================================================
;; 3. Weighted Strategy Composition 
;; =================================================

;; Compose strategies with weights
(define weighted-long-term
  (compose-strategies-fn annual-momentum 
                         biannual-momentum
                         #:weights '(0.7 0.3)))

;; Compose strategies with different weights
(define weighted-short-term
  (compose-strategies-fn monthly-momentum 
                         short-term-momentum
                         #:weights '(0.3 0.7)))

;; =================================================
;; 4. Backtesting
;; =================================================
(displayln "\n=== Backtesting Results ===")

;; Backtest the annual momentum strategy
(displayln "Annual Momentum Strategy:")
(displayln (backtest-fn annual-momentum 
                        "2024-01-05"
                        "2024-06-05" 
                        5))

;; Backtest the combined strategy
(displayln "\nSeasonal Strategy:")
(displayln (backtest-fn seasonal-strategy
                        "2024-01-05"
                        "2024-12-20" 
                        5))

;; Backtest weighted strategy
(displayln "\nWeighted Long-Term Strategy:")
(displayln (backtest-fn weighted-long-term
                        "2024-01-05"
                        "2024-06-05" 
                        5))

;; =================================================
;; 5. Strategy Display
;; =================================================
(displayln "\n=== Strategy Analysis ===")

;; Display allocation for annual momentum strategy
(displayln "\nPortfolio Allocation for Annual Momentum Strategy:")
(display-strategy-allocation (strategy-spec-strategy-fn annual-momentum) 
                            (reduced-date 2024 1 16) 
                            10)

;; Compare strategies 
(displayln "\nOptimized Strategy:")
(display-strategy-comparison 
  (list (strategy-spec-strategy-fn annual-momentum) 
        (strategy-spec-strategy-fn weighted-long-term) 
        (strategy-spec-strategy-fn monthly-momentum) 
        (strategy-spec-strategy-fn short-term-momentum))
  (list "Annual" "Weighted Long-Term" "Monthly" "Short-Term")
  (reduced-date 2024 1 16)
  5)

;; Custom strategy functions
(define (tech-heavy date)
  (list (ticker-weight "NVDA" 2.5)
        (ticker-weight "MSFT" 1.8)
        (ticker-weight "AAPL" 1.6)
        (ticker-weight "GOOGL" 1.4)
        (ticker-weight "AMZN" 1.2)
        (ticker-weight "META" 0.8)
        (ticker-weight "TSLA" 0.6)))

(define (value-focused date)
  (list (ticker-weight "META" 2.2)
        (ticker-weight "AMZN" 1.9)
        (ticker-weight "MSFT" 1.5)
        (ticker-weight "AAPL" 1.3)
        (ticker-weight "TSLA" 1.1)
        (ticker-weight "GOOGL" 0.9)
        (ticker-weight "NVDA" 0.7)))

;; Define strategies with custom functions
(define tech-strategy 
  (strategy tech-heavy
            #:from "2024-01-05"
            #:to "2024-12-20"))

(define value-strategy 
  (strategy value-focused
            #:from "2024-01-05"
            #:to "2024-12-20"))

;; Display allocation differences
(displayln "\n Allocation Differences:")
(display-allocation-difference
  (strategy-spec-strategy-fn tech-strategy)
  (strategy-spec-strategy-fn value-strategy)
  "Tech Strategy"
  "Value Strategy"
  (reduced-date 2024 1 16)
  5)

;; =================================================
;; Error Handling Examples
;; =================================================

#|
;; These would raise runtime errors:

;; Invalid date range (end date before start date)
(define invalid-dates-strategy
  (strategy (top-performer #:period 1y)
            #:from "2024-06-05"
            #:to "2024-01-05"))

;; Non-overlapping strategy periods
(define strategy-a
  (strategy (top-performer #:period 1y)
            #:from "2024-01-05"
            #:to "2024-03-05"))

(define strategy-b
  (strategy (top-performer #:period 1y)
            #:from "2024-04-05"
            #:to "2024-06-05"))

;; This would fail: no overlap between strategy periods
(define invalid-composition
  (compose-strategies-fn strategy-a strategy-b))

;; This would fail: backtesting outside strategy's active period
(backtest-fn annual-momentum
            "2023-01-05"  ;; Before strategy's active period
            "2024-02-05"
            5)
|#