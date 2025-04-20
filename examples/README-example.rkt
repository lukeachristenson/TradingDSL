#lang racket

;; Trading DSL Example
;; This file demonstrates the basics of using the Trading DSL

(require "./trading-dsl.rkt")

(displayln "\n=== Trading DSL Example ===\n")

;; 1. Define a simple momentum strategy for the first half of 2024
(displayln "Defining annual momentum strategy...")
(define/strategy annual-momentum (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

;; 2. Define a different strategy for the second half of 2024
(displayln "Defining biannual momentum strategy...")
(define/strategy biannual-momentum (top-performer #:period 6m)
  #:from "2024-06-01"
  #:to "2024-12-20")

;; 3. Combine strategies with time-based switching
(displayln "\nCombining strategies with a mid-point switchover...")
(define/combined-strategy seasonal-strategy
  annual-momentum
  biannual-momentum
  #:mid "2024-06-03")

;; 4. Compose strategies with weighted allocation
(displayln "Creating weighted composition (70% annual, 30% biannual)...")
(define/strategy weighted-strategy
  (compose-strategies annual-momentum biannual-momentum
                     #:weights (0.7 0.3))
  #:from "2024-01-05"
  #:to "2024-06-05")

;; 5. Show the top stocks from the annual momentum strategy
(displayln "\nTop 5 stocks from annual momentum strategy:")
(displayln (take (annual-momentum (reduced-date 2024 1 16)) 5))

;; 6. Backtest the strategies
(displayln "\nBacktesting annual momentum strategy:")
(displayln (backtest annual-momentum 
                     "2024-01-05"
                     "2024-06-05" 
                     5))

(displayln "\nBacktesting combined seasonal strategy:")
(displayln (backtest seasonal-strategy
                     "2024-01-15"
                     "2024-07-15" 
                     5))

(displayln "\nBacktesting weighted composition strategy:")
(displayln (backtest weighted-strategy
                     "2024-01-05"
                     "2024-06-05" 
                     5))

;; 7. Display allocations for one of the strategies
(displayln "")
(display-strategy-allocation annual-momentum (reduced-date 2024 1 16) 5)

;; 8. Compare multiple strategies
(displayln "")
(display-strategy-comparison 
  (list annual-momentum weighted-strategy)
  (list "Annual Momentum" "Weighted Strategy")
  (reduced-date 2024 1 16)
  5)

;; 9. Create contrasting strategies for better comparison
(displayln "\nCreating contrasting tech and value strategies...")

;; Define strategies with different preferences
(define (tech-heavy date)
  (list (ticker-weight "NVDA" 2.5)
        (ticker-weight "MSFT" 1.8)
        (ticker-weight "AAPL" 1.6)
        (ticker-weight "GOOGL" 1.4)
        (ticker-weight "AMZN" 1.2)))

(define (value-focused date)
  (list (ticker-weight "META" 2.2)
        (ticker-weight "AMZN" 1.9)
        (ticker-weight "MSFT" 1.5)
        (ticker-weight "AAPL" 1.3)
        (ticker-weight "TSLA" 1.1)))

(define/strategy tech-strategy tech-heavy
  #:from "2024-01-05"
  #:to "2024-06-05")

(define/strategy value-strategy value-focused
  #:from "2024-01-05"
  #:to "2024-06-05")

;; Show detailed allocation differences
(displayln "\nDetailed allocation differences between Tech and Value strategies:")
(display-allocation-difference
  tech-strategy
  value-strategy
  "Tech"
  "Value"
  (reduced-date 2024 1 16)
  5)

(displayln "\n=== Example Completed Successfully ===\n")