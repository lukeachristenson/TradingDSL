#lang racket

;; Example of comparing strategies with detailed allocation differences

(require TradingDSL)

;; Define strategies with different time horizons
(define/strategy annual-momentum 
  (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

(define/strategy monthly-momentum 
  (top-performer #:period 1m)
  #:from "2024-01-05"
  #:to "2024-06-05")

;; Also define a weighted combination strategy
(define/strategy weighted-strategy
  (compose-strategies annual-momentum monthly-momentum
                     #:weights (0.7 0.3))
  #:from "2024-01-05"
  #:to "2024-06-05")

(displayln "\n=== Strategy Difference Analysis ===")

;; Display the top stocks for the annual momentum strategy
(displayln "\nAnnual Momentum Strategy Top 5 Stocks:")
(display-strategy-allocation annual-momentum (reduced-date 2024 1 16) 5)

;; Display the top stocks for the monthly momentum strategy
(displayln "\nMonthly Momentum Strategy Top 5 Stocks:")
(display-strategy-allocation monthly-momentum (reduced-date 2024 1 16) 5)

;; Show the detailed differences between annual and monthly strategies
(displayln "\nDetailed comparison between Annual and Monthly strategies:")
(display-allocation-difference 
  annual-momentum 
  monthly-momentum
  "Annual" 
  "Monthly"
  (reduced-date 2024 1 16)
  10)

;; Show the differences between the weighted strategy and its components
(displayln "\nWeighted Strategy vs Annual Strategy:")
(display-allocation-difference
  weighted-strategy
  annual-momentum
  "Weighted"
  "Annual"
  (reduced-date 2024 1 16)
  5)

(displayln "\n=== Analysis Complete ===\n")