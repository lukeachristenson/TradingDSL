#lang racket

;; Import the trading DSL
(require "./trading-dsl.rkt")

;; Define simple strategies
(define/strategy annual-momentum (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

;; Test backtest
(displayln "\nBacktest results:") 
(displayln (backtest annual-momentum 
                     "2024-01-05"
                     "2024-06-05" 
                     5))

(displayln "Example completed successfully!")