#lang racket

;; Import the trading DSL
(require "trading-dsl.rkt")

;; Define simple strategies using the DSL
(define-strategy annual-momentum (top-performer #:period 1y))
(define-strategy biannual-momentum (top-performer #:period 6m))

;; Define a combined strategy (70% annual, 30% biannual)
(define-strategy combined-strategy
  (compose-strategies annual-momentum biannual-momentum 
                     #:weights (0.7 0.3)))

;; Print some results
(displayln "Top 5 stocks from annual momentum strategy:")
(displayln (take (annual-momentum (reduced-date 2024 1 16)) 5))

;; Run a backtest
(displayln "\nBacktest results:")
(displayln (backtest combined-strategy
           #:from (reduced-date 2023 1 5)
           #:to (reduced-date 2023 12 20)
           #:top-n 5))