#lang racket

;; Example for visualizing trading strategies

(require TradingDSL)

;; Define simple strategies using the DSL
(define/strategy annual-momentum (top-performer #:period 1y)
  #:from "2024-01-05"
  #:to "2024-06-05")

(define/strategy biannual-momentum (top-performer #:period 6m)
  #:from "2024-06-01"
  #:to "2024-12-20")

(define/combined-strategy comb1
  annual-momentum
  biannual-momentum 
  #:mid "2024-06-03") 

;; Define a combined strategy (70% annual, 30% biannual)
(define/strategy composed-strategy
  (compose-strategies annual-momentum biannual-momentum
                     #:weights (0.7 0.3))
  #:from "2024-01-05"
  #:to "2024-12-20")

;; Visualize a single strategy's performance
(displayln "Visualizing annual momentum strategy performance...")
(visualize-strategy-performance annual-momentum 
                                "2024-01-05"
                                "2024-06-05" 
                                5)

;; Visualize comparison between strategies
(displayln "Visualizing strategy comparison...")
(visualize-strategy-comparison (list annual-momentum composed-strategy)
                             (list "Annual Momentum" "Composed Strategy")
                             "2024-01-05"
                             "2024-06-05" 
                             5)

;; Visualize portfolio weights
(displayln "Visualizing portfolio weights...")
(visualize-portfolio-weights annual-momentum 
                            (reduced-date 2024 1 16)
                            10)

(displayln "Visualization examples completed.")