#lang racket

;; Import the trading DSL
(require "trading-dsl.rkt")

;; Define simple strategies using the DSL
(define/strategy annual-momentum (top-performer #:period 1y)
  #:from "2023-01-05"
  #:to "2023-12-20")

(define/strategy biannual-momentum (top-performer #:period 6m)
   #:from "2023-01-05"
   #:to "2023-12-20")

(define/combined-strategy comb1
  annual-momentum
  biannual-momentum 
   #:mid "2023-06-05") 

;; Define a combined strategy (70% annual, 30% biannual)
(define/strategy combined-strategy
  (compose-strategies annual-momentum biannual-momentum
                     #:weights (0.7 0.3))
  #:from "2023-01-05"
  #:to "2023-12-20")
 
(displayln "Top 5 stocks from annual momentum strategy:")
(displayln (take (annual-momentum (reduced-date 2024 1 16)) 5)) 
 
;; Run a backtest 
(displayln "\nBacktest results:") 
(displayln (backtest combined-strategy 
                     "2023-01-05"
                     "2023-12-20" 
                     5))


(displayln "\nBacktest results:") 
(displayln (backtest comb1 
                     "2023-01-05"
                     "2023-12-20" 
                     5)) 

