#lang racket

;; Example of comparing strategies with artificial difference

(require "./trading-dsl.rkt")

;; Create two strategies with different stock preferences
(define (custom-strat1 date)
  (list (ticker-weight "NVDA" 1.5)
        (ticker-weight "MSFT" 1.4)
        (ticker-weight "AAPL" 1.3)
        (ticker-weight "AMZN" 1.2)
        (ticker-weight "GOOGL" 1.1)))

(define (custom-strat2 date)
  (list (ticker-weight "META" 1.6)
        (ticker-weight "AMZN" 1.5)
        (ticker-weight "TSLA" 1.4)
        (ticker-weight "NVDA" 1.3)
        (ticker-weight "AAPL" 1.2)))

;; Define them as proper strategies
(define/strategy tech-value custom-strat1
  #:from "2024-01-05"
  #:to "2024-06-05")

(define/strategy growth-tech custom-strat2
  #:from "2024-01-05"
  #:to "2024-06-05")

;; Define a weighted composition of these strategies
(define/strategy balanced-tech
  (compose-strategies tech-value growth-tech
                      #:weights (0.6 0.4))
  #:from "2024-01-05"
  #:to "2024-06-05")

(displayln "\n=== Strategy Difference Analysis (Custom Strategies) ===")

;; Show the top stocks for each strategy
(displayln "\nTech Value Strategy:")
(display-strategy-allocation tech-value (reduced-date 2024 1 16) 5)

(displayln "\nGrowth Tech Strategy:")
(display-strategy-allocation growth-tech (reduced-date 2024 1 16) 5)

;; Show the detailed differences between the strategies
(displayln "\nDetailed comparison between Tech Value and Growth Tech:")
(display-allocation-difference 
  tech-value 
  growth-tech
  "Tech Value" 
  "Growth Tech"
  (reduced-date 2024 1 16)
  8)

;; Show the differences between the balanced strategy and its components
(displayln "\nBalanced Tech vs Tech Value:")
(display-allocation-difference
  balanced-tech
  tech-value
  "Balanced"
  "Tech Value"
  (reduced-date 2024 1 16)
  5)

(displayln "\nBalanced Tech vs Growth Tech:")
(display-allocation-difference
  balanced-tech
  growth-tech
  "Balanced"
  "Growth Tech"
  (reduced-date 2024 1 16)
  5)

(displayln "\n=== Analysis Complete ===\n")