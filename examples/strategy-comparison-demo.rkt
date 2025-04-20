#lang racket

;; Trading DSL Strategy Comparison Demo
;; This demonstrates meaningful differences between strategies

(require "./trading-dsl.rkt")

;; Create strategies with deliberately different preferences
(define (tech-heavy date)
  (list (ticker-weight "NVDA" 2.5)    ; Higher weight on NVDA
        (ticker-weight "MSFT" 1.8)
        (ticker-weight "AAPL" 1.6)
        (ticker-weight "GOOGL" 1.4)
        (ticker-weight "AMZN" 1.2)
        (ticker-weight "META" 0.8)    ; Lower weight on META
        (ticker-weight "TSLA" 0.6)))

(define (value-focused date)
  (list (ticker-weight "META" 2.2)    ; Higher weight on META
        (ticker-weight "AMZN" 1.9)
        (ticker-weight "MSFT" 1.5)
        (ticker-weight "AAPL" 1.3)
        (ticker-weight "TSLA" 1.1)
        (ticker-weight "GOOGL" 0.9)
        (ticker-weight "NVDA" 0.7)))  ; Lower weight on NVDA

;; Register them as proper strategies
(define/strategy tech-strategy tech-heavy
  #:from "2024-01-05"
  #:to "2024-12-20")

(define/strategy value-strategy value-focused
  #:from "2024-01-05"
  #:to "2024-12-20")

;; Create a balanced strategy with both
(define/strategy balanced
  (compose-strategies tech-strategy value-strategy
                     #:weights (0.6 0.4))
  #:from "2024-01-05"
  #:to "2024-12-20")

;; Update our README example to use these more differentiated strategies
(displayln "\n=== Strategy Comparison Demo ===\n")

;; Show the top stocks for the tech-focused strategy
(displayln "Tech Strategy Top 5 Stocks:")
(display-strategy-allocation tech-strategy (reduced-date 2024 1 16) 5)

;; Show the top stocks for the value-focused strategy 
(displayln "\nValue Strategy Top 5 Stocks:")
(display-strategy-allocation value-strategy (reduced-date 2024 1 16) 5)

;; Show the basic strategy comparison
(displayln "\nBasic Strategy Comparison:")
(display-strategy-comparison 
  (list tech-strategy value-strategy balanced)
  (list "Tech" "Value" "Balanced")
  (reduced-date 2024 1 16)
  1)

;; Show the detailed differences between tech and value strategies
(displayln "\nDetailed Comparison - Tech vs Value:")
(display-allocation-difference 
  tech-strategy 
  value-strategy
  "Tech"
  "Value"
  (reduced-date 2024 1 16)
  7)

;; Show how the balanced strategy compares to the tech strategy
(displayln "\nDetailed Comparison - Balanced vs Tech:")
(display-allocation-difference
  balanced
  tech-strategy
  "Balanced"
  "Tech"
  (reduced-date 2024 1 16)
  5)

;; Show how the balanced strategy compares to the value strategy
(displayln "\nDetailed Comparison - Balanced vs Value:")
(display-allocation-difference
  balanced
  value-strategy
  "Balanced"
  "Value"
  (reduced-date 2024 1 16)
  5)

(displayln "\nDemo completed successfully!")