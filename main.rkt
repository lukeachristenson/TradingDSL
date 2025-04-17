#lang racket

;; Trading Strategy DSL - Main module
;; Provides both macro-based and function-based implementations

(require (only-in "trading-dsl.rkt"
                 define/strategy
                 define/combined-strategy
                 compose-strategies
                 backtest
                 top-performer
                 reduced-date
                 ticker-weight
                 ticker-weight-ticker
                 ticker-weight-weight
                 display-strategy-allocation
                 display-strategy-comparison
                 display-allocation-difference
                 1y 6m 3m 1m 2w 1w 5d 1d))

(require (only-in "function-dsl.rkt"
                 strategy
                 combined-strategy
                 compose-strategies-fn
                 backtest-fn))

(provide 
 ;; Macro-based implementation
 define/strategy
 define/combined-strategy
 compose-strategies
 backtest
 
 ;; Function-based implementation
 strategy
 combined-strategy
 compose-strategies-fn
 backtest-fn
 
 ;; Shared components
 top-performer
 reduced-date
 ticker-weight
 ticker-weight-ticker
 ticker-weight-weight
 display-strategy-allocation
 display-strategy-comparison
 display-allocation-difference
 
 ;; Time constants
 1y 6m 3m 1m 2w 1w 5d 1d)