#lang racket

(require "data-new.rkt")
(require "strat.rkt")
(require "backtest.rkt")

(provide display-strategy-allocation
         display-strategy-comparison)

;; Strategy Date Number -> Void
;; Display the top stocks and their weights for a strategy on a specific date
(define (display-strategy-allocation strategy date top-n)
  (define top-stocks (get-top-stocks strategy date top-n))
  
  (displayln (format "Portfolio Allocation on ~a:" (date->string date)))
  (for ([tw top-stocks])
    (displayln (format "  ~a: ~a" 
                       (ticker-weight-ticker tw)
                       (ticker-weight-weight tw)))))

;; (listof Strategy) (listof String) Date Number -> Void
;; Compare allocations from multiple strategies on a date
(define (display-strategy-comparison strategies strategy-names date top-n)
  (displayln (format "Strategy Comparison on ~a:" (date->string date)))
  (for ([strategy strategies]
        [name strategy-names])
    (define top-stock (first (get-top-stocks strategy date top-n)))
    (displayln (format "  ~a: top pick = ~a (~a)" 
                       name 
                       (ticker-weight-ticker top-stock)
                       (ticker-weight-weight top-stock)))))