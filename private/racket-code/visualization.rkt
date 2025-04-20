#lang racket

(require "./data-new.rkt")
(require "./strat.rkt")
(require "./backtest.rkt")

(provide display-strategy-allocation
         display-strategy-comparison
         display-allocation-difference)

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

;; Strategy Strategy String String Date Number -> Void
;; Compare two strategies and show the difference in stock allocation
(define (display-allocation-difference strat1 strat2 name1 name2 date top-n)
  (define stocks1 (get-top-stocks strat1 date top-n))
  (define stocks2 (get-top-stocks strat2 date top-n))
  
  ;; Create a hash of each strategy's stock weights
  (define weights1 (for/hash ([tw stocks1])
                     (values (ticker-weight-ticker tw) (ticker-weight-weight tw))))
  (define weights2 (for/hash ([tw stocks2])
                     (values (ticker-weight-ticker tw) (ticker-weight-weight tw))))
  
  ;; Get all unique tickers from both strategies
  (define all-tickers 
    (remove-duplicates
     (append (map ticker-weight-ticker stocks1)
             (map ticker-weight-ticker stocks2))))
  
  ;; Get the weight for a ticker in a specific strategy
  (define (get-weight ticker weights)
    (hash-ref weights ticker 0))
  
  (displayln (format "Allocation Difference: ~a vs ~a on ~a" name1 name2 (date->string date)))
  (displayln "  Format: TICKER: [Strategy1 weight] vs [Strategy2 weight] (difference)")
  (displayln "  Sorted by absolute difference magnitude")
  
  ;; Calculate differences and sort by magnitude
  (define differences
    (for/list ([ticker all-tickers])
      (define w1 (get-weight ticker weights1))
      (define w2 (get-weight ticker weights2))
      (define diff (- w1 w2))
      (list ticker w1 w2 diff)))
  
  (define sorted-diffs
    (sort differences (lambda (a b) (> (abs (fourth a)) (abs (fourth b))))))
  
  ;; Display differences, formatting based on which strategy weights it higher
  (for ([item (take sorted-diffs (min top-n (length sorted-diffs)))])
    (define ticker (first item))
    (define w1 (second item))
    (define w2 (third item))
    (define diff (fourth item))
    
    (cond
      [(> diff 0) ; First strategy ranks it higher
       (displayln (format "  ~a: [~a: ~a] vs [~a: ~a] (~a higher in ~a)" 
                          ticker name1 w1 name2 w2 (abs diff) name1))]
      [(< diff 0) ; Second strategy ranks it higher
       (displayln (format "  ~a: [~a: ~a] vs [~a: ~a] (~a higher in ~a)" 
                          ticker name1 w1 name2 w2 (abs diff) name2))]
      [else ; Same weight - very unlikely
       (displayln (format "  ~a: [~a: ~a] vs [~a: ~a] (same)" 
                          ticker name1 w1 name2 w2))])))