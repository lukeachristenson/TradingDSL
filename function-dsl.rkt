#lang racket

;; Trading Strategy DSL - Function-based implementation
;; An alternative implementation that uses functions with optional keyword arguments
;; instead of macros for the trading DSL.

(require "./private/racket-code/data-new.rkt")
(require "./private/racket-code/strat.rkt")
(require "./private/racket-code/backtest.rkt")
(require "./private/racket-code/visualization.rkt")

(provide 
 ;; Function-based strategy definitions
 strategy
 combined-strategy
 compose-strategies-fn
 backtest-fn
 ;; Time constants
 1y 6m 3m 1m 2w 1w 5d 1d
 ;; Strategy functions
 top-performer
 ;; Result functions
 display-strategy-allocation
 display-strategy-comparison
 display-allocation-difference
 ;; Access functions
 ticker-weight
 ticker-weight-ticker
 ticker-weight-weight
 reduced-date)

;; Time constants (in days)
(define 1y 365) 
(define 6m 182)
(define 3m 90)
(define 1m 30)
(define 2w 14)
(define 1w 7)
(define 5d 5)
(define 1d 1)

;; -----------------------------------------------
;; Strategy representation
;; -----------------------------------------------

;; A StrategySpec is a (struct strategy-spec (strategy-fn from-date to-date))
;; Represents a strategy function with its active trading period
(struct strategy-spec (strategy-fn from-date to-date) #:transparent)

;; (-> Date (listof TickerWeight)) String String -> StrategySpec
;; Creates a new trading strategy with an active period
(define (strategy strat-fn #:from from-date #:to to-date)
  ;; Validate dates
  (unless (date-before? (string->date from-date) (string->date to-date))
    (error 'strategy "Invalid date range: end date is before start date"))
  
  ;; Return strategy specification
  (strategy-spec strat-fn from-date to-date))

;; StrategySpec StrategySpec String -> StrategySpec
;; Creates a combined strategy that switches between two strategies at a midpoint date
(define (combined-strategy s1 s2 #:mid mid-date)
  (define s1-from (strategy-spec-from-date s1))
  (define s2-to (strategy-spec-to-date s2))
  
  ;; Validate date ranges
  (unless (date-before? (string->date s1-from) (string->date s2-to))
    (error 'combined-strategy "Invalid date range: end date is before start date"))
  
  ;; Check for strategy overlap
  (define s1-to (strategy-spec-to-date s1))
  (define s2-from (strategy-spec-from-date s2))
  (unless (not (date-before? (string->date s1-to) (string->date s2-from)))
    (error 'combined-strategy "Uncombinable periods: strat1 is inactive before strat2 is active"))
  
  ;; Create combined strategy
  (define combined-fn 
    (lambda (date)
      (if (date-before? date (string->date mid-date))
          ((strategy-spec-strategy-fn s1) date)
          ((strategy-spec-strategy-fn s2) date))))
  
  ;; Return combined strategy spec
  (strategy-spec combined-fn s1-from s2-to))

;; -----------------------------------------------
;; Strategy Composition
;; -----------------------------------------------

;; StrategySpec StrategySpec [Number Number] -> StrategySpec
;; Composes two strategies with optional weights
(define (compose-strategies-fn s1 s2 #:weights [weights '(0.5 0.5)])
  (define w1 (first weights))
  (define w2 (second weights))
  
  ;; Get periods
  (define s1-from (strategy-spec-from-date s1))
  (define s1-to (strategy-spec-to-date s1))
  (define s2-from (strategy-spec-from-date s2))
  (define s2-to (strategy-spec-to-date s2))
  
  ;; Find overlapping period
  (define overlap-from 
    (if (date-before? (string->date s1-from) (string->date s2-from))
        s2-from s1-from))
  (define overlap-to
    (if (date-before? (string->date s1-to) (string->date s2-to))
        s1-to s2-to))
  
  ;; Check for valid overlap
  (unless (date-before? (string->date overlap-from) (string->date overlap-to))
    (error 'compose-strategies-fn 
           (format "Strategies have non-overlapping periods: ~a to ~a and ~a to ~a"
                   s1-from s1-to s2-from s2-to)))
  
  ;; Create composed function
  (define composed-fn
    (lambda (date)
      (let* ([s1-result ((strategy-spec-strategy-fn s1) date)]
             [s2-result ((strategy-spec-strategy-fn s2) date)]
             [combined (compose-strategy-results s1-result s2-result w1 w2)])
        combined)))
  
  ;; Return composed strategy spec
  (strategy-spec composed-fn overlap-from overlap-to))

;; -----------------------------------------------
;; Backtesting
;; -----------------------------------------------

;; StrategySpec String String Number -> Any
;; Run a backtest on a strategy over a specific period
(define (backtest-fn s start-date end-date n-val)
  ;; Validate date range
  (unless (date-before? (string->date start-date) (string->date end-date))
    (error 'backtest "Invalid date range: end date is before start date"))
  
  ;; Validate backtest period against strategy's active period
  (define s-from (strategy-spec-from-date s))
  (define s-to (strategy-spec-to-date s))
  (unless (and (not (date-before? (string->date s-to) (string->date end-date)))
               (not (date-before? (string->date start-date) (string->date s-from))))
    (error 'backtest "Invalid backtest period: can only backtest during strategy active period"))
  
  ;; Run the backtest
  (run-backtest (strategy-spec-strategy-fn s) start-date end-date n-val))

;; (listof TickerWeight) (listof TickerWeight) Number Number -> (listof TickerWeight)
;; Helper function to combine strategy results
;; Takes results from two strategies and weights them according to the provided weights
(define (compose-strategy-results results1 results2 weight1 weight2)
  ;; Get all unique tickers from both strategies
  (define all-tickers (remove-duplicates 
                       (append (map ticker-weight-ticker results1)
                               (map ticker-weight-ticker results2))))
  
  ;; Get the weight for a ticker in a specific set of results
  (define (get-weight ticker results)
    (define found (findf (lambda (tw) (string=? (ticker-weight-ticker tw) ticker)) results))
    (if found (ticker-weight-weight found) 0))
  
  ;; For each ticker, calculate its weighted value
  (for/list ([ticker all-tickers])
    (define weight1-val (get-weight ticker results1))
    (define weight2-val (get-weight ticker results2))
    (define composed-weight (+ (* weight1 weight1-val)
                               (* weight2 weight2-val)))
    (ticker-weight ticker composed-weight)))