#lang racket

(require rackunit)
(require "../racket-code/data-new.rkt")
(require "../racket-code/strat.rkt")
(require (for-syntax syntax/parse))

;; Tests for strategy validation

;; Structure to represent a strategy period
(struct strategy-period [from to] #:transparent)

;; Test for validity of strategy periods
(define (is-valid-period? period)
  (date-before? (string->date (strategy-period-from period))
                (string->date (strategy-period-to period))))

;; Test period validation
(check-true (is-valid-period? (strategy-period "2024-01-01" "2024-06-01")))
(check-false (is-valid-period? (strategy-period "2024-06-01" "2024-01-01")))
(check-false (is-valid-period? (strategy-period "2024-01-01" "2024-01-01")))

;; Test for combinability of strategies
(define (can-combine-strategies? period1 period2)
  (not (date-before? (string->date (strategy-period-to period1))
                    (string->date (strategy-period-from period2)))))

;; Test strategy combination
(define period1 (strategy-period "2024-01-01" "2024-06-01"))
(define period2 (strategy-period "2024-05-01" "2024-12-31"))
(define period3 (strategy-period "2024-07-01" "2024-12-31"))

(check-true (can-combine-strategies? period1 period2))  ; Overlap
(check-false (can-combine-strategies? period1 period3)) ; No overlap

;; Test for backtest period validity
(define (is-valid-backtest-period? strategy-period backtest-start backtest-end)
  (and (not (date-before? (string->date (strategy-period-to strategy-period))
                         (string->date backtest-end)))
       (not (date-before? (string->date backtest-start)
                         (string->date (strategy-period-from strategy-period))))))

;; Test backtest period validation
(check-true (is-valid-backtest-period? period1 "2024-01-15" "2024-05-15"))  ; Within period
(check-false (is-valid-backtest-period? period1 "2023-12-01" "2024-02-01")) ; Starts before
(check-false (is-valid-backtest-period? period1 "2024-05-01" "2024-07-01")) ; Ends after

;; Test that a strategy function returns expected structure
(check-true (procedure? (top-performer #:period 5)))

;; Tests for stock tickers
(check-true (andmap string? stock-tickers))

;; Tests existence of calculate-momentum function
(check-true (procedure? calculate-momentum))

(displayln "Strategy validation tests completed")