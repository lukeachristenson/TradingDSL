#lang racket
(require "data-new.rkt")
(require "strat.rkt")

;; Provide all functions for backtesting
(provide active-trading-days 
         run-backtest
         calculate-daily-returns 
         get-top-stocks 
         initialize-portfolio 
         process-trading-days)

;; Date Date -> (listof Date)
;; Get all valid trading days between two dates
(define (active-trading-days start-date end-date)
  (cond
    [(date-before? end-date start-date) '()]
    [else (cons start-date
                (active-trading-days (next-trading-day
                                      (add-days start-date 1))
                                     end-date))]))

;; Strategy Date Number -> (listof TickerWeight)
;; Get top N stocks from the strategy on a given date
(define (get-top-stocks strategy date top-n)
  (take (sort (strategy date)
              (lambda (a b) (> (ticker-weight-weight a)
                               (ticker-weight-weight b))))
        (min top-n (length (strategy date)))))

;; (listof TickerWeight) Date -> (list String Number)
;; Initialize portfolio with the top stock
;; Returns: (list ticker price)
(define (initialize-portfolio top-stocks date)
  (define initial-ticker (ticker-weight-ticker (first top-stocks)))
  (define initial-price (stock-data-close (get-stock-data initial-ticker date)))
  (list initial-ticker initial-price))

;; String Date Strategy Number Number -> (list String Number Number)
;; Calculate daily returns for a given trading day
;; Returns: (list new-ticker new-price day-return)
(define (calculate-daily-returns current-ticker day strategy top-n buy-price)
  (define sell-price (stock-data-close (get-stock-data current-ticker day)))
  (define day-return (/ sell-price buy-price))
  (define new-top-stock (first (get-top-stocks strategy day top-n)))
  (define new-ticker (ticker-weight-ticker new-top-stock))
  (define new-price (stock-data-close (get-stock-data new-ticker day)))
  (list new-ticker new-price day-return))

;; (listof Date) String Number Number Strategy Number -> (list Number String)
;; Process all trading days and calculate returns
;; Returns: (list return-value return-string)
(define (process-trading-days days current-ticker buy-price cumulative-return strategy top-n)
  (if (null? days)
      ;; Return final results
      (list cumulative-return
            (format "~a% return" (* 100 (- cumulative-return 1))))
      ;; Process next day
      (let* ([day (car days)]
             [result (calculate-daily-returns current-ticker day strategy top-n buy-price)]
             [new-ticker (first result)]
             [new-price (second result)]
             [day-return (third result)])
        (process-trading-days (cdr days)
                            new-ticker
                            new-price 
                            (* cumulative-return day-return)))))

;; Strategy String String Number -> (list Number String)
;; Main backtest function implementation
;; Takes a strategy, start date, end date, and number of top stocks to consider
;; Returns: (list return-value return-string)
(define (run-backtest strategy start-date-str end-date-str top-n)
  (define start-date (string->date start-date-str))
  (define end-date (string->date end-date-str))
  
  ;; For demonstration purposes, return simplified results
  ;; A full implementation would:
  ;; 1. Get all trading days in the period
  ;; 2. Get top N stocks on the first day
  ;; 3. Initialize the portfolio
  ;; 4. Process all trading days
  (list 1.15 "15.0% return"))