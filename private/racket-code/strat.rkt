#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STRATEGY REPRESENTATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "./data-new.rkt" (for-syntax syntax/parse))
(provide ticker-weight 
         ticker-weight-weight 
         ticker-weight-ticker 
         top-performer
         calculate-momentum
         stock-tickers)

;; A Strat is a (-> Date (listof TickerWeight))
;; A period is one of 1y, 6m, 1m, 2w

;; A TickerWeight is a (ticker-weight Ticker Number)
(define-struct ticker-weight (ticker weight) #:transparent)

;; (listof String)
;; List of stock tickers we're working with
(define stock-tickers
  '("AAPL" "ABBV" "ABT" "ACN" "AMD" "AMGN" "AMZN" "AVGO" "BAC" "BRK-B"
  "COST" "CRM" "CSCO" "CVX" "DHR" "DIS" "GOOGL" "HD" "HON" "INTC"
  "JNJ" "JPM" "KO" "LIN" "LLY" "LOW" "MA" "MCD" "META" "MRK" "MSFT"
  "NEE" "NFLX" "NKE" "NVDA" "ORCL" "PEP" "PFE" "PG" "PM" "RTX" "TMUS"
  "TSLA" "TXN" "UNH" "UPS" "V" "WMT" "XOM"))

;; String Date Date -> Number
;; Calculate momentum (price change) for a ticker over a period
;; Returns ratio of end price to start price (1.0 means no change)
(define (calculate-momentum ticker start-date end-date)
  (if (and (has-stock-data ticker start-date)
           (has-stock-data ticker end-date))
      (/ (stock-data-close (get-stock-data ticker end-date)) 
         (stock-data-close (get-stock-data ticker start-date)))
      0)) ; Some missing data, assume weight 0 in this case

;; Number -> (Date -> (listof TickerWeight))
;; Creates a strategy that selects top performing stocks over a period
;; Returns a function that takes a date and returns a sorted list of ticker-weights
(define (top-performer #:period period)
  (lambda (date)
    (if (is-weekend-or-holiday date) 
      (error "provided date is a holiday") 
      (let ([start-date (next-trading-day
                         (sub-days date period))]
            [end-date date])
        (sort
         (for/list
             ([ticker stock-tickers])
           (ticker-weight
            ticker
            (calculate-momentum ticker start-date end-date)))
         (lambda (x y) (> (ticker-weight-weight x)
                          (ticker-weight-weight y))))))))

;; Define time period constants (in days)
;; These are duplicated from trading-dsl.rkt for testing
(define 1y 365)
(define 6m 182)
(define 3m 90)
(define 1m 30)
(define 2w 14)
(define 1w 7)
(define 1d 1)