#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STRATEGY REPRESENTATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "data-new.rkt" (for-syntax syntax/parse))
(provide st-1 ticker-weight ticker-weight-weight ticker-weight-ticker top-performer)

; A Strat is a (-> Date (Hash Ticker Weight))
; A period is one of 1y, 6m, 1m, 2w

; A TickerWeight is a (ticker-weight Ticker Number)
(define-struct ticker-weight (ticker weight) #:transparent)

(define stock-tickers
  '("AAPL" "ABBV" "ABT" "ACN" "AMD" "AMGN" "AMZN" "AVGO" "BAC" "BRK-B"
  "COST" "CRM" "CSCO" "CVX" "DHR" "DIS" "GOOGL" "HD" "HON" "INTC"
  "JNJ" "JPM" "KO" "LIN" "LLY" "LOW" "MA" "MCD" "META" "MRK" "MSFT"
  "NEE" "NFLX" "NKE" "NVDA" "ORCL" "PEP" "PFE" "PG" "PM" "RTX" "TMUS"
  "TSLA" "TXN" "UNH" "UPS" "V" "WMT" "XOM"))

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
              (if (and (has-stock-data ticker start-date)
                       (has-stock-data ticker end-date))
                  (/ (stock-data-close
                      (get-stock-data ticker end-date)) 
                     (stock-data-close
                      (get-stock-data ticker start-date)))
                  0))) ; Some missing data, assume weight 0 in this case
           (lambda (x y) (> (ticker-weight-weight x)
                            (ticker-weight-weight y))))))))


(define 1y 365)
(define 6m 182)
(define 3m 90)
(define 1m 30)
(define 2w 14)
(define 1w 7)
(define 1d 1)




(define st-1 (top-performer #:period 1y))










;;(st-1 (reduced-date 2024 12 31))
#;(def/custom-strat (/ (stock-data-close
                      (get-stock-data ticker end-date)) 
                     (stock-data-close
                      (get-stock-data ticker start-date))))









