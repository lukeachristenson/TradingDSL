#lang racket
(require "data-new.rkt")
(require "strat.rkt")





(define (active-trading-days start-date end-date)
  (cond
    [(date-before? end-date start-date) '()]
    [else (cons start-date
                (active-trading-days (next-trading-day
                                      (add-days start-date 1))
                                     end-date))]))

; Strategy Date Date -> Number 
; Given a strategy, backtest it over the given period, produce the annual compounding percantage
(define (backtest st start end)
  (let*
      ([start-date (next-trading-day start)]
       [end-date (next-trading-day end)]
       [trading-days (active-trading-days start-date end-date)]
       [cur-ticker (ticker-weight-ticker (first (st start-date)))]
       [buy-price (stock-data-close (get-stock-data cur-ticker start-date))]
       [cur-mul 1])
    (for-each
     (lambda (day)
       (define sell-price (stock-data-close (get-stock-data cur-ticker day)))
       (define new-ticker (ticker-weight-ticker (first (st day))))
       
       (newline)
       (println (date->string day))
       (println new-ticker)
       (println cur-mul)

       (set! cur-mul (/ sell-price buy-price))
       (set! buy-price (stock-data-close (get-stock-data new-ticker day)))
       (set! cur-ticker new-ticker))
     trading-days)
    cur-ticker))


#;(backtest st-1
          (reduced-date 2024 01 01)
          (reduced-date 2024 12 31))


#;(cons start-date
      (active-trading-days (next-trading-day
                            (add-days start-date 1))
                           end-date))

