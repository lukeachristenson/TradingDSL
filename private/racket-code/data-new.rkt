#lang racket

;; Trading Strategy DSL - Data Module
;; Handles data acquisition, formatting, and manipulation

(require csv-reading)
(require racket/date)
(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; BACKTESTING DATA FORMAT ;;
;;                         ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A StockData is a (stock-data Number Number)
;; Represents open and close prices for a single ticker on a given date
(define-struct stock-data (open close) #:transparent)

;; A DataKey is a (key Ticker Date)
;; Used as a key in the data hash table
(define-struct key (ticker date) #:transparent)
  
;; A Ticker is a String representing a stock symbol
;; A Date is a (reduced-date Number Number Number)
;; Representing year, month, day
(define-struct reduced-date (year month day) #:transparent)


;; String -> Date
;; Parse a date string in YYYY-MM-DD format and return a date struct
(define (string->date str)
  (match (regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})$" str)
    [(list _ year month day)
     (reduced-date (string->number year)
                   (string->number month)
                   (string->number day))]
    [_ (error "Invalid date format. Expected YYYY-MM-DD.")]))

;; Date -> String
;; Convert a date struct to a string in YYYY-M-D format
(define (date->string date)
  (string-append
   (number->string (reduced-date-year date))
   "-"
   (number->string (reduced-date-month date))
   "-"
   (number->string (reduced-date-day date))))

;; Test cases for date conversion
(check-equal? (string->date "2024-01-02") (reduced-date 2024 01 02))
(check-equal? (string->date "2025-03-18") (reduced-date 2025 03 18))
(check-equal? (date->string (reduced-date 2024 01 02)) "2024-1-2")
(check-equal? (date->string (reduced-date 2025 03 18)) "2025-3-18")

;; Date -> RacketDate
;; Convert our date struct to a Racket date object
(define (date->racket-date d)
  (seconds->date (find-seconds 0 0 0 
                               (reduced-date-day d)
                               (reduced-date-month d)
                               (reduced-date-year d))))

;; RacketDate -> Date
;; Convert a Racket date object back to our date struct
(define (racket-date->date d)
  (make-reduced-date (date-year d) (date-month d) (date-day d)))

;; Test cases for Racket date conversion
(check-equal? (date->racket-date (string->date "2024-01-02"))
              (date* 0 0 0 2 1 2024 2 1 #f -18000 0 "EST"))
(check-equal? (racket-date->date
               (date* 0 0 0 2 1 2024 2 1 #f -18000 0 "EST"))
              (reduced-date 2024 1 2))

;; Date Number -> Date
;; Add n days to a given date struct
(define (add-days d n)
  (racket-date->date (seconds->date (+ (date->seconds (date->racket-date d))
                                       (* n 90000)))))

;; Date Number -> Date
;; Subtract n days from a given date struct
(define (sub-days d n)
  (racket-date->date (seconds->date (- (date->seconds (date->racket-date d))
                                       (* n 90000)))))

;; Date Date -> Boolean
;; Return true if the first date is before the second
(define (date-before? d1 d2)
  (< (date->seconds (date->racket-date d1))
     (date->seconds (date->racket-date d2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;;      LOADING DATA       ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; String -> Hash
;; Load a CSV file and convert it to a hash table
(define (load-table path)
  (with-input-from-file path
    (lambda () (csv-list->table (csv->list (current-input-port))))))

;; (listof (listof String)) -> Hash
;; Convert a list of CSV rows to a hash table
;; Keys are (key ticker date), values are (stock-data open close)
(define (csv-list->table lst)
  (define column-names (first lst))
  (define row-lists (rest lst))
  (for/hash ([row-list row-lists])
    (values (key (second row-list)
                 (string->date (first row-list))) 
            (stock-data (string->number (third row-list))
                        (string->number (fourth row-list)))))) 

;; The main stock data hash table
;; Maps (key ticker date) -> (stock-data open close)
(define STOCK-DATA (load-table "./data/1y50-data/pricedata-1y-with-metrics.csv"))

;; String Date -> StockData
;; Get stock data for a ticker on a specific date
(define (get-stock-data ticker dt)  
  (hash-ref STOCK-DATA (key ticker dt))) 

;; String Date -> Boolean
;; Check if we have valid stock data for a ticker on a specific date
(define (has-stock-data ticker dt)
  (define data (get-stock-data ticker dt))
  (not (= (stock-data-close data) -1)))

;; Date -> Boolean
;; Check if a date is a weekend or holiday (no trading)
(define (is-weekend-or-holiday dt)
  (with-handlers ([exn:fail?
                   (λ (e) #t)])
    (get-stock-data "AAPL" dt)
    #f))

;; Date -> Date
;; Get the next valid trading day (skip weekends and holidays)
(define (next-trading-day dt)
  (if (is-weekend-or-holiday dt)
      (next-trading-day (add-days dt 1))
      dt))