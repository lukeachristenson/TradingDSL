#lang racket

;; Trading Strategy DSL - Data Module
;; Handles data acquisition, formatting, and manipulation

(provide get-market-data
         get-stock-data
         load-data
         calculate-performance
         calculate-beta
         normalize-weights)

;; Data structure format (as described in design document):
;; (Map Date Options)
;; 
;; Options is a map of stock tickers to weights:
;; (Map Ticker Weight)
;; 
;; Data structure for each stock:
;; (Map Date
;;   (listof
;;     (Map Ticker
;;       (listof Price, P/E, Beta, RSI))))

;; load-data: Loads market data from CSV files
;; file-path - Path to CSV file
;; Returns formatted market data structure
(define (load-data file-path)
  ;; Implementation of data loading from CSV
  (let ([data (make-hash)])
    ;; Read CSV file and populate the data structure
    (with-input-from-file file-path
      (lambda ()
        (let ([header (read-csv-line)]
              [line (read-csv-line)])
          (let loop ([line line])
            (unless (eof-object? line)
              (when (>= (length line) 6) ; Ensure there are enough fields
                (let* ([date (list-ref line 0)]
                       [ticker (list-ref line 1)]
                       [price (string->number (list-ref line 2))]
                       [pe (string->number (list-ref line 3))]
                       [beta (string->number (list-ref line 4))]
                       [rsi (string->number (list-ref line 5))])
                  
                  ;; Create nested structure if needed
                  (unless (hash-has-key? data date)
                    (hash-set! data date (make-hash)))
                  
                  ;; Store stock data
                  (let ([date-map (hash-ref data date)])
                    (hash-set! date-map ticker (list price pe beta rsi)))))
              (loop (read-csv-line)))))))
    data))

;; read-csv-line: Helper function to read a line from a CSV file
;; Returns a list of fields from the CSV line
(define (read-csv-line)
  (let ([line (read-line)])
    (if (eof-object? line)
        line
        (string-split line ","))))

;; get-market-data: Gets market data for a date range
;; start-date, end-date - Date range for data retrieval
;; Returns market data structure for the specified range
(define (get-market-data start-date end-date)
  ;; TODO: Implement fetching and filtering historical data for the date range
  ;; For now, return a sample data structure
  (let ([data (make-hash)])
    ;; Add sample data for demonstration
    (define sample-stocks (list "AAPL" "MSFT" "GOOGL" "AMZN" "META"))
    
    ;; For each date, create stock data
    (for ([i (in-range 10)])
      (let ([date (format "2023-~a-~a" (+ 1 (quotient i 3)) (+ 1 (remainder i 30)))]
            [stocks (make-hash)])
        
        ;; For each stock, create random data
        (for ([ticker sample-stocks])
          (hash-set! stocks ticker 
                     (list (+ 100 (random 100))  ; Price
                           (+ 10 (random 30))    ; P/E
                           (+ 0.5 (random 4.0))  ; Beta
                           (+ 30 (random 70))))) ; RSI
        
        (hash-set! data date stocks)))
    
    data))

;; get-stock-data: Gets stock data from market data for a specific interval
;; market-data - Market data structure
;; interval - Time period to consider
;; Returns filtered stock data
(define (get-stock-data market-data interval)
  ;; TODO: Filter market data based on the interval
  ;; For now, return the market data as is
  (let ([result (make-hash)])
    (for ([(date stocks) (in-hash market-data)])
      (for ([(ticker stock-data) (in-hash stocks)])
        (unless (hash-has-key? result ticker)
          (hash-set! result ticker stock-data))))
    result))

;; calculate-performance: Calculates stock performance over a time period
;; data - Stock data
;; interval - Time period
;; Returns a performance score between 0 and 100
(define (calculate-performance data interval)
  ;; TODO: Implement actual performance calculation based on historical data
  ;; For now, return a random value weighted by interval
  (let ([interval-weight (cond [(equal? interval '1y) 2.0]
                               [(equal? interval '6m) 1.5]
                               [(equal? interval '5d) 1.0]
                               [else 1.0])])
    (* interval-weight (random 50.0))))

;; calculate-beta: Calculates stock beta over a time period
;; data - Stock data
;; interval - Time period
;; Returns a beta value
(define (calculate-beta data interval)
  ;; TODO: Implement actual beta calculation based on historical data
  ;; For now, return the beta from the stock data, or a random value if not available
  (let ([beta (if (and (list? data) (>= (length data) 3)) 
                  (list-ref data 2)
                  (+ 0.5 (random 4.0)))])
    beta))

;; normalize-weights: Normalizes weights so they sum to 100
;; weights - Hash map of weights
;; Returns a hash map of normalized weights
(define (normalize-weights weights)
  (let* ([total (for/sum ([(ticker weight) (in-hash weights)]) 
                 (if (number? weight) weight 0))]
         [normalized (make-hash)])
    (when (positive? total)
      (for ([(ticker weight) (in-hash weights)])
        (when (number? weight)
          (hash-set! normalized ticker (* 100 (/ weight total))))))
    normalized))