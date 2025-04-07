#lang racket

;; Trading Strategy DSL - Core definitions

(require (for-syntax syntax/parse))
(require "./racket-code/data-new.rkt")
(require "./racket-code/strat.rkt")
(require syntax-spec-v3
         (for-syntax syntax/parse racket/list "./racket-code/data-new.rkt"))

;; Provide all DSL definitions
(provide define/strategy
         compose-strategies
         backtest
         ;; Re-provide existing strategy functions
         top-performer
         reduced-date
         ;; Constants
         1y 6m 3m 1m 2w 1w 5d 1d) 

;; Define time period constants (in days)
(define 1y 365) 
(define 6m 182)
(define 3m 90)
(define 1m 30)
(define 2w 14)
(define 1w 7)
(define 5d 5)
(define 1d 1)

;; -------------------------------------
;; Syntax-spec macros
;; -------------------------------------
(begin-for-syntax
  ;Syntax -> (Maybe Error)
  (define (check-valid-backtest-period! start-date end-date expr)
    (unless (date-before? start-date end-date)
      (raise-syntax-error 'invalid-period "Invalid date range provided to backtest" expr))))
 

(syntax-spec
  (binding-class strategy
                 #:description "trading strategy"
                 #:reference-compiler mutable-reference-compiler)
  
  (host-interface/definitions
   (define/strategy id:strategy expr:racket-expr)
   #:binding (export id)
   #'(define id expr))

  (host-interface/expression
   (backtest s:strategy start-date:racket-expr end-date:racket-expr n-val:racket-expr)
   #'(run-backtest s start-date end-date n-val))

  #;(host-interface/expression
   (compose strat1:expr strat2:expr 
        (~optional (~seq #:weights (w1:number w2:number)) 
                   #:defaults ([w1 0.5] [w2 0.5])))
   #'(run-backtest s start-date end-date n-val))) 

;; -------------------------------------
;; Strategy Composition Macro
;; -------------------------------------

;; Compose two strategies with optional weights
(define-syntax (compose-strategies stx)
  (syntax-parse stx
    [(_ strat1:expr strat2:expr 
        (~optional (~seq #:weights (w1:number w2:number)) 
                   #:defaults ([w1 0.5] [w2 0.5])))
     #'(lambda (date)
         (let* ([s1-result (strat1 date)]
                [s2-result (strat2 date)]
                [combined (combine-strategy-results s1-result s2-result w1 w2)])
           combined))]))

;; Helper function to combine strategy results
(define (combine-strategy-results results1 results2 weight1 weight2)
  (define all-tickers (remove-duplicates 
                       (append (map ticker-weight-ticker results1)
                               (map ticker-weight-ticker results2))))
  
  (define (get-weight ticker results)
    (define found (findf (lambda (tw) (string=? (ticker-weight-ticker tw) ticker)) results))
    (if found (ticker-weight-weight found) 0))
  
  (for/list ([ticker all-tickers])
    (define weight1-val (get-weight ticker results1))
    (define weight2-val (get-weight ticker results2))
    (define combined-weight (+ (* weight1 weight1-val)
                               (* weight2 weight2-val)))
    (ticker-weight ticker combined-weight)))

;; -------------------------------------
;; Backtesting Macro
;; -------------------------------------



#|
;; Backtest a strategy over time
(define-syntax (backtest stx)
  (syntax-parse stx
    [(_ strategy-expr:expr 
        #:from start-date:expr 
        #:to end-date:expr
        (~optional (~seq #:top-n n-val:expr) #:defaults ([n-val 10])))
     #'(run-backtest strategy-expr start-date end-date n-val)]))
|#


;; Backtesting implementation
(define (run-backtest strategy start-date end-date top-n)
  (define trading-days (active-trading-days start-date end-date))
  
  (unless (pair? trading-days)
    (error "Backtest period contains no trading days"))
  
  (define first-day (car trading-days))
  ;; Get top N stocks from strategy
  (define top-stocks 
    (take (sort (strategy first-day)
                (lambda (a b) (> (ticker-weight-weight a)
                                 (ticker-weight-weight b))))
          (min top-n (length (strategy first-day)))))
  
  ;; Start with initial portfolio
  (define initial-ticker (ticker-weight-ticker (first top-stocks)))
  (define initial-price (stock-data-close (get-stock-data initial-ticker first-day)))
  
  ;; Calculate performance for each trading day
  (let loop ([days (cdr trading-days)]
             [current-ticker initial-ticker]
             [buy-price initial-price]
             [cumulative-return 1.0])
    (if (null? days)
        ;; Return final results
        (list cumulative-return
              (format "~a% return" (* 100 (- cumulative-return 1))))
        ;; Process next day
        (let* ([day (car days)]
               [sell-price (stock-data-close (get-stock-data current-ticker day))]
               [day-return (/ sell-price buy-price)]
               ;; Get new top stock for rebalancing
               [new-top-stock (first (take (sort (strategy day)
                                                (lambda (a b) (> (ticker-weight-weight a)
                                                                 (ticker-weight-weight b))))
                                          (min top-n (length (strategy day)))))]
               [new-ticker (ticker-weight-ticker new-top-stock)]
               [new-price (stock-data-close (get-stock-data new-ticker day))])
          (loop (cdr days)
                new-ticker
                new-price 
                (* cumulative-return day-return))))))
 
(define (active-trading-days start-date end-date)
  (cond
    [(date-before? end-date start-date) '()]
    [else (cons start-date
                (active-trading-days (next-trading-day
                                      (add-days start-date 1))
                                     end-date))]))

