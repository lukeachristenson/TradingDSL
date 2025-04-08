#lang racket

;; Trading Strategy DSL - Core definitions

(require (for-syntax syntax/parse))
(require "./racket-code/data-new.rkt")
(require "./racket-code/strat.rkt")
(require syntax-spec-v3
         (for-syntax syntax/parse racket/list "./racket-code/data-new.rkt"))

;; Provide all DSL definitions
(provide define/strategy
         define/combined-strategy
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
  (define (check-valid-interval! start-date end-date expr)
    (unless (date-before? (string->date start-date) (string->date end-date))
      (raise-syntax-error 'invalid-period "Invalid date range provided: end date is before start date" expr)))

  (struct period [from to] #:prefab)
  (define-persistent-symbol-table periods)


  (define (store-period! strat-name from to)
    (symbol-table-set! periods strat-name #`#,(period from to)))

  (define (check-valid-backtest-period! strat-name from to)
    (unless (and (not (date-before? (string->date (period-to (get-period strat-name)))
                                    (string->date to)))
                 (not (date-before? (string->date from)
                                    (string->date (period-from (get-period strat-name))))))
      (raise-syntax-error #f "Invalid backtest period: can only backtest during strategy active period" strat-name)))

  (define (check-combinable! s1 s2 expr)
    (let ([to-s1 (string->date (get-period-to s1))]
          [from-s2 (string->date (get-period-from s2))])
      (unless (not (date-before? to-s1 from-s2))
        (raise-syntax-error #f "Uncombinable periods: strat1 is inactive before strat2 is active" expr))))

  (define (get-period strat-name) 
    (syntax->datum 
     (symbol-table-ref periods strat-name
                       (lambda () (raise-syntax-error #f "No active trading period found" strat-name)))))

  (define (get-period-from strat-name)
    (period-from (get-period strat-name)))

  (define (get-period-to strat-name)
    (period-to (get-period strat-name))))
 

(syntax-spec
  (binding-class strategy
                 #:description "trading strategy"
                 #:reference-compiler mutable-reference-compiler)
  
  (host-interface/definitions
   (define/strategy id:strategy expr:racket-expr #:from from-date:string #:to to-date:string)
   #:binding (export id)
   (check-valid-interval! (syntax->datum (attribute from-date))
                          (syntax->datum (attribute to-date))
                        #'start-date)
   (store-period! (attribute id)
                  (attribute from-date)
                  (attribute to-date))
   #'(define id expr)) 

  (host-interface/definitions
   (define/combined-strategy new:strategy s1:strategy s2:strategy #:mid mid-date:string)
   #:binding (export new)
   (check-valid-interval! (get-period-from (attribute s1))
                          (get-period-to (attribute s2))
                          #'new)
   (check-combinable! (attribute s1) (attribute s2) #'s2)
   (store-period! (attribute new)
                  (get-period-from (attribute s1))
                  (get-period-to (attribute s2)))
   #'(define new (lambda (date) (if (date-before? date (string->date mid-date))
                                    (s1 date) 
                                    (s2 date)))))

  (host-interface/expression
   (backtest s:strategy start-date:string end-date:string n-val:racket-expr)
   (check-valid-interval!
    (syntax->datum (attribute start-date)) 
    (syntax->datum (attribute end-date)) #'start-date)
   (check-valid-backtest-period! (attribute s)
                                 (syntax->datum (attribute start-date))
                                 (syntax->datum (attribute end-date)))
   #'(run-backtest s
                   start-date
                   end-date
                   n-val))

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
                [combined (compose-strategy-results s1-result s2-result w1 w2)])
           combined))])) 

;; Helper function to combine strategy results
(define (compose-strategy-results results1 results2 weight1 weight2)
  (define all-tickers (remove-duplicates 
                       (append (map ticker-weight-ticker results1)
                               (map ticker-weight-ticker results2))))
  
  (define (get-weight ticker results)
    (define found (findf (lambda (tw) (string=? (ticker-weight-ticker tw) ticker)) results))
    (if found (ticker-weight-weight found) 0))
  
  (for/list ([ticker all-tickers])
    (define weight1-val (get-weight ticker results1))
    (define weight2-val (get-weight ticker results2))
    (define composed-weight (+ (* weight1 weight1-val)
                               (* weight2 weight2-val)))
    (ticker-weight ticker composed-weight)))

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
(define (run-backtest strategy start-date-str end-date-str top-n)
  (define start-date (string->date start-date-str))
  (define end-date (string->date end-date-str))
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

