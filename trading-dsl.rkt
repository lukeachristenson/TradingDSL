#lang racket

;; Trading Strategy DSL - Core definitions

(require (for-syntax syntax/parse))
(require "./racket-code/data-new.rkt")
(require "./racket-code/strat.rkt")
(require "./racket-code/backtest.rkt")
(require "./racket-code/visualization.rkt")
(require syntax-spec-v3
         (for-syntax syntax/parse racket/list "./racket-code/data-new.rkt"))

(provide define/strategy
         define/combined-strategy
         compose-strategies
         backtest
         ;; existing strategy functions
         top-performer
         reduced-date
         ticker-weight
         ticker-weight-ticker
         ticker-weight-weight
         ;; result functions
         display-strategy-allocation
         display-strategy-comparison
         display-allocation-difference
         ;; time constants
         1y 6m 3m 1m 2w 1w 5d 1d)

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
  ;; Syntax -> (Maybe Error)
  ;; Check if start date is before end date, raise error if not
  (define (check-valid-interval! start-date end-date expr)
    (unless (date-before? (string->date start-date) (string->date end-date))
      (raise-syntax-error 'invalid-period "Invalid date range provided: end date is before start date" expr)))

  ;; Structure to represent a strategy's active period
  (struct period [from to] #:prefab)
  
  ;; Symbol table to store strategy periods
  (define-persistent-symbol-table periods)

  ;; Symbol Syntax Syntax -> Void
  ;; Store a strategy's active period in the symbol table
  (define (store-period! strat-name from to)
    (symbol-table-set! periods strat-name #`#,(period from to)))

  ;; Symbol String String -> Void
  ;; Check if a backtest period is within a strategy's active period
  (define (check-valid-backtest-period! strat-name from to)
    (unless (and (not (date-before? (string->date (period-to (get-period strat-name)))
                                    (string->date to)))
                 (not (date-before? (string->date from)
                                    (string->date (period-from (get-period strat-name))))))
      (raise-syntax-error #f "Invalid backtest period: can only backtest during strategy active period" strat-name)))

  ;; Symbol Symbol Syntax -> Void
  ;; Check if two strategies can be combined (periods must overlap)
  (define (check-combinable! s1 s2 mid-date expr)
    (let ([to-s1 (string->date (get-period-to s1))]
          [from-s2 (string->date (get-period-from s2))])
      (unless (not (date-before? to-s1 from-s2))
        (raise-syntax-error #f "Uncombinable periods: strat1 is inactive before strat2 is active" expr))

      (unless (and (date-before? (string->date mid-date) to-s1)
                   (date-before? from-s2 (string->date mid-date)))
        (raise-syntax-error #f "Mid-date not in interval overlap" expr))))

  ;; Symbol -> Period
  ;; Get a strategy's active period from the symbol table
  (define (get-period strat-name) 
    (syntax->datum 
     (symbol-table-ref periods strat-name
                       (lambda () (raise-syntax-error #f "No active trading period found" strat-name)))))

  ;; Symbol -> String
  ;; Get a strategy's start date
  (define (get-period-from strat-name)
    (period-from (get-period strat-name)))

  ;; Symbol -> String
  ;; Get a strategy's end date
  (define (get-period-to strat-name)
    (period-to (get-period strat-name))))
 

(syntax-spec
  (binding-class strategy
                 #:description "trading strategy"
                 #:reference-compiler immutable-reference-compiler)
  
  ;; define/strategy - Define a strategy with its active period
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

  ;; define/combined-strategy - Combine two strategies with a switchover date
  (host-interface/definitions
   (define/combined-strategy new:strategy s1:strategy s2:strategy #:mid mid-date:string)
   #:binding (export new)
   (check-valid-interval! (get-period-from (attribute s1))
                          (get-period-to (attribute s2))
                          #'new)
   (check-combinable! (attribute s1) (attribute s2) (syntax->datum (attribute mid-date)) #'mid-date)
   (store-period! (attribute new)
                  (get-period-from (attribute s1))
                  (get-period-to (attribute s2)))
   #'(define new (lambda (date) (if (date-before? date (string->date mid-date))
                                    (s1 date) 
                                    (s2 date)))))

  ;; backtest - Run a backtest on a strategy over a specific period
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
                   n-val)))


;;EXAMPLE EXPANSIONS: 


#;(define/combined-strategy seasonal-strategy
  annual-momentum      ;; Use annual momentum strategy initially
  biannual-momentum    ;; Switch to biannual momentum at the midpoint
  #:mid "2024-06-03")

;; Expands to  =>

#;(define seasonal-strategy (lambda (date)
                              (if (date-before? date (string->date "2024-06-03"))
                                    (s1 date) 
                                    (s2 date))))



#;(compose-strategies monthly-momentum short-term-momentum
                     #:weights (0.3 0.7))

;; Expands to =>

#;#'(lambda (date)
      (let* ([s1-result (monthly-momentum date)]
             [s2-result (short-term-momentum date)]
             [combined (compose-strategy-results s1-result s2-result 0.3 0.7)])
        combined))

;; -------------------------------------
;; Strategy Composition Macro
;; -------------------------------------

;; Compose two strategies with optional weights and verify date compatibility
;; (compose-strategies strat1 strat2 #:weights (w1 w2))
;; If weights are not provided, defaults to 50/50 split
(define-syntax (compose-strategies stx)
  (syntax-parse stx
    [(_ strat1:id strat2:id 
        (~optional (~seq #:weights (w1:number w2:number)) 
                   #:defaults ([w1 0.5] [w2 0.5])))
     ;; Check if strategies have compatible periods
     (with-handlers ([exn:fail? (lambda (e) 
                                  ;; If we can't get period info, just proceed without checking
                                  #'(lambda (date)
                                      (let* ([s1-result (strat1 date)]
                                             [s2-result (strat2 date)]
                                             [combined (compose-strategy-results s1-result s2-result w1 w2)])
                                        combined)))])
       (define s1-from (get-period-from (syntax->datum #'strat1)))
       (define s1-to (get-period-to (syntax->datum #'strat1)))
       (define s2-from (get-period-from (syntax->datum #'strat2)))
       (define s2-to (get-period-to (syntax->datum #'strat2)))
       
       ;; Find the overlapping period
       (define overlap-from 
         (if (date-before? (string->date s1-from) (string->date s2-from))
             s2-from s1-from))
       (define overlap-to
         (if (date-before? (string->date s1-to) (string->date s2-to))
             s1-to s2-to))
       
       ;; Check if overlap is valid
       (unless (date-before? (string->date overlap-from) (string->date overlap-to))
         (raise-syntax-error 'compose-strategies 
                           (format "Strategies have non-overlapping periods: ~a to ~a and ~a to ~a"
                                   s1-from s1-to s2-from s2-to)
                           stx))
       
       #'(lambda (date)
           (let* ([s1-result (strat1 date)]
                  [s2-result (strat2 date)]
                  [combined (compose-strategy-results s1-result s2-result w1 w2)])
             combined)))]
    ;; Handle non-identifier or expression strategies
    [(_ strat1:expr strat2:expr 
        (~optional (~seq #:weights (w1:number w2:number)) 
                   #:defaults ([w1 0.5] [w2 0.5])))
     #'(lambda (date)
         (let* ([s1-result (strat1 date)]
                [s2-result (strat2 date)]
                [combined (compose-strategy-results s1-result s2-result w1 w2)])
           combined))]))

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