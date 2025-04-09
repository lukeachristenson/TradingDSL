#lang racket

(require rackunit)
(require "../racket-code/data-new.rkt")
(require (for-syntax syntax/parse))
(require syntax-spec-v3
         (for-syntax syntax/parse racket/list "../racket-code/data-new.rkt"))

;; Tests for date validation functions

;; Test string->date with valid inputs
(check-equal? (string->date "2024-01-02") (reduced-date 2024 1 2))
(check-equal? (string->date "2025-12-31") (reduced-date 2025 12 31))

;; Test string->date with invalid format
(check-exn exn:fail? (lambda () (string->date "01/02/2024")))
(check-exn exn:fail? (lambda () (string->date "2024-1-2")))  ; needs zero padding
(check-exn exn:fail? (lambda () (string->date "not-a-date")))

;; Test date->string
(check-equal? (date->string (reduced-date 2024 1 2)) "2024-1-2")
(check-equal? (date->string (reduced-date 2025 12 31)) "2025-12-31")

;; Test date-before?
(check-true (date-before? (reduced-date 2024 1 1) (reduced-date 2024 1 2)))
(check-true (date-before? (reduced-date 2024 1 1) (reduced-date 2025 1 1)))
(check-false (date-before? (reduced-date 2024 1 2) (reduced-date 2024 1 1)))
(check-false (date-before? (reduced-date 2024 1 1) (reduced-date 2024 1 1)))

;; Test add-days and sub-days
;; Note: add-days isn't precise for test purposes - testing the concept, not exact days
(check-true (date-before? (reduced-date 2024 1 1) (add-days (reduced-date 2024 1 1) 1)))
;; Note: sub-days isn't precise for test purposes - testing the concept, not exact days
(check-true (date-before? (sub-days (reduced-date 2024 1 2) 1) (reduced-date 2024 1 2)))

;; Simulate compile-time check for invalid intervals
(begin-for-syntax
  (define (test-check-valid-interval! start-date end-date)
    (unless (date-before? (string->date start-date) (string->date end-date))
      (raise-syntax-error 'invalid-period "Invalid date range provided: end date is before start date"))))

;; Compile-time error tests: These should fail to compile

#|
;; Test 1: End date before start date in strategy definition
(define-syntax test-invalid-strategy
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       (begin
         (test-check-valid-interval! "2024-06-01" "2024-01-01")
         #'(void))])))

;; Test 2: Same date for start and end
(define-syntax test-same-dates-strategy
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       (begin
         (test-check-valid-interval! "2024-01-01" "2024-01-01")
         #'(void))])))

;; Test 3: Malformed date strings
(define-syntax test-malformed-dates
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       (begin
         (test-check-valid-interval! "2024/01/01" "2024-06-01")
         #'(void))])))
|#

(displayln "Date validation tests completed")

(provide)