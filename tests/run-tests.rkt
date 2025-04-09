#lang racket

;; Test runner for the Trading DSL

(require rackunit)

(displayln "\n=== Running Date Validation Tests ===")
(require "date-validation-tests.rkt")

(displayln "\n=== Running Strategy Validation Tests ===")
(require "strategy-validation-tests.rkt")

(displayln "\n=== All tests completed successfully! ===")