#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STRATEGY REPRESENTATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "data-new.rkt")

; A Strat is a (-> Date (Hash Ticker Weight))

(define (top-performer #:default (period "1y"))
  period)


(top-performer )

