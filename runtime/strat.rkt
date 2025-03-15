#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STRATEGY REPRESENTATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "../data/racket-code/data-new.rkt")

; A Strat is a (-> Date (Hash Ticker Weight))


;A period is one of 1y, 1m

(define (top-performer #:default (period "1y"))
  period)


