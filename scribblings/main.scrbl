#lang scribble/manual

@(require
   scribble/example
   (for-label racket TradingDSL))

@title{TradingDSL: A Domain-Specific Language for Trading Strategy Development}

@defmodule[TradingDSL]

@section{Introduction}

TradingDSL is a domain-specific language for defining, testing, and analyzing stock trading strategies. It provides a clean syntax for specifying strategies based on momentum and other factors, defining active periods for strategies, combining strategies, and backtesting.

The DSL is implemented in two complementary ways:
@itemlist[
  @item{A macro-based implementation using @tt{syntax-spec} for compile-time validation}
  @item{A function-based implementation providing similar capabilities with runtime checks}
]

Both implementations allow traders to express strategies concisely while ensuring important constraints like date validation are enforced.

@section{Key Concepts}

@subsection{Strategies}

A strategy in TradingDSL is a function that takes a date and returns a list of ticker-weight pairs, representing an allocation of stocks in a portfolio. The DSL provides macros and functions to define, compose, and test these strategies.

@subsection{Active Periods}

Each strategy has an active period defined by start and end dates. This represents the time period for which the strategy is valid. The DSL enforces that strategies are only used within their active periods.

@subsection{Backtesting}

Backtesting simulates running a strategy over a historical period to evaluate its performance. TradingDSL provides built-in backtesting functions that validate the test period against the strategy's active period.

@section{Macro-Based Interface}

@defform[(define/strategy id strategy-expr #:from from-date #:to to-date)]{
  Defines a new strategy with identifier @racket[id], implemented by @racket[strategy-expr], which should evaluate to a function that takes a date and returns a list of @racket[ticker-weight] structures.

  The strategy is constrained to be active from @racket[from-date] to @racket[to-date], which must be strings in the format "YYYY-MM-DD". Compile-time error checking ensures the end date is not before the start date.

  @examples[#:eval (make-base-eval '(require TradingDSL))
    (eval:error (define/strategy invalid-strategy
                  (lambda (date) '())
                  #:from "2023-12-31"
                  #:to "2023-01-01")) ; Error: end date before start date
  ]
}

@defform[(define/combined-strategy id strategy1 strategy2 #:mid mid-date)]{
  Defines a new strategy that uses @racket[strategy1] up to @racket[mid-date] and then switches to @racket[strategy2] afterward.

  Compile-time checks ensure that the periods of @racket[strategy1] and @racket[strategy2] overlap at @racket[mid-date], and that @racket[mid-date] is a valid date string in the format "YYYY-MM-DD".
}

@defform[(compose-strategies strategy1 strategy2 [#:weights (w1 w2)])]{
  Creates a new strategy that combines the allocations from @racket[strategy1] and @racket[strategy2] according to the weights @racket[w1] and @racket[w2].

  If weights are not provided, a default 50/50 split is used. The resulting strategy is valid only during the period where both input strategies are active.
}

@defform[(backtest strategy start-date end-date top-n)]{
  Backtests @racket[strategy] from @racket[start-date] to @racket[end-date], considering the top @racket[top-n] stocks for investment.

  Compile-time checks ensure that the backtest period is within the strategy's active period, and that the end date is not before the start date.
}

@section{Function-Based Interface}

@defproc[(strategy [strat-fn (-> any/c (listof ticker-weight?))]
                  [#:from from-date string?]
                  [#:to to-date string?])
         any]{
  Creates a new strategy specification from @racket[strat-fn], active from @racket[from-date] to @racket[to-date].

  Unlike the macro version, validation occurs at runtime. If the end date is before the start date, an error is raised when the function is called.
}

@defproc[(combined-strategy [s1 any]
                           [s2 any]
                           [#:mid mid-date string?])
         any]{
  Creates a new strategy that uses @racket[s1] up to @racket[mid-date] and then switches to @racket[s2].

  Runtime checks verify that the strategies can be combined (i.e., their periods overlap).
}

@defproc[(compose-strategies-fn [s1 any]
                               [s2 any]
                               [#:weights weights (list/c number? number?) '(0.5 0.5)])
         any]{
  Creates a new strategy by combining @racket[s1] and @racket[s2] according to the provided @racket[weights].

  Runtime checks verify that the strategies have overlapping active periods.
}

@defproc[(backtest-fn [s any]
                     [start-date string?]
                     [end-date string?]
                     [n-val number?])
         any]{
  Backtests strategy @racket[s] from @racket[start-date] to @racket[end-date], considering the top @racket[n-val] stocks.

  Runtime checks verify that the backtest period is within the strategy's active period.
}

@section{Strategy Building Blocks}

@defproc[(top-performer [#:period period number?])
         (-> any/c (listof ticker-weight?))]{
  Creates a momentum-based strategy that selects stocks with the highest price increase over the specified @racket[period] (in days).

  The returned function takes a date and returns a sorted list of @racket[ticker-weight] structures, with higher weights for better-performing stocks.
}

@section{Data Structures}

@defstruct[ticker-weight ([ticker string?] [weight number?])]{
  Represents a stock ticker and its weight in a portfolio allocation.
}

@defstruct[reduced-date ([year number?] [month number?] [day number?])]{
  A lightweight date representation used throughout the DSL.
}

@section{Time Period Constants}

For convenience, the DSL provides several predefined time period constants (in days):

@deftogether[(
  @defthing[1y number? #:value 365]
  @defthing[6m number? #:value 182]
  @defthing[3m number? #:value 90]
  @defthing[1m number? #:value 30]
  @defthing[2w number? #:value 14]
  @defthing[1w number? #:value 7]
  @defthing[5d number? #:value 5]
  @defthing[1d number? #:value 1]
)]

@section{Visualization Functions}

@defproc[(display-strategy-allocation 
          [strategy (-> any/c (listof ticker-weight?))]
          [date any/c]
          [top-n number?])
         void?]{
  Displays the top @racket[top-n] stocks and their weights for @racket[strategy] on the specified @racket[date].
}

@defproc[(display-strategy-comparison
          [strategies (listof (-> any/c (listof ticker-weight?)))]
          [strategy-names (listof string?)]
          [date any/c]
          [top-n number?])
         void?]{
  Compares multiple strategies on a specific date, showing the top pick for each.
}

@defproc[(display-allocation-difference
          [strat1 (-> any/c (listof ticker-weight?))]
          [strat2 (-> any/c (listof ticker-weight?))]
          [name1 string?]
          [name2 string?]
          [date any/c]
          [top-n number?])
         void?]{
  Compares the allocations of two strategies, highlighting the differences in weights between them.
}