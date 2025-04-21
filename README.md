# TradingDSL

A domain-specific language for defining, testing, and analyzing stock trading strategies in Racket.

## Overview

TradingDSL makes it easy to define trading strategies, test them against historical data, and analyze their performance. It provides a clean, declarative syntax for specifying strategies, their active periods, and how they should be combined.

The DSL supports:

- **Strategy Definition**: Define strategies with explicit active periods
- **Strategy Composition**: Combine strategies by time period or weighted allocation
- **Backtesting**: Test strategies against historical stock data
- **Visualization**: Compare strategies and analyze allocations

## Example

```racket
#lang racket
(require trading-dsl)

;; Define an annual momentum strategy (active Jan-Jun 2024)
(define/strategy annual-momentum 
  (top-performer #:period 1y)
  #:from "2024-01-05" 
  #:to "2024-06-05")

;; Define a monthly momentum strategy (active Jan-Dec 2024)
(define/strategy monthly-momentum 
  (top-performer #:period 1m)
  #:from "2024-01-05" 
  #:to "2024-12-20")

;; Combine strategies with weights
(define/strategy balanced-momentum
  (compose-strategies annual-momentum monthly-momentum
                     #:weights (0.6 0.4))
  #:from "2024-01-05"
  #:to "2024-06-05")

;; Backtest the strategy
(displayln (backtest balanced-momentum 
                    "2024-01-05" 
                    "2024-06-05" 
                    5))

;; Display allocation for a specific date
(display-strategy-allocation 
  balanced-momentum 
  (reduced-date 2024 2 15) 
  10)
```

The DSL provides both macro-based and function-based implementations:

```racket
;; Function-based alternative
(define balanced-momentum-fn
  (strategy (top-performer #:period 1y)
            #:from "2024-01-05"
            #:to "2024-06-05"))
            
;; Function-based backtest
(backtest-fn balanced-momentum-fn
            "2024-01-05"
            "2024-06-05"
            5)
```

## Key Features

- **Compile-time Validation**: The macro-based implementation checks date ranges and strategy compatibility at compile time
- **Runtime Alternative**: Function-based implementation provides same features with runtime checking
- **Strategy Composition**: Combine strategies by time period or weighted allocation
- **Historical Backtesting**: Test strategies against included stock data
- **Visualization Tools**: Compare strategies and analyze differences

## Installation

Install from the command line:

```
raco pkg install
cd TradingDSL
```

Or directly from GitHub:

```
raco pkg install git://github.com/USERNAME/TradingDSL
```

## Documentation

Full documentation is available after installation:

```
raco docs trading-dsl
```

In case that does not work (weird package setup issues due to naming change), run 
```
raco scribble scribblings/main.scrbl
```

Then open the generated index.html file in scribblings/main/index.html

For implementation details, see the [developer documentation](private/README.md).

## License

MIT