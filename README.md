# TradingDSL

Our DSL is designed to implement and test trading strategies, focusing on forecasting future performance and backtesting historical data to evaluate their effectiveness.

## Project Structure

- `trading-dsl.rkt`: Core DSL definition file including syntax-spec macros
- `example-trading.rkt`: Example usage of the DSL
- `racket-code/`: Supporting Racket code
  - `data-new.rkt`: Data handling and date manipulation utilities
  - `strat.rkt`: Trading strategy implementation
  - `backtest.rkt`: Backtesting functionality
- `data/`: Stock market data for testing strategies
  - `1y50-data/`: 1-year data for 50 stocks
  - `5y500-data/`: 5-year data for 500 stocks

## Core Concepts

### Strategies

A strategy is a function that takes a date and returns a list of ticker-weight pairs, representing the allocation of assets at that date.

### Time Periods

The DSL provides constants for common time periods:
- `1y` (365 days)
- `6m` (182 days)
- `3m` (90 days)
- `1m` (30 days)
- `2w` (14 days)
- `1w` (7 days)
- `5d` (5 days)
- `1d` (1 day)

### Key Features

1. Define strategies with trading periods
2. Combine strategies with time-based switching
3. Compose strategies with weighted combinations
4. Backtest strategies over specific time ranges

## DSL Syntax

### Define a Strategy

```racket
(define/strategy strategy-name strategy-function
  #:from "YYYY-MM-DD" 
  #:to "YYYY-MM-DD")
```

### Combine Strategies

```racket
(define/combined-strategy new-strategy
  strategy1
  strategy2
  #:mid "YYYY-MM-DD")
```

### Compose Strategies with Weights

```racket
(compose-strategies strategy1 strategy2
  #:weights (0.7 0.3))
```

### Backtest a Strategy

```racket
(backtest strategy
  "YYYY-MM-DD"  ; start date
  "YYYY-MM-DD"  ; end date
  5)            ; number of top stocks to use
```

## Example Usage

See `example-trading.rkt` for complete examples of how to use the DSL.