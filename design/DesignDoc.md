# Trading Strategy DSL - Design Document

## Purpose and Concepts

Our DSL is designed to implement and test trading strategies, focusing on forecasting future performance and backtesting historical data to evaluate their effectiveness.

### Key Concepts
- **Backtesting** – Testing a strategy using historical data to evaluate its past performance.
- **Indicator** – A function or metric used to determine trading signals (e.g., moving averages, RSI, Beta).
- **Strategy** – A composition of indicators that generates stock selection and weighting decisions.

### Computation Involved
- **Filtering** – Selecting relevant stocks based on conditions.
- **Sorting** – Ranking stocks by performance metrics.
- **Weight Calculation** – Assigning a confidence score to stocks based on strategy criteria.

---

## Example Programs in the DSL

### Defining Simple Strategies

```scheme
(require top-performer) ; Imported from the DSL
(define 1y-tp-strat (top-performer #:1y))
(define 6m-tp-strat (top-performer #:6m))
(define 5d-tp-strat (top-performer #:5d))
```

```scheme
(require high-beta) ; Imported from the DSL
(define beta-strat-1 
  (high-beta #:interval 1y  
    (lambda (x) (cond [(> x 5) 0]
                       [else (- 1 (/ x 5))]))))

(define beta-strat-2 
  (high-beta #:interval 3y  
    (lambda (x) (cond [(< x 2) 0]
                       [(> x 5) 0]
                       [else (/ x 5)]))))
```

### Composing Strategies

```scheme
; Compose function combines two strategies with a user-defined function
(define comp1 (compose 6m-tp-strat beta-strat-1 
  (lambda (x y) (/ (+ x y) 2))))

(define comp2 (compose 5d-tp-strat beta-strat-2 
  (lambda (x y) (+ (* x 0.4) (* y 0.6)))))
```

### Backtesting Strategies

```scheme
(backtest beta-strat-1 #:from date1 #:to date2) ; Returns performance metrics
(backtest comp1 #:from date1 #:to date2)
```

---

## Grammars and Signatures

A **strategy (strat)** takes in market data over a given time period and returns forecasted future performance weights for stocks after that period.

### Example Signature

```scheme
(top-performer #:interval <time-period>) → strat
(high-beta #:interval <time-period> <weight-function>) → strat
(compose strat1 strat2 <composition-function>) → strat
(backtest strat #:from <start-date> #:to <end-date>) → performance-metrics
```

---

## Implementation Milestones

Our approach follows a **bottom-up development strategy**, starting with DSL functionality and layering syntax/macros for usability.

### 1) Establish Core DSL Macros
We will begin by defining macros that simplify the creation of trading strategies. This ensures that we have a solid foundation for strategy composition before integrating more complex syntax or data manipulation. Macros will be used to streamline the process of defining and manipulating trading strategies, making the DSL both functional and intuitive from the outset.

### 2) Data Acquisition and Formatting
Next, we will obtain and preprocess the necessary stock market data. Our primary data source will be **Yahoo Finance**, from which we will download CSV files containing historical stock prices and performance metrics. The data will then be structured into a format that is convenient for strategies to operate on.

The goal is to transform raw CSV data into the following format:

```scheme
(Map Date Options)

; Options is a map of stock tickers to weights
(Map Ticker Weight)

; Data structure for each stock
(Map Date
  (listof
    (Map Ticker
      (listof Price, P/E, Beta, RSI))))
```

The **ticker** represents an individual stock, and the **weight** is a confidence score (out of 100) that reflects how promising the stock is according to the strategy’s selection criteria.

### 3) Implement Default Strategies
Once the data structure is in place, we will implement simple **default strategies** that users can use as building blocks. One such strategy will be a **greedy strategy**, which selects stocks based solely on historical return performance. Another example is **beta scaling**, which adjusts stock weights based on their beta values.

**Example:**

```scheme
(define beta-strat-2 
  (high-beta #:interval 3y  
    (lambda (x) (cond [(< x 2) 0]
                       [(> x 5) 0]
                       [else (/ x 5)]))))
```

### 4) Implement Strategy Composition
We will develop a **composition system** that enables combining multiple strategies into a single decision-making framework. Each strategy will assign a **weight** to stocks based on its criteria, and the composition function will allow users to mix and adjust strategy weights to refine stock selection.

Each strategy outputs a **score between 0 and 100**, and users can assign relative importance to different strategies using a **weighting function**. The result is a **blended stock selection strategy**.

**Example:**

```scheme
(compose strat1 strat2 (lambda (x y) (+ (* x 0.4) (* y 0.6))))
```

### 5) Implement Backtesting Functionality
Once strategy composition is functional, we will implement a **backtesting system** to evaluate strategy effectiveness. This will involve applying a strategy to **historical data** and analyzing how well it would have predicted **future stock performance**.

The evaluation will be based on:
- The return on investment (ROI) of the strategy compared to a market benchmark (e.g., **S&P 500**).
- A user-defined selection mechanism (e.g., **picking the top 15 weighted stocks from the strategy**).
- Performance metrics such as volatility, Sharpe ratio, and drawdown.

By running backtests, users will gain insight into how well a strategy **performs in different market conditions** and can iterate on their models accordingly.

### 6) Finalizing Syntax and Usability
After backtesting functionality is complete, we will **review and refine the DSL syntax** to ensure usability and efficiency. This involves:
- Ensuring all key functions have **consistent naming conventions**.
- Verifying that users can define and test strategies with minimal friction.
- Adding documentation and examples to facilitate adoption.

At this stage, we will also incorporate **any feedback from our design review** and make necessary improvements before finalizing the implementation.

---

*This document will evolve as we receive feedback and refine our implementation.*