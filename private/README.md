@ -1,89 +0,0 @@
# TradingDSL Implementation Architecture

This document describes the internal architecture of the TradingDSL implementation for developers and contributors.

## Overview

TradingDSL is implemented using two approaches:
1. A macro-based approach using syntax-spec that enforces constraints at compile time
2. A function-based approach that provides similar capabilities with runtime checking

## Component Structure

### Core Components

The implementation is divided into the following components:

1. **Data Management** (`private/data-new.rkt`)
   - Date handling and conversion
   - Data retrieval from CSV sources
   - Stock data representation

2. **Strategy Implementation** (`private/strat.rkt`)
   - Core strategy representation
   - `ticker-weight` structure
   - Momentum calculation
   - Strategy factories (e.g., `top-performer`)

3. **Backtesting Engine** (`private/backtest.rkt`)
   - Trading day simulation
   - Portfolio management
   - Return calculation

4. **Visualization Tools** (`private/visualization.rkt`)
   - Strategy allocation display
   - Strategy comparison
   - Portfolio difference analysis

5. **DSL Implementation**
   - Macro-based (`TradingDSL.rkt`): Uses syntax-spec for compile-time validation
   - Function-based (`FunctionDSL.rkt`): Uses functions with runtime validation

### Public Interface

The DSL provides two primary interfaces:

1. **Macro-based Interface**:
   - `define/strategy`: Define a trading strategy with a specific active period
   - `define/combined-strategy`: Create a strategy that switches between two existing strategies
   - `compose-strategies`: Combine two strategies with optional weights
   - `backtest`: Run a backtest on a strategy over a specific time period

2. **Function-based Interface**:
   - `strategy`: Create a strategy with a specific active period
   - `combined-strategy`: Create a switch-based combined strategy
   - `compose-strategies-fn`: Weight-based strategy composition
   - `backtest-fn`: Run a backtest with runtime validation

## Data Flow

1. Strategy definitions specify a function that generates allocations for a given date
2. When a backtest is requested:
   - The date range is validated against the strategy's active period
   - Trading days in the range are identified
   - For each day, the strategy function is called to get that day's allocation
   - Returns are calculated based on allocation changes
3. Results can be displayed using the visualization tools

## Compile-Time vs. Runtime

The two implementations differ in when they perform validation:

### Macro Implementation (compile-time)
1. Date ranges are validated during macro expansion
2. Strategy periods are stored in a symbol table for use during compilation
3. Errors in strategy definition or usage are caught before execution

### Function Implementation (runtime)
1. Date ranges are validated when functions are called
2. Strategy periods are stored in strategy-spec structures
3. Errors are caught during execution

## Extension Points

To add new features to TradingDSL:

1. **New Strategy Types**: Add new strategy factory functions to `strat.rkt`
2. **Additional Metrics/utility functions**: Extend `data-new.rkt` with new metrics or data sources
3. **Enhanced Visualization**: Add new display functions to `visualization.rkt`
4. **New Composition Methods**: Add new ways to combine strategies in both implementations