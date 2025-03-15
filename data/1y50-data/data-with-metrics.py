import pandas as pd
import numpy as np
import time
import os

def calculate_rsi(prices, window=14):
    """Calculate RSI (Relative Strength Index) for a price series.
    
    Args:
        prices (pd.Series): Series of closing prices
        window (int): Lookback window for RSI calculation (default: 14)
        
    Returns:
        pd.Series: RSI values with -1 for non-existent stocks
    """
    # Create a mask for valid values (not -1)
    valid_mask = prices != -1
    
    # Replace -1 with NaN for calculations
    prices_calc = prices.copy()
    prices_calc[~valid_mask] = np.nan
    
    # Calculate daily changes
    deltas = prices_calc.diff()
    
    # Separate gains and losses
    gains = deltas.copy()
    gains[gains < 0] = 0
    losses = -deltas.copy()
    losses[losses < 0] = 0
    
    # Calculate average gains and losses
    avg_gains = gains.rolling(window=window, min_periods=1).mean()
    avg_losses = losses.rolling(window=window, min_periods=1).mean()
    
    # Calculate RS
    rs = avg_gains / avg_losses
    
    # Calculate RSI
    rsi = 100 - (100 / (1 + rs))
    
    # Ensure -1 values result in -1 RSI
    rsi[~valid_mask] = -1
    
    # Handle NaNs for valid stocks
    rsi[valid_mask & rsi.isna()] = -1  # Not enough data
    
    return rsi

def calculate_beta(stock_returns, market_returns, window_days):
    """Calculate beta for the specified window.
    
    Args:
        stock_returns (pd.Series): Series of stock returns
        market_returns (pd.Series): Series of market returns
        window_days (int): Lookback window in trading days
        
    Returns:
        pd.Series: Beta values with NaN for insufficient data
    """
    result = pd.Series(index=stock_returns.index, dtype=float)
    dates = stock_returns.index.tolist()
    
    # For each date, calculate beta using lookback window
    for i, date in enumerate(dates):
        # Skip dates with insufficient history
        if i < 30:  # Need at least 30 data points
            result[date] = np.nan
            continue
        
        # Get the lookback window (limited by available history)
        lookback_start_idx = max(0, i - window_days)
        
        # Extract returns for the window
        stock_window = stock_returns.iloc[lookback_start_idx:i+1]
        market_window = market_returns.iloc[lookback_start_idx:i+1]
        
        # Align the series (drop any NaN values)
        valid_data = pd.concat([stock_window, market_window], axis=1).dropna()
        
        # Skip if not enough valid data
        if len(valid_data) < 30:
            result[date] = np.nan
            continue
        
        try:
            # Calculate beta using covariance/variance method
            aligned_stock = valid_data.iloc[:, 0]
            aligned_market = valid_data.iloc[:, 1]
            
            covariance = np.cov(aligned_stock, aligned_market)[0, 1]
            variance = np.var(aligned_market)
            
            if variance == 0:  # Avoid division by zero
                result[date] = np.nan
            else:
                beta = covariance / variance
                result[date] = beta
        except Exception as e:
            print(f"  Error calculating beta for date {date}: {e}")
            result[date] = np.nan
    
    return result

def process_stock_data(input_file, output_file):
    """Process stock data to add RSI and Beta columns.
    
    Args:
        input_file (str): Path to input CSV file
        output_file (str): Path to output CSV file
    """
    start_time = time.time()
    print(f"Starting process at {time.strftime('%Y-%m-%d %H:%M:%S')}...")
    
    # Check if input file exists
    if not os.path.exists(input_file):
        print(f"Error: Input file '{input_file}' not found.")
        return
    
    # Read the CSV file
    print(f"Reading data from {input_file}...")
    try:
        df = pd.read_csv(input_file)
    except Exception as e:
        print(f"Error reading file: {e}")
        return
    
    # Validate expected columns
    required_columns = ['Date', 'Ticker', 'Open', 'Close']
    missing_columns = [col for col in required_columns if col not in df.columns]
    if missing_columns:
        print(f"Error: Missing required columns: {missing_columns}")
        return
    
    # Convert date column to datetime
    df['Date'] = pd.to_datetime(df['Date'])
    
    # Sort data for time series calculations
    df = df.sort_values(['Ticker', 'Date'])
    
    print(f"Processing {len(df)} rows for {df['Ticker'].nunique()} tickers...")
    
    # Create pivot table for calculations
    pivot_df = df.pivot(index='Date', columns='Ticker', values='Close')
    
    # Create market proxy (average of all valid stocks)
    print("Creating market proxy for beta calculations...")
    market_prices = pivot_df.replace(-1, np.nan).mean(axis=1)
    market_returns = market_prices.pct_change().fillna(0)
    
    # Calculate returns for each stock
    stock_returns = pivot_df.replace(-1, np.nan).pct_change().fillna(0)
    
    # Ensure stocks with -1 values have NaN returns
    stock_returns[pivot_df == -1] = np.nan
    
    # Calculate RSI for each ticker
    print("Calculating RSI values...")
    rsi_values = pd.DataFrame(index=pivot_df.index, columns=pivot_df.columns)
    
    ticker_count = len(pivot_df.columns)
    for i, ticker in enumerate(pivot_df.columns):
        print(f"  Calculating RSI for {ticker} ({i+1}/{ticker_count})")
        rsi_values[ticker] = calculate_rsi(pivot_df[ticker], window=14)
    
    # Initialize beta dataframes
    beta_1y = pd.DataFrame(index=pivot_df.index, columns=pivot_df.columns)
    beta_5y = pd.DataFrame(index=pivot_df.index, columns=pivot_df.columns)
    
    # Calculate betas for each ticker
    print("Calculating Beta values...")
    
    for i, ticker in enumerate(pivot_df.columns):
        print(f"  Calculating Beta for {ticker} ({i+1}/{ticker_count})")
        
        # Skip tickers with all -1 values
        if (pivot_df[ticker] == -1).all():
            print(f"    All values are -1, skipping calculations")
            beta_1y[ticker] = -1
            beta_5y[ticker] = -1
            continue
        
        # Get ticker returns
        ticker_returns = stock_returns[ticker]
        
        # 1-year beta (252 trading days)
        print(f"    Calculating 1-year beta for {ticker}...")
        beta_1y[ticker] = calculate_beta(ticker_returns, market_returns, 252)
        
        # 5-year beta (1260 trading days)
        print(f"    Calculating 5-year beta for {ticker}...")
        beta_5y[ticker] = calculate_beta(ticker_returns, market_returns, 1260)
        
        # Ensure -1 entries have -1 betas
        invalid_mask = pivot_df[ticker] == -1
        beta_1y.loc[invalid_mask, ticker] = -1
        beta_5y.loc[invalid_mask, ticker] = -1
    
    # Clean up any NaN, inf values
    beta_1y = beta_1y.replace([np.inf, -np.inf, np.nan], -1)
    beta_5y = beta_5y.replace([np.inf, -np.inf, np.nan], -1)
    
    # Convert back to long format
    print("Reformatting results...")
    
    # Reset index on pivoted dataframes
    rsi_values = rsi_values.reset_index()
    beta_1y = beta_1y.reset_index()
    beta_5y = beta_5y.reset_index()
    
    # Melt to long format
    rsi_long = pd.melt(rsi_values, id_vars=['Date'], var_name='Ticker', value_name='RSI_14')
    beta_1y_long = pd.melt(beta_1y, id_vars=['Date'], var_name='Ticker', value_name='Beta_1Y')
    beta_5y_long = pd.melt(beta_5y, id_vars=['Date'], var_name='Ticker', value_name='Beta_5Y')
    
    # Merge with original dataframe
    print("Merging results with original data...")
    result = df.merge(rsi_long, on=['Date', 'Ticker'], how='left')
    result = result.merge(beta_1y_long, on=['Date', 'Ticker'], how='left')
    result = result.merge(beta_5y_long, on=['Date', 'Ticker'], how='left')
    
    # Final check to ensure all -1 stocks have -1 for all metrics
    invalid_mask = (result['Open'] == -1) & (result['Close'] == -1)
    result.loc[invalid_mask, ['RSI_14', 'Beta_1Y', 'Beta_5Y']] = -1
    
    # Save the result
    print(f"Saving results to {output_file}...")
    result.to_csv(output_file, index=False)
    
    elapsed_time = time.time() - start_time
    print(f"Process completed in {elapsed_time/60:.2f} minutes.")
    print(f"Added columns: RSI_14, Beta_1Y, Beta_5Y")
    print(f"Output file: {output_file}")

if __name__ == "__main__":
    # File paths
    input_file = '1y50.csv'
    output_file = 'pricedata-1y-with-metrics.csv'
    
    # Process the data
    process_stock_data(input_file, output_file)