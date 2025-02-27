#!/usr/bin/env python3
"""
Trading Strategy DSL - Data Retrieval Script
Retrieves stock data from Yahoo Finance and formats it for use with the DSL
"""

import pandas as pd
import yfinance as yf
import numpy as np
import datetime
import os
from tqdm import tqdm
import talib
import argparse

def get_sp500_tickers():
    """Retrieves the list of S&P 500 tickers from Wikipedia."""
    try:
        sp500_url = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
        table = pd.read_html(sp500_url)
        sp500_tickers = table[0]["Symbol"].tolist()
        
        # Clean tickers (remove dots, convert to uppercase)
        sp500_tickers = [ticker.replace('.', '-').upper() for ticker in sp500_tickers]
        
        print(f"Retrieved {len(sp500_tickers)} S&P 500 tickers.")
        return sp500_tickers
    except Exception as e:
        print(f"Error retrieving S&P 500 tickers: {e}")
        return []

def download_market_data(tickers, start_date, end_date, limit=None):
    """
    Downloads historical data for the specified tickers.
    
    Args:
        tickers: List of stock tickers
        start_date: Start date for data retrieval
        end_date: End date for data retrieval
        limit: Optional limit on number of tickers to process
    
    Returns:
        DataFrame with historical data
    """
    if limit:
        tickers = tickers[:limit]
        print(f"Limited to {limit} tickers for testing.")
    
    # Download SPY data for benchmark and beta calculation
    print("Downloading SPY data for market benchmark...")
    spy_data = yf.download("SPY", start=start_date, end=end_date)
    spy_data['Returns'] = spy_data['Adj Close'].pct_change()
    
    all_data = []
    
    print("Downloading stock data...")
    for ticker in tqdm(tickers):
        try:
            # Download basic price data
            stock_data = yf.download(ticker, start=start_date, end=end_date, progress=False)
            
            if stock_data.empty or len(stock_data) < 20:
                continue
                
            # Calculate daily returns
            stock_data['Returns'] = stock_data['Adj Close'].pct_change()
            
            # Get stock info for P/E ratio
            try:
                stock_info = yf.Ticker(ticker).info
                pe_ratio = stock_info.get('trailingPE', np.nan)
                if pe_ratio is None:
                    pe_ratio = np.nan
            except:
                pe_ratio = np.nan
            
            # Calculate beta (120-day rolling window)
            merged_data = pd.merge(
                stock_data['Returns'], 
                spy_data['Returns'], 
                left_index=True, 
                right_index=True, 
                suffixes=('', '_spy')
            )
            
            # Calculate beta using 120-day rolling window
            stock_data['Beta'] = merged_data['Returns'].rolling(window=120).cov(merged_data['Returns_spy']) / \
                                merged_data['Returns_spy'].rolling(window=120).var()
            
            # Calculate RSI (14-day)
            if len(stock_data) > 14:
                try:
                    stock_data['RSI'] = talib.RSI(stock_data['Adj Close'], timeperiod=14)
                except:
                    # If talib not available, use a simple implementation
                    delta = stock_data['Adj Close'].diff()
                    gain = (delta.where(delta > 0, 0)).rolling(window=14).mean()
                    loss = (-delta.where(delta < 0, 0)).rolling(window=14).mean()
                    rs = gain / loss
                    stock_data['RSI'] = 100 - (100 / (1 + rs))
            else:
                stock_data['RSI'] = np.nan
            
            # Add P/E column to all rows
            stock_data['PE'] = pe_ratio
            
            # Add ticker column
            stock_data['Ticker'] = ticker
            
            # Append to our list
            all_data.append(stock_data)
            
        except Exception as e:
            print(f"Failed to download {ticker}: {e}")
    
    # Combine all stock data
    if all_data:
        final_df = pd.concat(all_data)
        return final_df
    else:
        print("No data was downloaded.")
        return None

def format_for_dsl(df):
    """
    Formats the DataFrame to match the DSL's expected structure.
    
    Output format follows this structure:
    Date,Ticker,Price,PE,Beta,RSI,Volume
    """
    # Reset index to get Date as a column
    df = df.reset_index()
    
    # Select and rename columns
    formatted_df = df[['Date', 'Ticker', 'Adj Close', 'PE', 'Beta', 'RSI', 'Volume']].copy()
    formatted_df = formatted_df.rename(columns={'Adj Close': 'Price'})
    
    # Convert date to string format
    formatted_df['Date'] = formatted_df['Date'].dt.strftime('%Y-%m-%d')
    
    # Fill NaN values with reasonable defaults
    formatted_df['PE'] = formatted_df['PE'].fillna(-1)  # -1 indicates no P/E data
    formatted_df['Beta'] = formatted_df['Beta'].fillna(1.0)  # Default beta is 1.0
    formatted_df['RSI'] = formatted_df['RSI'].fillna(50)  # Neutral RSI
    
    return formatted_df

def create_additional_timeframes(df):
    """
    Creates additional CSV files for different timeframes (1y, 6m, 3m).
    """
    # Get the latest date in the data
    latest_date = pd.to_datetime(df['Date']).max()
    
    # Calculate cutoff dates for different timeframes
    one_year_ago = latest_date - pd.DateOffset(years=1)
    six_months_ago = latest_date - pd.DateOffset(months=6)
    three_months_ago = latest_date - pd.DateOffset(months=3)
    
    # Create filtered DataFrames
    df['Date'] = pd.to_datetime(df['Date'])
    df_1y = df[df['Date'] >= one_year_ago].copy()
    df_6m = df[df['Date'] >= six_months_ago].copy()
    df_3m = df[df['Date'] >= three_months_ago].copy()
    
    # Convert dates back to string format
    df_1y['Date'] = df_1y['Date'].dt.strftime('%Y-%m-%d')
    df_6m['Date'] = df_6m['Date'].dt.strftime('%Y-%m-%d')
    df_3m['Date'] = df_3m['Date'].dt.strftime('%Y-%m-%d')
    
    return df_1y, df_6m, df_3m

def main():
    parser = argparse.ArgumentParser(description='Download market data for trading strategy DSL')
    parser.add_argument('--years', type=int, default=5, help='Number of years of historical data to download')
    parser.add_argument('--output', type=str, default='market_data', help='Output directory')
    parser.add_argument('--limit', type=int, help='Limit number of tickers (for testing)')
    
    args = parser.parse_args()
    
    # Create output directory if it doesn't exist
    if not os.path.exists(args.output):
        os.makedirs(args.output)
    
    # Calculate date range
    end_date = datetime.datetime.today().strftime('%Y-%m-%d')
    start_date = (datetime.datetime.today() - datetime.timedelta(days=args.years*365)).strftime('%Y-%m-%d')
    
    print(f"Downloading data from {start_date} to {end_date}")
    
    # Get tickers
    tickers = get_sp500_tickers()
    
    # Download data
    df = download_market_data(tickers, start_date, end_date, args.limit)
    
    if df is not None:
        # Format data for DSL
        formatted_df = format_for_dsl(df)
        
        # Save full dataset
        full_output_path = os.path.join(args.output, 'market_data_full.csv')
        formatted_df.to_csv(full_output_path, index=False)
        print(f"Saved full dataset to {full_output_path}")
        
        # Create and save timeframe-specific datasets
        df_1y, df_6m, df_3m = create_additional_timeframes(formatted_df)
        
        df_1y.to_csv(os.path.join(args.output, 'market_data_1y.csv'), index=False)
        df_6m.to_csv(os.path.join(args.output, 'market_data_6m.csv'), index=False)
        df_3m.to_csv(os.path.join(args.output, 'market_data_3m.csv'), index=False)
        
        print(f"Created timeframe-specific datasets in {args.output}")

if __name__ == "__main__":
    main()