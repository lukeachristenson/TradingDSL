from datetime import datetime, timedelta
import pandas as pd
from pricedata import get_price
import csv


def is_date_before(date1, date2):
    '''
    Checks if date1 is before date2.

    Args:
        date1 (str): The first date string in 'yyyy-mm-dd' format.
        date2 (str): The second date string in 'yyyy-mm-dd' format.

    Returns:
        bool: True if date1 is before date2, False otherwise.
    '''
    try:
        # Parse the date strings into datetime objects
        date1_obj = datetime.strptime(date1, "%Y-%m-%d")
        date2_obj = datetime.strptime(date2, "%Y-%m-%d")
        # Compare the dates
        return date1_obj < date2_obj
    except ValueError:
        raise ValueError("Invalid date format. Please use 'yyyy-mm-dd'.")


def years_difference(date1: str, date2: str) -> float:
    '''
    Calculates the difference in years (including decimals) between two dates.

    :param date1: The earlier date as a string in the format "YYYY-MM-DD".
    :param date2: The later date as a string in the format "YYYY-MM-DD".
    :return: The difference in years as a float.
    '''
    # Parse the date strings into datetime objects
    d1 = datetime.strptime(date1, "%Y-%m-%d")
    d2 = datetime.strptime(date2, "%Y-%m-%d")

    # Calculate the total difference in days
    delta_days = (d2 - d1).days

    # Convert the difference in days to years (accounting for leap years)
    years = delta_days / 365.25

    return years


def add_one_day(date_str):
    '''
    Adds one day to the given date string in 'yyyy-mm-dd' format.

    Args:
        date_str (str): The input date string.

    Returns:
        str: The date string with one day added, in 'yyyy-mm-dd' format.
    '''
    try:
        # Parse the input string into a datetime object
        date_obj = datetime.strptime(date_str, "%Y-%m-%d")
        # Add one day
        new_date_obj = date_obj + timedelta(days=1)
        # Format back to string and return
        return new_date_obj.strftime("%Y-%m-%d")
    except ValueError:
        raise ValueError("Invalid date format. Please use 'yyyy-mm-dd'.")

def is_weekend_or_holiday(date_str: str):
    '''
    Determine if a given date is a weekend or a U.S. nationally recognized holiday.
    Assuming that if data not available for AAPL, must be holiday/market close day
    
    Args:
        date_str (str): The date in the format "YYYY-MM-DD".
        
    Returns:
        bool: True if the date is a weekend or a holiday, False otherwise.
    '''
    try:
        aapl_price = get_price("AAPL", date_str, "")


        return aapl_price == -1
    
    except Exception as e:
        return True # if exception, assume that aapl doesn't have data for some reason, so market holiday


start_date = "2020-01-01"
end_date = "2024-12-31"
bucket = ["AAPL", "MSFT", "PLTR"]

cur_date = start_date
data = [["Ticker", "Open", "Close"]]

while is_date_before(cur_date, end_date):
    cur_date = add_one_day(cur_date)
    if (is_weekend_or_holiday(cur_date)):
        continue

    for ticker in bucket:
        open_price = get_price(ticker, cur_date, open=True)
        close_price = get_price(ticker, cur_date, open=False)
        data.append([ticker, open_price, close_price])

# Save to CSV
with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)