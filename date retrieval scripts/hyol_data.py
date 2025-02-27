from datetime import datetime, timedelta
import pandas as pd
from pricedata import get_price, save_get_price_cache
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
bucket = []

cur_date = start_date
data = [["Date", "Ticker", "Open", "Close"]]


bucket = "A,AAPL,ABBV,ABNB,ABT,ACGL,ACN,ADBE,ADI,ADM,ADP,ADSK,AEE,AEP,AES,AFL,AIG,AIZ,AJG,AKAM,ALB,ALGN,ALL,ALLE,AMAT,AMCR,AMD,AME,AMGN,AMP,AMT,AMTM,AMZN,ANET,ANSS,AON,AOS,APA,APD,APH,APTV,ARE,ATO,AVB,AVGO,AVY,AWK,AXON,AXP,AZO,BA,BAC,BALL,BAX,BBY,BDX,BEN,BF-B,BG,BIIB,BK,BKNG,BKR,BLDR,BLK,BMY,BR,BRK-B,BRO,BSX,BWA,BX,BXP,C,CAG,CAH,CARR,CAT,CB,CBOE,CBRE,CCI,CCL,CDNS,CDW,CE,CEG,CF,CFG,CHD,CHRW,CHTR,CI,CINF,CL,CLX,CMCSA,CME,CMG,CMI,CMS,CNC,CNP,COF,COO,COP,COR,COST,CPAY,CPB,CPRT,CPT,CRL,CRM,CRWD,CSCO,CSGP,CSX,CTAS,CTLT,CTRA,CTSH,CTVA,CVS,CVX,CZR,D,DAL,DAY,DD,DE,DECK,DELL,DFS,DG,DGX,DHI,DHR,DIS,DLR,DLTR,DOC,DOV,DOW,DPZ,DRI,DTE,DUK,DVA,DVN,DXCM,EA,EBAY,ECL,ED,EFX,EG,EIX,EL,ELV,EMN,EMR,ENPH,EOG,EPAM,EQIX,EQR,EQT,ERIE,ES,ESS,ETN,ETR,EVRG,EW,EXC,EXPD,EXPE,EXR,F,FANG,FAST,FCX,FDS,FDX,FE,FFIV,FI,FICO,FIS,FITB,FMC,FOX,FOXA,FRT,FSLR,FTNT,FTV,GD,GDDY,GE,GEHC,GEN,GEV,GILD,GIS,GL,GLW,GM,GNRC,GOOG,GOOGL,GPC,GPN,GRMN,GS,GWW,HAL,HAS,HBAN,HCA,HD,HES,HIG,HII,HLT,HOLX,HON,HPE,HPQ,HRL,HSIC,HST,HSY,HUBB,HUM,HWM,IBM,ICE,IDXX,IEX,IFF,INCY,INTC,INTU,INVH,IP,IPG,IQV,IR,IRM,ISRG,IT,ITW,IVZ,J,JBHT,JBL,JCI,JKHY,JNJ,JNPR,JPM,K,KDP,KEY,KEYS,KHC,KIM,KKR,KLAC,KMB,KMI,KMX,KO,KR,KVUE,L,LDOS,LEN,LH,LHX,LIN,LKQ,LLY,LMT,LNT,LOW,LRCX,LULU,LUV,LVS,LW,LYB,LYV,MA,MAA,MAR,MAS,MCD,MCHP,MCK,MCO,MDLZ,MDT,MET,META,MGM,MHK,MKC,MKTX,MLM,MMC,MMM,MNST,MO,MOH,MOS,MPC,MPWR,MRK,MRNA,MS,MSCI,MSFT,MSI,MTB,MTCH,MTD,MU,NCLH,NDAQ,NDSN,NEE,NEM,NFLX,NI,NKE,NOC,NOW,NRG,NSC,NTAP,NTRS,NUE,NVDA,NVR,NWS,NWSA,NXPI,O,ODFL,OKE,OMC,ON,ORCL,ORLY,OTIS,OXY,PANW,PARA,PAYC,PAYX,PCAR,PCG,PEG,PEP,PFE,PFG,PG,PGR,PH,PHM,PKG,PLD,PLTR,PM,PNC,PNR,PNW,PODD,POOL,PPG,PPL,PRU,PSA,PSX,PTC,PWR,PYPL,QCOM,QRVO,RCL,REG,REGN,RF,RJF,RL,RMD,ROK,ROL,ROP,ROST,RSG,RTX,RVTY,SBAC,SBUX,SCHW,SHW,SJM,SLB,SMCI,SNA,SNPS,SO,SOLV,SPG,SPGI,SRE,STE,STLD,STT,STX,STZ,SW,SWK,SWKS,SYF,SYK,SYY,T,TAP,TDG,TDY,TECH,TEL,TER,TFC,TFX,TGT,TJX,TMO,TMUS,TPL,TPR,TRGP,TRMB,TROW,TRV,TSCO,TSLA,TSN,TT,TTWO,TXN,TXT,TYL,UAL,UBER,UDR,UHS,ULTA,UNH,UNP,UPS,URI,USB,V,VICI,VLO,VLTO,VMC,VRSK,VRSN,VRTX,VST,VTR,VTRS,VZ,WAB,WAT,WBA,WBD,WDC,WEC,WELL,WFC,WM,WMB,WMT,WRB,WST,WTW,WY,WYNN,XEL,XOM,XYL,YUM,ZBH,ZBRA,ZTS".split(",")

while is_date_before(cur_date, end_date):
    cur_date = add_one_day(cur_date)
    if (is_weekend_or_holiday(cur_date)):
        continue

    for ticker in bucket:
        open_price = get_price(ticker, cur_date, open=True)
        close_price = get_price(ticker, cur_date, open=False)
        data.append([cur_date, ticker, open_price, close_price])

# Save to CSV
with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)



save_get_price_cache()