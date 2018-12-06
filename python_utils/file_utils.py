import pandas as pd
import numpy as np
import datetime as dt

def read_header(in_url):
    
    '''
    Read in header from csv file
    
    Keyword arguments:
    in_dir : url for header file
    
    Returns:
    ts_header: list of variable names to be used as header in pd.read_csv
    
    General header format:
    var1,vqr2,var3,...,varn
    unit1,unit2,unit3,...,unitn
    '''
    
    with open(in_url) as f:
        reader = csv.reader(f)
        ts_header = next(reader)
        ts_units = next(reader)
        f.close()
    
    return ts_header, ts_units

def fort_date_secs(yr,doy,hr,sec):
    '''
    Date parser for pandas.read_csv when year, doy, hrmin, and sec columns are there
    '''
    return pd.datetime.strptime(f'{yr}{doy}{int(hr):04}{float(sec):.2f}', '%Y%j%H%M%S.%f')

def fort_dates_hours(yr,doy,hr):
    '''
    
    '''
    return pd.datetime.strptime(f'{yr}{doy}{int(hr):04}', '%Y%j%H%M')