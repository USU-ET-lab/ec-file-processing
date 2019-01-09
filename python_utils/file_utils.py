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
    Date parser for pandas.read_csv when yr, doy, hr, and sec are the time columns
    '''
    if '2400' in hr:
        hr = '000'
        return pd.datetime.strptime(f'{yr}{int(doy)+1}{int(hr):04}{float(sec):.2f}', '%Y%j%H%M%S.%f')
    else:
        return pd.datetime.strptime(f'{yr}{doy}{int(hr):04}{float(sec):.2f}', '%Y%j%H%M%S.%f')

def fort_date_hours(yr,doy,hr):
    '''
    Date parser for pandas.read_csv when yr, doy, and hr are the time columns
    '''
    if '2400' in hr:
        hr = '000'
        return pd.datetime.strptime(f'{yr}{int(doy)+1}{int(hr):04}', '%Y%j%H%M')
    else:
        return pd.datetime.strptime(f'{yr}{doy}{int(hr):04}', '%Y%j%H%M')
    
def merge_cols(df,old_cols,new_col):
    '''
    Merges multiple columns with non-overlapping points together
    '''
    
    merged = pd.concat([df[old] for old in old_cols]).rename(new_col).dropna()
    merged = merged.where(merged<1.)
    return merged