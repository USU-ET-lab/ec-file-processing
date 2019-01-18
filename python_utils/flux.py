import pandas as pd
import numpy as np
import datetime as dt
import glob
from . file_utils import fort_date_hours

def fort_to_csv(in_files, out_file, in_kwargs=None, out_kwargs=None):
    '''
    Read in list of files from flux_calc.exe, fix timestamps, and output to .csv
    '''
    
    if not in_kwargs:
        
        in_kwargs = dict(parse_dates={'TIMESTAMP':[0,1,2]}, date_parser=fort_date_hours,
                        na_values=['*****','******','NAN'], index_col=['TIMESTAMP'],  
                        delim_whitespace=True, header=0, skipfooter=3, engine='python')
    
    
    
    #Read in files, stripping last 3 lines that contain daily ET values
    in_df = pd.concat([pd.read_csv(f,**in_kwargs) for f in in_files])
    in_df.to_csv(out_file)
    return in_df