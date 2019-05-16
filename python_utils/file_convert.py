'''

@author: miksch
'''

import pandas as pd
import numpy as np
import dask.dataframe as dd
import dask
import os
import csv
import multiprocessing
from dask.distributed import Client
from . file_utils import read_header

    
class toa5_convert_dask(object):
    '''
    Class used to read multiple toa5 files either as globstring or list of urls, then output to
    csv (pandas), csv (fortran), and/or parquet as daily files
    
    
    '''
    
    def __init__(self,f_list,header,stat_id):
        self.f_list = f_list
        self.header, self.units = self.read_header(header)
        self.stat_id = stat_id
        self.v_num = 'v01'
        self.client = Client()
        self.client
        #print(self.client.scheduler_info())
    
    
    def read_header(self,in_url):
        '''
        Read in header from csv file
        
        Keyword arguments:
        in_dir : url for header file
        
        Returns:
        ts_header: list of variable names to be used as header in pd.read_csv
        
        General header format:
        var1,var2,var3,...,varn
        unit1,unit2,unit3,...,unitn
        '''
    
        with open(in_url) as f:
            reader = csv.reader(f)
            ts_header = next(reader)
            ts_units = next(reader)
            f.close()
        
        return ts_header, ts_units
    
    def read_files(self,npart=None,**kwargs):
        '''
        Read list of files into one dask dataframe. 
        
        Arguments:
        npart : number of partitions for dask dataframe
        
        REQUIRED kwargs to pass on to read_csv:
        skiprows - number of rows to skip before the actual data
        parse_dates - column name of datetime 
        index_col - column index of... index column
        na_values - list of strings that are nans
        
        Some optional kwargs to pass that speed up the process:
        blocksize - # of bytes to cut up larger files
        dtype - dictionary of columns and their (numpy) datatypes
        '''
        
        print('Reading File')
        self.df = dd.read_csv(self.f_list, names=self.header, **kwargs).set_index('TIMESTAMP')
        print('File Read')
        if npart is not None:
            self.df = self.df.repartition(npartitions = npart)
        self.df['doy'] = self.df.index.dayofyear.persist()
        self.doys = self.df['doy'].unique().compute()
        print('Days split')
        
    def add_offset(self,t_offset):
        self.df.index = self.df.index+t_offset
        
    def to_csv(self,out_url):
        
        #group_df = self.df.groupby(self.df.index.dayofyear)
        #doys = group_df.index.unique().compute()
        #print(group_df)
        for doy in self.doys:
            print(doy)
            temp_df = self.df[self.df['doy']==doy].drop('doy',axis=1).persist()
            temp_df = temp_df.compute()
            print('temp_df computed')
            yr = str(temp_df.index.year[0])[2:]
            temp_df.to_csv(f'{out_url}{self.stat_id}{yr}{doy}_{self.v_num}.csv',
                      header=False,float_format='%g')


    
    def to_fortran_csv(self,out_url):
        
        return
    
    def to_parq(self,out_url):
        
        return
    
    def clear_df(self):
        #clear out current dask dataframe
        self.client.cancel(self.df)
        self.doys = None
        
class toa5_convert_pd(object):
    '''
    Class used to read multiple toa5 files either as globstring or list of urls, then output to
    csv (pandas), csv (fortran), and/or parquet as daily files
    
    
    '''

    def __init__(self,f_list,header,stat_id):
        self.f_list = f_list
        self.header, self.units = self.read_header(header)
        self.stat_id = stat_id
        self.v_num = 'v01'
    
    
    def read_header(self,in_url):
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
    
    def read_files(self,**kwargs):
        '''
        Read list of files into one pandas dataframe. 
        '''
        
        print('Reading File')
        self.df = pd.concat([pd.read_csv(f, names=self.header, **kwargs) for f in self.f_list])
        print('File Read')
        
    def add_offset(self,t_offset):
        self.df.index = self.df.index+t_offset
    
    def to_csv(self,out_url):
        
        for doy,data in self.df.groupby(self.df.index.dayofyear):
            print(doy)
            if data.shape[0] <= 1:
                continue
            yr = str(data.index.year[0])[2:]
            data.to_csv(f'{out_url}{self.stat_id}{yr}{doy}_{self.v_num}.csv',
                      header=False,float_format='%g')
        print(f'Finished writing csv files to {out_url}')
    
    def to_fortran_csv(self,cols,out_url):
        '''
        Make sure to have list of column names so that it writes out properly
        '''
        
        for doy,data in self.df.groupby(self.df.index.dayofyear):
            print(doy)
            if data.shape[0] <= 1:
                continue
            yr = str(data.index.year[0])[2:]
            data['year'] = data.index.year
            data['hrmin'] = data.index.strftime('%H%M')
            data['doy'] = data.index.strftime('%j')
            data.to_csv(f'{out_url}{self.stat_id}{yr}{doy}_{self.v_num}.csv',
                      header=False,index=False,float_format='%g')
        
        print(f'Finished writing fortran-compatible csv files to {out_url}')
    
    def to_parqet(self,out_url):
        for doy,data in self.df.groupby(self.df.index.dayofyear):
            print(doy)
            if data.shape[0] <= 1:
                continue
            yr = str(data.index.year[0])[2:]
            data.to_parquet(f'{out_url}{self.stat_id}{yr}{doy}_{self.v_num}')
        print(f'Finished writing parqet files to {out_url}')
    
    def clear_df(self):
        self.df = None

class ec_file_convert(object):
    '''
    Class used to read multiple csv files either as globstring or list of urls, then output to
    csv (pandas), csv (fortran), and/or parquet as daily files
    
    
    '''

    def __init__(self,file_list,header,stat_id,v_num=1,read_header=False):
        self.file_list = file_list
        if read_header:
            self.header, self.units = self.read_header(header)
        else:
            self.header = header
        self.stat_id = stat_id
        self.v_num = v_num
    
    
    def read_header(self,in_url):
        '''
        Read in header from csv file
        
        Inputs:
            in_dir : url for header file
        
        Returns:
            ts_header : list of variable names to be used as header in pd.read_csv
        
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
    
    def to_csv(self,out_url):
        
        for doy,data in self.df.groupby(self.df.index.dayofyear):
            print(doy)
            if data.shape[0] <= 1:
                continue
            yr = str(data.index.year[0])[2:]
            data.to_csv(f'{out_url}{self.stat_id}{yr}{doy}_{self.v_num}.csv',
                      header=False,float_format='%g')
        print(f'Finished writing csv files to {out_url}')
        
    def _check_na(self,df,thresh=.75):
        '''
        Internal function to nan out entire hour if there are a certain amount of missing values.
        
        Inputs:
            param df (pd.DataFrame) : 
            param thresh (float) : 
        '''
        
        grouped = df.notnull().groupby(df.index.hour).sum().min(axis=1) / 72000.
        all_hrs = pd.Series(np.where(grouped.values >= thresh, True,False),index=grouped.index)
    
        good_hrs = all_hrs[all_hrs==True].index.tolist()
        
        print('Bad Hours: ',all_hrs[all_hrs==False].index.tolist())
        
        return df[df.index.hour.isin(good_hrs)]
    
    
        
    def fort_to_csv(self,out_url,**file_kwargs):
        '''
        
        '''
        def fort_date_secs(yr,doy,hr,sec):
            '''
            Date parser for pandas.read_csv when yr, doy, hr, and sec are the time columns
            '''    
            if '2400' in hr:
                hr = '000'
                return pd.datetime.strptime(f'{yr}{int(doy)+1}{int(hr):04}{float(sec):.2f}', '%Y%j%H%M%S.%f')
            else:
                return pd.datetime.strptime(f'{yr}{doy}{int(hr):04}{float(sec):.2f}', '%Y%j%H%M%S.%f')
        
        for f in self.file_list:
            print(f)
            temp_df = pd.read_csv(f,sep='\s+',parse_dates={'TIMESTAMP':[0,1,2,3]},date_parser=fort_date_secs,
                                  names=self.header,index_col=['TIMESTAMP'],**file_kwargs)
            yr = str(temp_df.index.year[0])[2:]
            doy = temp_df.index.dayofyear[0]
            temp_df.to_csv(f'{out_url}{self.stat_id}{yr}{doy:03}_v{self.v_num:02}.csv',
                           header=False,float_format='%g')

        return None
    
    def to_fortran_csv(self,out_url,drop_nans=True,**file_kwargs):
        '''
        Make sure to have list of column names so that it writes out properly
        '''
        
        for f in self.file_list:
            temp_df = pd.read_csv(f,names=self.header,index_col=[0],**file_kwargs)
            print(temp_df.index.dayofyear[0])
            if temp_df.shape[0] <= 1:
                continue

            temp_df['year'] = temp_df.index.year
            temp_df['doy'] = temp_df.index.dayofyear
            temp_df['hrmin'] = temp_df.index.strftime('%H%M').str.pad(width=4,fillchar='0')
            temp_df['sec'] = temp_df.index.strftime('%S').astype(float) + temp_df.index.strftime('%f').astype(float)*1e-6
            temp_df['hour'] = temp_df.index.hour
            
            yr = str(temp_df['year'][0])[2:]
            doy = temp_df['doy'][0]
            
            cols = ['year','doy','hrmin','sec','Ux','Uy','Uz','Ts','diag_sonic',
                    'CO2_li','H2O_li','amb_press_li','agc_li']
            
            temp_df = self._check_na(temp_df)
            
            if drop_nans:
                temp_df = temp_df.dropna()
            
            
            temp_df.to_csv(f'{out_url}{self.stat_id}{yr}{doy:03}_v{self.v_num:02}.csv',
                      header=False,index=False,columns=cols,float_format='%g')
            
        print(f'Finished writing fortran-compatible csv files to {out_url}')
    
    def to_parquet(self,out_url,**file_kwargs):
        for f in self.file_list:
            temp_df = pd.read_csv(f,names=self.header,index_col=[0],parse_dates=True,**file_kwargs)
            print(temp_df.index.dayofyear[0])
            
            if temp_df.shape[0] <= 1:
                continue
            
            yr =str(temp_df.index.year[0])[2:]
            doy = temp_df.index.dayofyear[0]
            
            temp_df.to_parquet(f'{out_url}{self.stat_id}{yr}{doy:03}_v{self.v_num:02}.parquet',index=True,
                               compression='gzip')
            
        print(f'Finished writing parqet files to {out_url}')
    
    def clear_df(self):
        self.df = None
    
    

    
        
