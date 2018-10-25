import pandas as pd
import numpy as np
import holoviews as hv
from bokeh.models.formatters import DatetimeTickFormatter
from holoviews.operation.datashader import datashade
import os

#functions to create plots
def date_fmt(plot,element,constant_date_fmt='%H:%M:%S'):
    fields = ['microseconds', 'milliseconds', 'seconds', 'minsec', 'minutes', 'hourmin', 'hours', 'days', 'months', 'years']
    fmt_dict = {f:constant_date_fmt for f in fields}
    plot.handles['xaxis'].formatter = DatetimeTickFormatter(**fmt_dict)
    
def create_curve(data,var,color,lim,group,ts_var='TIMESTAMP'):
    curve = hv.Curve((data.index,data[var]),ts_var,var,group=group)
    ds = datashade(curve,min_alpha=100,cmap=color,y_range=lim,group=group)
    return ds

def create_layout(data,var_dict,plot_spec,group,num_cols):
    curve_list = [create_curve(data = data,var = v,color = var_dict[v]['color'],
                          lim = var_dict[v]['lim'],group=group) for v in var_dict]
    layout = hv.Layout(curve_list).cols(num_cols)
    return layout.opts(plot_spec)

#functions for cutting out bad data
def nan_times(df,cols,times):
    base_date = df.index[0].strftime('%Y-%m-%d')
    for t in times:
        df.loc[f'{base_date} {t[0]}':f'{base_date} {t[1]}',[c for c in cols]] = np.nan
    return df

def cut_times(df,times):
    base_date = df.index[0].strftime('%Y-%m-%d')
    for t in times:
        df = df[(df.index < f'{base_date}T{t[0]}') | (df.index > f'{base_date}T{t[1]}')]
    return df
        
#filter data to avoid crashing while interacting with plots that have large spikes
def filter_bounds(data,var,bounds):
    return ((data[var] > bounds[0]) & (data[var] < bounds[1]))


