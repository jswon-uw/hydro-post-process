#!/usr/bin/python3
import os
import sys
import math
import datetime as dt
import decimal
import re
import argparse
import pandas as pd
import numpy as np
import scipy as sp
from scipy.stats import genextreme as gev

###############################################################################
# Calculates metrics for a given daily stream data
#
# jwon - last edit 11/21/2017
# 06/06/2016 - Added -k option to keep NaN data with threshold
#            - Fixed the dates, wday, and flow optiosn
#            - Added -h header option
# 06/07/2016 - Fixed the -k options for duplicate entries.
#              Added -k implementations for months
#            - Fixed diagnostic view for missing months
# 06/20/2016 - Added decimals for trailing zeros
# 09/21/2017 - Added extreme statistic return year options
#            - Added option to choose peak and low day averages (q)
#            - Added option for frequency from magnitude
# 11/16/2017 - Added options to skip n lines in the beginning of file [--skip]
# 11/21/2017 - Added option to list na values [--na]
#            - Added backbone for option to change the delimiter [--ifs];
#              need to check -,/\t vs -, /\t compatability
# 04/21/2018 - Fixed return frequency functions pfreq and lfreq
###############################################################################

parser = argparse.ArgumentParser()
parser.add_argument('input_file')
# Threshold (between 0 to 1).
parser.add_argument(
    '-t', nargs='?', default=0,
    help=('Apply a threshold for NaN calculations. Choose between 0 to 1.' + 
          " 0 accepts all. 1 requires all data points."))
# d: Daily Data
parser.add_argument(
    '-d', action='store_true', default=False,
    help='Creates a daily data file.')
# m: Monthly Average
parser.add_argument(
    '-m', action='store_true', default=False,
    help='Calculates average monthly data.')
# p: Water yearly Peak flow + Date
parser.add_argument(
    '-p', action='store_true', default=False,
    help='Calculates peak flow by water year.')
# l: Water yearly min 7-day rolling flow
parser.add_argument(
    '-l', action='store_true', default=False,
    help='Calculates minimum 7-day average flow by water year.')


# parser.add_argument('-q', action='store_true', default=False,
# help='Toggles calculation weekly low flow vs daily low flow.
# Default weekly.') # q: 7q10 vs q10

# pq: 7q10 vs q10
parser.add_argument(
    '--pq', action='store', type=int, default=1,
    help='Choose the number of days to include in a rolling flow. (Peak)')
# lq: 7q10 vs q10
parser.add_argument(
    '--lq', action='store', type=int, default=7,
    help='Choose the number of days to include in a rolling flow. (Low)')

# pfreq: Flood frequency from magnitude <- provide list
parser.add_argument(
    '--pfreq', action='store', type=str,  default="",
    help='Calculates frequency from magnitudes')
# lfreq: Low frequency from magnitude <- provide list
parser.add_argument(
    '--lfreq', action='store', type=str,  default="",
    help='Calculates frequency from magnitudes')

# g: Monthly Average + group by months
parser.add_argument(
    '-g', action='store_true', default=False,
    help='Calculates monthly averages grouped by months.')
# y: Water Yearly Average
parser.add_argument(
    '-y', action='store_true', default=False,
    help='Calculates water yearly averages.')
# e: Extreme statistics
parser.add_argument(
    '-e', action='store_true', default=False,
    help='Calculates extreme statistics.')
# gev:
parser.add_argument(
    '--gev', action='store_true', default=False,
    help='Save GEV parameters as output.')

# expyr: Specify return years used for peak flow extreme statistics.
#        <- provide comma delimited list
parser.add_argument(
    '--expyr', type=str,  default="2,10,50,100",
    help='Specify return years to use for extreme statistics.'
    + ' Input comma delimited list. Default: 2,10,50,100')
# Must be greater than 1
# exlyr: Specify return years used for low flow extreme statistics.
#        <- provide comma delimited list
parser.add_argument(
    '--exlyr', type=str, default="2,10",
    help='Specify return years to use for extreme statistics. '
    + 'Input comma delimited list. Default: 2,10')
# na: Provide list of values to treat as NA.
#     <- provide comma delimited list
parser.add_argument(
    '--na', type=str, default="NA,NaN, ",
    help='Provide list of values to treat as NA. '
    + 'Input comma delimited list. Default NA,NaN, .')
# ifs: Provide list of values to treat as NA.
#      <- provide string match expression
# parser.add_argument('--ifs', type=str, default="-, /\t",
# help='Provide list of values to treat as input delimiters.
# Input string expression. Provide Default \"-, /\t\"')

# s: Strips the date to water years
parser.add_argument(
    '-s', action='store_true', default=False,
    help='Strips the dates to start and end on water year.')
# x: Diagnostics
parser.add_argument(
    '-x', action='store_true', default=False,
    help='Diagnostics table showing NA for each month and year')
# k: Keep NaN with threshold
parser.add_argument(
    '-k', action='store_true', default=False,
    help='Keep NaN in dataset when using threshold.')
# v: Output results on screen
parser.add_argument(
    '-v', action='store_true', default=False,
    help='Outputs results on screen.')
# skip: Skip the first [n] line of files
parser.add_argument(
    '--skip', nargs='?', type=int, default=0,
    help='Skips the first [n] lines of file.')
# header: Uses the first line as header. Default assumes no header
parser.add_argument(
    '--header', action='store_true', default=False,
    help='Uses the first line as header. Default assumes no header.')
# headout: Turns off header on output
parser.add_argument(
    '--headout', action='store_false', default=True,
    help='No header on output. Header enabled by default.')
# start: Cut data to only include data after given start year
parser.add_argument(
    '--start', nargs='?', type=int, default=0,
    help="Specify start year to cut.")
# end: Cut data to only include data before given end year
parser.add_argument(
    '--end', nargs='?', type=int, default=0,
    help="Specify end year to cut.")
# dates:
parser.add_argument(
    '--dates', action='store_true', default=False,
    help='Creates a dates only csv for peak or low flow.')
# wday:
parser.add_argument(
    '--wday', action='store_true', default=False,
    help='Creates a wday only csv for peak or low flow.')
# flow:
parser.add_argument(
    '--flow', action='store_true', default=False,
    help='Creates a flow only csv for peak or low flow.')
# quantiles:
parser.add_argument(
    '--quantiles', action='store_true', default=False,
    help='Create a quantile file during stat calculations.')
# name:
parser.add_argument(
    '--name', type=str, default=None,
    help='Use a custom filename for outputs.')
parser.add_argument('out_dir', nargs='?', default=os.getcwd())
args = parser.parse_args()
infile = args.input_file

if float(args.t) < 0 or float(args.t) > 1:
    print('ERROR: Threshold out of range. Please choose between 0 and 1.')
    sys.exit()


# ---Functions---
def get_wyear(year, month):
    if (month > 9):
        return year + 1
    else:
        return year


def print_full(x):
    pd.set_option('display.max_rows', len(x))
    print(x)
    pd.reset_option('display.max_rows')


# ---Variables---
thold = float(args.t)
prec = 2
naVal = 'NaN'

if (args.name is None):
    filename = os.path.splitext(os.path.basename(infile))[0].split('.')[0]
else:
    filename = args.name


# ---Check directory---
out_dir = args.out_dir + '/'
if (not os.path.isdir(args.out_dir)):
    print("Given output directory doesn't exist.")
    print("   " + out_dir)
    sys.exit()

outDaily = out_dir + filename + "_DailyFlows.csv"
outMonthAvg = out_dir + filename + "_MonthlyFlows.csv"
outYearlyAvg = out_dir + filename + "_YearlyFlows.csv"
outPeak = out_dir + filename + "_PeakFlows"
outLow = out_dir + filename + "_LowFlows"
outGroupMonth = out_dir + filename + "_MonthlyAvg.csv"
outDiagn = out_dir + filename + "_NAView"
outExQPeak = out_dir + filename + "_PeakQuantile.csv"
outExQLow = out_dir + filename + "_LowQuantile.csv"
outExSPeak = out_dir + filename + "_PeakStats.csv"
outExSLow = out_dir + filename + "_LowStats.csv"
outFqPeak = out_dir + filename + "_PeakFreq.csv"
outFqLow = out_dir + filename + "_LowFreq.csv"
outGEVPeak = out_dir + filename + "_PeakGEV.csv"
outGEVLow = out_dir + filename + "_LowGEV.csv"


# ---Intializing dataframes---
mdf = pd.DataFrame()   # Monthly Average Dataframe
pdf = pd.DataFrame()   # Water year Peak Dataframe
ldf = pd.DataFrame()   # Water year min 7 day average Dataframe
gdf = pd.DataFrame()   # Monthly Grouped average Dataframe
pgev = pd.DataFrame()  # Peak GEV Params
lgev = pd.DataFrame()  # Low GEV Params
epdf = pd.DataFrame()  # Extreme PeakDataframe
eldf = pd.DataFrame()  # Extreme Low Dataframe
xdf = pd.DataFrame()   # Diagnostics Dataframe
tmdf = pd.DataFrame()  # Threshold Monthly Dataframe
tydf = pd.DataFrame()  # Threshold Yearly Dataframe
gy = pd.DataFrame()    # WYear only Dataframe (For adding back NaN)
gm = pd.DataFrame()    # WYear + Month only Dataframe (For adding back NaN)

# ---Prepare list params---
pl_func = lambda x: [float(y) for y in x.split(',') if x]
pfreqList = pl_func(args.pfreq)
lfreqList = pl_func(args.lfreq)
expyrList = np.array(pl_func(args.expyr))
exlyrList = np.array(pl_func(args.exlyr))
naList = args.na.split(',')
# ifsList = "[" + args.ifs + ']*' ### TODO: allow user provided delimiters
ifsList = "[-, /\t]*"

# ====================Preperation====================
# ---Load data---
if(args.header):
    df = pd.read_csv(infile, sep=ifsList, engine='python',
                     skiprows=args.skip, na_values=naList)
else:
    df = pd.read_csv(infile, sep=ifsList,engine='python', header=None,
                     skiprows=args.skip, na_values=naList)
df.columns = ['Year', 'Month', 'Day', 'Flow']


# Strip data to only include within given years
if(args.start > 0):
    df = df[df.Year >= args.start]

if(args.end > 0):
    df = df[df.Year <= args.end]
df = df.reset_index()

if (len(df) == 0):
    print("No data in range. Check data or adjust date range.")
    sys.exit()

# Filter out non-numeric data
# -> For older version of pandas
# #df['Flow'] = df['Flow'].convert_objects(convert_numeric=True)

# -> Requires pandas 0.17.0
df['Flow'] = pd.to_numeric(df['Flow'], errors='coerce')

# Get Water Years
df['WYear'] = np.vectorize(get_wyear)(df['Year'], df['Month'])
# #print_full(df)

# Strip dates to match water years
if (args.s):
    ind = 0

    # TODO: Failsafe checks for missing data
    # Behavior Check: If the following year is empty,
    # should it try the next year?

    # Earlier than 10/01
    if(df.iloc[0].Month < 10):
        # -> drop everything in that year to 10/01
        ind = df.loc[(df['Year'] == df.iloc[0].Year)
                     & (df['Month'] == 10)
                     & (df['Day'] == 1)].index[0]
    # Later than 10/01
    if(df.iloc[0].Month >= 10 and df.iloc[0].Day > 1):
        # -> drop everything till 10/01 of the following year
        ind = df.loc[(df['Year'] == df.iloc[0].Year + 1)
                     & (df['Month'] == 10)
                     & (df['Day'] == 1)].index[0]
    # Strip top
    if(ind > len(df.index)):
        print("ERROR: Not enough data to perform data stripping.")
        sys.exit()
    df = df[ind:].reset_index()

    # Ends later than 09/30
    if(df.iloc[-1].Month >= 10):
        # -> drop everything in that year to 9/30
        ind = df.loc[(df['Year'] == df.iloc[-1].Year)
                     & (df['Month'] == 9)
                     & (df['Day'] == 30)].index[0] + 1
        df = df[:ind]
    # Ends earlier than 09/30
    if(df.iloc[-1].Month <= 9 and df.iloc[-1].Day < 30):
        # -> drop everything till 09/30 of the previous year
        ind = df.loc[(df['Year'] == df.iloc[-1].Year-1)
                     & (df['Month'] == 9)
                     & (df['Day'] == 30)].index[0] + 1
        df = df[:ind]

    # TODO: check there is enough data when stripping from below


# Clear out data outside of threshold for months
def threshold_m(df, gm):
    tmdf = df[['Year', 'Month', 'Flow']].groupby(['Year', 'Month'])
    tmdf = tmdf.agg(['count', 'size']).reset_index()
    tmdf.columns = tmdf.columns.droplevel()
    tmdf.columns = ['Year', 'Month', 'count', 'size']
    tmdf['Thold'] = tmdf['count'].div(tmdf['size'], axis='index')
    tmdf = tmdf[tmdf.Thold >= thold]
    tmdf = pd.merge(tmdf[['Year', 'Month']], df,
                    on=['Year', 'Month'], how='left')
    if(args.k):
        gm = df.groupby(['Year', 'Month']).count()
        gm = gm.reset_index()[['Year', 'Month']]
    tmdf = tmdf[['WYear', 'Year', 'Month', 'Day', 'Flow']]

    # Check that there is enough data
    if(tmdf.empty):
        print("ERROR: Not enough data for processing.")
        sys.exit()
    return (tmdf, gm)


# Clear out data outside of threshold for water years
def threshold_y(df, gy):
    tydf = df[['WYear', 'Flow']].groupby(['WYear'])
    tydf = tydf.agg(['count', 'size']).reset_index()
    tydf.columns = tydf.columns.droplevel()
    tydf.columns = ['WYear', 'count', 'size']
    tydf['Thold'] = tydf['count'].div(tydf['size'], axis='index')
    tydf = tydf[tydf.Thold >= thold]
    tydf = pd.merge(tydf[['WYear']], df, on=['WYear'], how='left')
    if(args.k):
        # #tydf = pd.merge(df[['Year', 'Month', 'Day', 'WYear']], tydf,
        # #on=['Year', 'Month', 'Day', 'WYear'], how='left')
        gy = df.groupby(['WYear']).count()
        gy = gy.reset_index()[['WYear']]
    tydf = tydf[['WYear', 'Year', 'Month', 'Day', 'Flow']]

    # Check that there is enough data
    if(tydf.empty):
        print("ERROR: Not enough data for processing.")
        sys.exit()

    return (tydf, gy)


# ----------------------------------Calculations-------------------------------
# Daily Data
if (args.d):
    print('Formatting Daily Data: ' + filename)
    (tydf, gy) = threshold_y(df, gy)

    # Keep NaN
    if(args.k):
        tydf = pd.merge(
            df[['WYear', 'Year', 'Month', 'Day']],
            tydf, on=['WYear', 'Year', 'Month', 'Day'],
            how='left')

    # Verbose
    if(args.v):
        print(tydf)

    # Output
    out = tydf[['Year', 'Month', 'Day', 'Flow']].fillna(naVal)
    out.to_csv(outDaily, index=False, header=args.headout)


# -----------------------------------------------------------------------------
# Calculate monthly averages
def get_month(tmdf):
    mdf = tmdf[['Year', 'Month', 'Flow']].groupby(['Year', 'Month'])
    mdf = mdf.agg(['mean']).reset_index()
    mdf.columns = mdf.columns.droplevel()

    # Cleanup
    mdf[['mean']] = np.round(mdf[['mean']].astype(np.double), prec)
    mdf.columns = ['Year', 'Month', 'Avg.Flow']
    return mdf


if (args.m):
    print('Calculating Monthly Averages: ' + filename)
    if(tmdf.empty):
        (tmdf, gm) = threshold_m(df, gm)
    mdf = get_month(tmdf)

    # Keep NaN
    if(args.k):
        mdf = pd.merge(gm, mdf, on=['Year', 'Month'], how='left')

    # Verbose
    if(args.v):
        print(mdf)

    # Output
    mdf.fillna(naVal).to_csv(outMonthAvg, index=False, header=args.headout)


# -----------------------------------------------------------------------------
# Calculate water yearly averages
def get_wyear(tydf):
    ydf = tydf[['WYear', 'Flow']].groupby(['WYear'])
    ydf = ydf.agg(['mean']).reset_index()

    # Cleanup
    ydf.columns = ['Year', 'Avg.Flow']
    ydf['Avg.Flow'] = np.round(ydf[['Avg.Flow']].astype(np.double), prec)
    return ydf


if(args.y):
    print('Calculating Yearly Averages: ' + filename)
    if(tydf.empty):
        (tydf, gy) = threshold_y(df, gy)
    ydf = get_wyear(tydf)

    # Keep NaN
    if(args.k):
        ydf = pd.merge(gy, ydf, left_on='WYear', right_on='Year', how='left')
        ydf.drop('Year', axis=1, inplace=True)
        ydf.columns = ['Year', 'Avg.Flow']

    # Verbose
    if(args.v):
        print(ydf)

    # Output
    out = ydf.fillna(naVal)
    out.to_csv(outYearlyAvg, index=False, header=args.headout)


# -----------------------------------------------------------------------------
# Calculate Peak flow by water year
def get_peak(tydf):
    pdf = tydf
    pdf['PeakFlow'] = tydf['Flow'].rolling(window=args.pq, center=False).mean()
    pdf = pdf.loc[pdf.groupby('WYear')['PeakFlow'].idxmax()].reset_index()
    pdf['WYear'] = pd.unique(tydf.WYear.ravel())
    pdf.drop_duplicates(['WYear'], inplace=True)

    # Cleanup
    pdf[['LowFlow']] = np.round(pdf[['PeakFlow']].astype(np.double), prec)
    pdf = pdf[['WYear', 'Year', 'Month', 'Day', 'PeakFlow']]

    # #pdf = tydf[tydf.groupby(['WYear'], sort=False)['Flow']
    # #pdf = pdf.transform(max)==tydf['Flow']].copy()
    # #pdf.drop_duplicates(['WYear'], inplace=True)
    # Cleanup
    # #pdf[['Flow']] = np.round(pdf[['Flow']].astype(np.double), prec)
    # #pdf.rename(columns={'Flow':'PeakFlow'}, inplace=True)
    return pdf


if (args.p):
    print('Calculating Water Yearly %d-day Peak Flows: %s'
          % (args.pq, filename))
    if(tydf.empty):
        (tydf, gy) = threshold_y(df, gy)
    pdf = get_peak(tydf)

    # Keep NaN
    if(args.k):
        pdf = pd.merge(gy, pdf, on=['WYear'], how='left')

    # Verbose
    if(args.v):
        print(pdf)

    # Output
    if(args.dates):
        pdft = pdf.copy()
        pdft['Date'] = pdft.apply(
            lambda x:str('{0: 0>4g}'.format(x.Year))
            + "-" + str('{0: 0>2g}'.format(x.Month))
            + "-" + str('{0: 0>2g}'.format(x.Day))
            if pd.notnull(x.PeakFlow) else x.PeakFlow, axis=1)
        out = pdft[['WYear', 'Date']].fillna(naVal)
        out.to_csv(outPeak + "-dates.csv", index=False, header=args.headout)

    if(args.wday):
        pdft = pdf.copy()
        pdft['FDate'] = pdft.apply(
            lambda x: str('{0: 0>4g}'.format(x.Year))
            + "-"
            + str('{0: 0>2g}'.format(x.Month))
            + "-"
            + str('{0: 0>2g}'.format(x.Day))
            if pd.notnull(x.PeakFlow)
            else x.PeakFlow, axis=1)
        pdft['FDate'] = pd.to_datetime(pdft['FDate'], format='%Y-%m-%d')
        pdft['SDate'] = pdft.apply(lambda x: str(x.WYear-1) + "-09-30", axis=1)
        pdft['SDate'] = pd.to_datetime(pdft['SDate'])
        pdft['DoWY'] = (pdft['FDate'] - pdft['SDate']) / np.timedelta64(1, 'D')

        out = pdft[['WYear', 'DoWY']].fillna(naVal)
        out.to_csv(outPeak + "-wday.csv", index=False, header=args.headout)

    if(args.flow):
        out = pdf[['WYear', 'PeakFlow']].fillna(naVal)
        out.to_csv(outPeak + "-Flow.csv", index=False, header=args.headout)

    if(not(args.dates & args.wday & args.flow)):
        out = pdf.fillna(naVal)
        out.to_csv(outPeak + ".csv", index=False, header=args.headout)


# -----------------------------------------------------------------------------
# Calculate 7-day minimum average flow by water year.
# Date returned is the last day of the window
def get_low(tydf):
    ldf = tydf
    # #ldf['LowFlow'] = pd.rolling_mean(tydf['Flow'], window=7)#.shift(-3)
    # # Shift for which day is the center  ## deprecated
    ldf['LowFlow'] = tydf['Flow'].rolling(window=args.lq, center=False).mean()   # Shift for which day is the center

    ldf = ldf.loc[ldf.groupby('WYear')['LowFlow'].idxmin()].reset_index()
    ldf['WYear'] = pd.unique(tydf.WYear.ravel())
    ldf.drop_duplicates(['WYear'], inplace=True)

    # Cleanup
    ldf[['LowFlow']] = np.round(ldf[['LowFlow']].astype(np.double), prec)
    ldf = ldf[['WYear', 'Year', 'Month', 'Day', 'LowFlow']]

    return ldf


if (args.l):
    print('Calculating Water Yearly %d-day Low Flows: %s'
          % (args.lq, filename))
    if(tydf.empty):
        (tydf, gy) = threshold_y(df, gy)
    ldf = get_low(tydf)

    # Keep NaN
    if(args.k):
        ldf = pd.merge(gy, ldf, on=['WYear'], how='left')

    # Verbose
    if(args.v):
        print(ldf)

    # Output
    if(args.dates):
        ldft = ldf.copy()
        ldft['Date'] = ldft.apply(
            lambda x: str('{0: 0>4g}'.format(x.Year))
            + "-"
            + str('{0: 0>2g}'.format(x.Month))
            + "-"
            + str('{0: 0>2g}'.format(x.Day))
            if pd.notnull(x.LowFlow)
            else x.LowFlow, axis=1)
        out = ldft[['WYear', 'Date']].fillna(naVal)
        out.to_csv(outLow + "-dates.csv", index=False, header=args.headout)

    if(args.wday):
        ldft = ldf.copy()
        ldft['FDate'] = ldft.apply(
            lambda x: str('{0: 0>4g}'.format(x.Year + 1))
            + "-"
            + str('{0: 0>2g}'.format(x.Month))
            + "-"
            + str('{0: 0>2g}'.format(x.Day))
            if pd.notnull(x.LowFlow)
            else x.LowFlow, axis=1)
        ldft['FDate'] = pd.to_datetime(ldft['FDate'])
        ldft['SDate'] = ldft.apply(lambda x: str(x.WYear) + "-09-30", axis=1)
        ldft['SDate'] = pd.to_datetime(ldft['SDate'])
        ldft['DoWY'] = (ldft['FDate'] - ldft['SDate']) / np.timedelta64(1, 'D')

        out = ldft[['WYear', 'DoWY']].fillna(naVal)
        out.to_csv(outLow + "-wday.csv", index=False, header=args.headout)

    if (args.flow):
        out = ldf[['WYear', 'LowFlow']].fillna(naVal)
        out.to_csv(outLow + "-Flow.csv", index=False, header=args.headout)

    if (not(args.dates & args.wday & args.flow)):
        out = ldf.fillna(naVal)
        out.to_csv(outLow + ".csv", index=False, header=args.headout)

# -----------------------------------------------------------------------------
# Calculate Monthly Averages grouped by month
if (args.g):
    print('Calculating Monthly Average grouped by Month: ' + filename)
    if(tmdf.empty):
        (tmdf, gm) = threshold_m(df, gm)
    if(mdf.empty):
        mdf = get_month(tmdf)
    gdf = mdf[['Month', 'Avg.Flow']].groupby('Month')
    gdf = gdf.agg(['mean']).reset_index()
    gdf.columns = gdf.columns.droplevel()
    gdf = gdf.reindex(index=np.roll(gdf.index, 3))
    gdf.columns = ['Month', 'Avg.Flow']

    # Cleanup
    gdf[['Avg.Flow']] = np.round(gdf[['Avg.Flow']].astype(np.double), prec)

    # Verbose
    if(args.v):
        print(gdf)

    # Output
    out = gdf.fillna(naVal)
    out.to_csv(outGroupMonth, index=False, header=args.headout)


# -----------------------------------------------------------------------------
#  Helper function to calculate extreme moments
def get_extreme_moment(edf):
    edf.dropna(inplace=True)
    # #edf.sort(['Flow'], ascending=False, inplace=True) ## DEPRECATED
    edf.sort_values(by=['Flow'], ascending=False, inplace=True)

    edf = edf.reset_index()[['Flow']]
    edf['Rank'] = edf.index + 1
    n = len(edf.index)

    # #edf['B1i'] = edf.apply(
    # #   lambda x: (n - x.Rank) / (n*(n - 1)) * x.Flow, axis=1)
    # #edf['B2i'] = edf.apply(lambda x: ((n - x.Rank) * (n - x.Rank - 1)) / (n * (n - 1) * (n - 2)) * x.Flow, axis=1)
    # #edf['B3i'] = edf.apply(lambda x: ((n - x.Rank) * (n - x.Rank - 1) * (n-x.Rank-2)) / (n * (n - 1) * (n - 2) * (n-3)) * x.Flow, axis=1)
    edf['B1i'] = (n - edf['Rank']) / (n * (n - 1)) * edf['Flow']
    edf['B2i'] = edf['B1i'] * (n - 1 - edf['Rank']) / (n - 2)
    edf['B3i'] = edf['B2i'] * (n - 2 - edf['Rank']) / (n - 3)
    return edf


# Helper function to save quantile outputs
def save_quantile(edf, outname):
    n = len(edf.index)
    edf['Quantile'] = edf.apply(lambda x: (x.Rank - 0.4) / (n + 0.2), axis=1)
    edf[['Quantile']] = np.round(edf[['Quantile']].astype(np.double), prec)

    out = edf[['Quantile', 'Flow']]
    out.to_csv(outname, index=False, header=args.headout)


# Helper function to calculate GEV parameters
def get_gev_param(edf):
    B = [0, 0, 0, 0, 0]
    L = [0, 0, 0, 0, 0]
    c = [0, 0, 0, 0]
    kappa = [0, 0, 0, 0]
    alpha = [0, 0, 0, 0]
    psi = [0, 0, 0, 0]
    gev = pd.DataFrame({"kappa": [3, 0, 0, 0],
                        "alpha": [2, 0, 0, 0],
                        "psi": [0, 0, 0, 0]})

    B[0] = edf['Flow'].mean()
    B[1] = edf['B1i'].sum()
    B[2] = edf['B2i'].sum()
    B[3] = edf['B3i'].sum()

    L[1] = B[0]
    L[2] = 2 * B[1] - B[0]
    L[3] = 6 * B[2] - 6 * B[1] + B[0]

    # #print("B0: " + str(B[0]) + " B1: " + str(B[1])
    # #      + " B2: " + str(B[2]) + " B3: " + str(B[3]))
    # print("L1: " + str(L[1]) + " L2: " + str(L[2]) + " L3: " + str(L[3]))

    # GEV distribution using L moments
    c[0] = (2 * L[2]) / (L[3] + 3 * L[2]) - 0.630930
    gev.loc[0, 'kappa'] = 7.8590 * c[0] + 2.9554 * c[0] * c[0]
    gamma = math.gamma(1 + gev.loc[0, 'kappa'])
    gev.loc[0, 'alpha'] = gev.loc[0, 'kappa'] * L[2] / (gamma * (1 - math.pow(2, -gev.loc[0, 'kappa'])))
    gev.loc[0, 'psi'] = L[1] + gev.loc[0, 'alpha'] * (gamma - 1) / gev.loc[0, 'kappa']

    # GEV parameters based on LH2 moments (Wang 1997)
    a2 = [0.5914, -2.3351, 0.6442, -0.1616]

    gev.loc[1, 'kappa'] = a2[0] + a2[1]
    gamma = math.gamma(1 + gev.loc[1,  'kappa'])
    # #alpha[1] = LH2[1] / (2 * gamma / kappa[1] * math.e(-1 * kappa[1] * math.log(3.0)) - math.e(-1 * kappa[1] * math.log(40)))
    # #psi[1] = LH2[0] - alpha[1] / kappa[1] * (1.0 - gamma * math.e(10 * kappa[1] * log(30)))

    # GEV parameters based on LH4 moments (Wang 1997)
    a4 = [0.7113, -2.5383, 0.5142, -0.1027]

    # EV1 parameters based on L moments
    gev.loc[3, 'alpha'] = 1.443 * L[1]
    gev.loc[3, 'psi'] = L[0] - 0.5772 * gev.loc[3, 'alpha']

    # #print("c: " + str(c[0]) + " k: " + str(kappa[0]) )
    # #print("gamma: " + str(gamma) + " alpha:  " + str(alpha[0]) + " psi: " + str(psi[0]))
    return gev


# Helper function to calculate extremes
def get_extreme(gev, isFlood):
    if(isFlood):
        edf = pd.DataFrame({"prob": 1 - 1 / expyrList})   # Peak Flows
    else:
        edf = pd.DataFrame({"prob": 1 / exlyrList})       # Low Flows

    a = gev.loc[0, 'alpha']
    k = gev.loc[0, 'kappa']
    p = gev.loc[0, 'psi']

    #edf['dist0'] = gev.loc[0, 'psi'] + gev.loc[0, 'alpha'] / gev.loc[0, 'kappa'] * (1 - (np.power(-np.log(edf['prob']), gev.loc[0, 'kappa'])))
    edf['dist0'] = p + a / k * (1 - (np.power(-np.log(edf['prob']), k)))    
    edf.ix[:, edf.columns != 'prob'] = (edf.ix[:, edf.columns != 'prob'].astype(np.double))

    return edf


# Calculate extreme statistics
if (args.e):
    print('Calculating Extreme Flood Stats: ' + filename)
    if(tydf.empty):
        (tydf, gy) = threshold_y(df, gy)
    if(pdf.empty):
        pdf = get_peak(tydf)
        
    pgev = pdf[['PeakFlow']].copy()
    pgev.columns = ['Flow']    
    pgev = get_extreme_moment(pgev)

    # Quantiles
    if(args.quantiles):
        save_quantile(pgev, outExQPeak)

    pgev = get_gev_param(pgev)
    epdf = get_extreme(pgev, True)

    # Verbose
    if(args.v):
        print("\nPeak Flows")
        print(epdf.set_index('prob').T)

    epdf['prob'] = epdf['prob'].apply(lambda x : int(round(1 / (1 - x), 0)))
    epdf = epdf.rename(columns={'prob': 'returnYr'})
    epdf.to_csv(outExSPeak, index=False, float_format='%.2f',
                header=args.headout)

    # --------------------------------------------------------------
    print('Calculating Extreme Low Stats: ' + filename)
    if(ldf.empty):
        ldf = get_low(tydf)

    lgev = ldf[['LowFlow']].copy()
    lgev.columns = ['Flow']
    lgev = get_extreme_moment(lgev)

    # Quantiles
    if(args.quantiles):
        save_quantile(lgev, outExQLow)

    lgev = get_gev_param(lgev)
    eldf = get_extreme(lgev, False)

    # Verbose
    if(args.v):
        print("\nLow Flows")
        print(eldf.set_index('prob').T)

    eldf['prob'] = eldf['prob'].apply(lambda x : int(round((1 / x), 0)))
    eldf = eldf.rename(columns={'prob': 'returnYr'})
    eldf.to_csv(outExSLow, index=False, float_format='%.2f',
                header=args.headout)


## Helper function to calculate frequency from magnitude
def get_freq(gev, freqList, isFlood):
    fqdf = pd.DataFrame({'flow': freqList})

    a = gev.loc[0, 'alpha']
    k = gev.loc[0, 'kappa']
    p = gev.loc[0, 'psi']
    
    fqdf['freq'] = np.exp(-np.power(1- ((fqdf['flow'] - p) * k / a), 1/k))
        
    if(isFlood):
        fqdf['freq'] = 1 / (1 - fqdf['freq'])
    else:
        fqdf['freq'] = 1 / fqdf['freq']

    return fqdf


# Calculate flood frequencies from magnitudes
if(pfreqList):
    print('Calculating flood frequencies from magnitude: ' + filename)
    if(pgev.empty):
        if(tydf.empty):
            (tydf, gy) = threshold_y(df, gy)
        if(pdf.empty):
            pdf = get_peak(tydf)
        pgev = pdf[['PeakFlow']].copy()
        pgev.columns = ['Flow']
        pgev = get_extreme_moment(pgev)
        pgev = get_gev_param(pgev)

    # Calculate magnitude
    pfdf = get_freq(pgev, pfreqList, True)

    # Save output
    if(args.gev):
        pgev.to_csv(outGEVPeak, index=False, float_format='%.2f',
                    header=args.headout)
    pfdf.to_csv(outFqPeak, index=False, float_format='%.2f',
                header=args.headout)

# Calculate low frequencies from magnitudes
if(lfreqList):
    print('Calculating low frequencies from magnitude: ' + filename)
    if(lgev.empty):
        if(tydf.empty):
            (tydf, gy) = threshold_y(df, gy)
        if(ldf.empty):
            ldf = get_low(tydf)
        lgev = ldf[['LowFlow']].copy()
        lgev.columns = ['Flow']
        lgev = get_extreme_moment(lgev)
        lgev = get_gev_param(lgev)

    # Calculate magnitude
    lfdf = get_freq(lgev, lfreqList, False)

    # Save output
    if(args.gev):
        lgev.to_csv(outGEVLow, index=False, float_format='%.2f',
                    header=args.headout)
    lfdf.to_csv(outFqLow, index=False, float_format='%.2f',
                header=args.headout)

# -----------------------------------------------------------------------------
# Creates a diagnostics table showing how many NA values are found
# in each month of each year
if (args.x):
    print('Creating diagnostic table: ' + filename)

    # By Year
    xdf = df[['Year', 'Month', 'Flow']].groupby(['Year', 'Month'])
    xdf = xdf.agg({'Flow': lambda x: x.isnull().sum()}).reset_index()
    xdf.columns = ['Year', 'Month', 'count']
    xdf = xdf.pivot_table(index='Year', columns='Month', values='count')
    for i in range(1, 12):
        if i not in xdf:
            xdf[str(i)] = 0
    xdf = xdf[[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]]
    xdf.columns = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                   'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

    # Verbose
    if(args.v):
        print_full(xdf)

    # By Water Year
    xdf2 = df[['WYear', 'Month', 'Flow']].groupby(['WYear', 'Month'])
    xdf2 = xdf2.agg({'Flow': lambda x: x.isnull().sum()}).reset_index()
    xdf2.columns = ['WYear', 'Month', 'count']
    xdf2 = xdf2.pivot_table(index='WYear', columns='Month', values='count')
    for i in range(1, 12):
        if i not in xdf2:
            xdf2[str(i)] = 0
    xdf2 = xdf2[[10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9]]
    xdf2.columns = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb',
                    'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']

    # Verbose
    if(args.v):
        print_full(xdf2)

    # Output
    xdf.to_csv(outDiagn + "-year.csv", header=args.headout)
    xdf2.to_csv(outDiagn + "-wyear.csv", header=args.headout)
