#!/usr/bin/python3
import argparse
import re
import pandas as pd
import numpy as np
import scipy as sp
import math
from scipy.stats import genextreme as gev
import datetime as dt
import os
import sys
import decimal

########################################################################################
### Calculates metrics for a given daily stream data
#
# jwon - last edit 6/7/2016
# 06/06/2016 - Added -k option to keep NaN data with threshold
#            - Fixed the dates, wday, and flow optiosn
#            - Added -h header option
# 06/07/2016 - Fixed the -k options for duplicate entries. Added -k implementations for months
#            - Fixed diagnostic view for missing months
# 06/20/2016 - Added decimals for trailing zeros
########################################################################################


parser = argparse.ArgumentParser()
parser.add_argument('input_file')
parser.add_argument('-t', nargs='?', default=0, help='Apply a threshold for NaN calculations. Choose between 0 to 1.')     # Threshold (between 0 to 1).
parser.add_argument('-d', action='store_true', default=False, 
help='Creates a daily data file.')                                # Daily Data
parser.add_argument('-m', action='store_true', default=False, 
help='Calculates average monthly data.')                          # Monthly Average
parser.add_argument('-p', action='store_true', default=False,
help='Calculates peak flow by water year.')                       # Water yearly Peak flow + Date
parser.add_argument('-l', action='store_true', default=False, 
help='Calculates minimum 7-day average flow by water year.')      # Water yearly min 7-day rolling flow
parser.add_argument('-g', action='store_true', default=False, 
help='Calculates monthly averages grouped by months.')            # Monthly Average + group by months
parser.add_argument('-e', action='store_true', default=False, 
help='Calculates extreme statistics.')                            # Extreme statistics
parser.add_argument('-s', action='store_true', default=False, 
help='Strips the dates to start and end on water year.')          # Strips the date to water years
parser.add_argument('-x', action='store_true', default=False, 
help='Diagnostics table showing NA for each month and year')      # Diagnostics 
parser.add_argument('-k', action='store_true', default=False, 
help='Keep NaN in dataset when using threshold.')                 # Keep NaN with threshold 
parser.add_argument('-v', action='store_true', default=False, 
help='Outputs results on screen.')                                # Output results on screen
parser.add_argument('--header', action='store_true', default=False, 
help='Parses out the header.')                                    # Parses out header
parser.add_argument('--start', nargs='?', type=int, default=0, 
help="Specify start year to cut.")                                # Cut data to only include data after given start year
parser.add_argument('--end', nargs='?', type=int, default=0, 
help="Specify end year to cut.")                                  # Cut data to only include data before given end year
parser.add_argument('--dates', action='store_true', default=False, 
help='Creates a dates only csv for peak or low flow.')            #
parser.add_argument('--wday', action='store_true', default=False, 
help='Creates a wday only csv for peak or low flow.')             #
parser.add_argument('--flow', action='store_true', default=False, 
help='Creates a flow only csv for peak or low flow.')             #
parser.add_argument('--quantiles', action='store_true', default=False, 
help='Create a quantile file during stat calculations.')          #
parser.add_argument('out_dir', nargs='?', default=os.getcwd() + '/')
args = parser.parse_args()

infile = args.input_file

if float(args.t) < 0 or float(args.t) > 1:
    print('ERROR: Threshold out of range. Please choose between 0 and 1.')
    sys.exit()

## Functions ##
def get_wyear(year, month):
    if (month > 9):
        return year + 1
    else:
       return year
                    
def print_full(x):
    pd.set_option('display.max_rows', len(x))
    print(x)
    pd.reset_option('display.max_rows')    


## Variables                    
thold = float(args.t)
prec = 2
naVal = 'NaN'
filename = os.path.splitext(os.path.basename(infile))[0].split('.')[0]
outDaily = args.out_dir + filename + "_DailyFlows.csv"
outMonthAvg = args.out_dir + filename + "_MonthlyFlows.csv"
outPeak = args.out_dir + filename + "_PeakFlows"
outLow = args.out_dir + filename + "_LowFlows"
outGroupMonth = args.out_dir + filename + "_MonthlyAvg.csv"
outDiagn = args.out_dir + filename + "_NAView"
outExQPeak = args.out_dir + filename + "_PeakQuantile.csv"
outExQLow = args.out_dir + filename + "_LowQuantile.csv"
outExSPeak = args.out_dir + filename + "_PeakStats.csv"
outExSLow = args.out_dir + filename + "_LowStats.csv" 

## Intializing dataframes
mdf = pd.DataFrame()   # Monthly Average Dataframe
pdf = pd.DataFrame()   # Water year Peak Dataframe
ldf = pd.DataFrame()   # Water year min 7 day average Dataframe
gdf = pd.DataFrame()   # Monthly Grouped average Dataframe
edf = pd.DataFrame()   # Extreme stats Dataframe
xdf = pd.DataFrame()   # Diagnostics Dataframe
tmdf = pd.DataFrame()  # Threshold Monthly Dataframe
tydf = pd.DataFrame()  # Threshold Yearly Dataframe
gy = pd.DataFrame()    # WYear only Dataframe (For adding back NaN)
gm = pd.DataFrame()    # WYear + Month only Dataframe (For adding back NaN)

#-------------------------Preperation----------------------------------------

# Load data
if(args.header):
    df = pd.read_csv(infile, sep="[-, /\t]*", engine='python', na_values=['NA', 'NaN', ' '])
else:
    df = pd.read_csv(infile, sep="[-, /\t]*", header=None, engine='python', na_values=['NA', 'NaN', ' '])    
df.columns = ['Year', 'Month', 'Day', 'Flow']

# Strip data to only include within given years
if(args.start > 0):
    df = df[df.Year >= args.start]

if(args.end > 0):
    df = df[df.Year <= args.end]
df = df.reset_index()

# Filter out non-numeric data
df['Flow'] = df['Flow'].convert_objects(convert_numeric=True) 
#df['Flow'] = pd.to_numeric(df['Flow'], errors='coerce')   # Requires pandas 0.17.0

# Get Water Years
df['WYear'] = np.vectorize(get_wyear)(df['Year'], df['Month'])
#print_full(df)

# Strip dates to match water years
if (args.s):
    ind = 0

    # TODO: Failsafe checks for missing data
    # Behavior Check: If the following year is empty, should it try the next year?
    
    # Earlier than 10/01
    if(df.iloc[0].Month < 10):
        # -> drop everything in that year to 10/01
        ind = df.loc[(df['Year']==df.iloc[0].Year) & (df['Month']==10) & (df['Day']== 1)].index[0]
    # Later than 10/01
    if(df.iloc[0].Month >= 10 and df.iloc[0].Day > 1):
        # -> drop everything till 10/01 of the following year
        ind = df.loc[(df['Year']==df.iloc[0].Year+1) & (df['Month']==10) & (df['Day']== 1)].index[0]

    # Strip top
    if(ind > len(df.index)):
        print("ERROR: Not enough data to perform data stripping.")
        sys.exit()
    df = df[ind:].reset_index()
    
    
    # Ends later than 09/30
    if(df.iloc[-1].Month >= 10):
        # -> drop everything in that year to 9/30
        ind = df.loc[(df['Year']==df.iloc[-1].Year) & (df['Month']==9) & (df['Day']== 30)].index[0] + 1
        df = df[:ind]
    # Ends earlier than 09/30
    if(df.iloc[-1].Month <= 9 and df.iloc[-1].Day < 30):
        # -> drop everything till 09/30 of the previous year
        ind = df.loc[(df['Year']==df.iloc[-1].Year-1) & (df['Month']==9) & (df['Day']== 30)].index[0] + 1
        df = df[:ind]

    #TODO: check there is enough data when stripping from below
        
# Clear out data outside of threshold for months
def threshold_m(df, gm):
    tmdf = df[['Year', 'Month', 'Flow']].groupby(['Year', 'Month']).agg(['count', 'size']).reset_index()
    tmdf.columns = tmdf.columns.droplevel()
    tmdf.columns = ['Year', 'Month', 'count', 'size']
    tmdf['Thold'] = tmdf['count'].div(tmdf['size'], axis='index')
    tmdf = tmdf[tmdf.Thold >= thold]
    tmdf = pd.merge(tmdf[['Year', 'Month']], df, on=['Year', 'Month'], how='left')
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
    tydf = df[['WYear', 'Flow']].groupby(['WYear']).agg(['count', 'size']).reset_index()
    tydf.columns = tydf.columns.droplevel()
    tydf.columns = ['WYear', 'count', 'size']
    tydf['Thold'] = tydf['count'].div(tydf['size'], axis='index')
    tydf = tydf[tydf.Thold >= thold]
    tydf = pd.merge(tydf[['WYear']], df, on=['WYear'], how='left')
    if(args.k):
        #tydf = pd.merge(df[['Year', 'Month', 'Day', 'WYear']], tydf, on=['Year', 'Month', 'Day', 'WYear'], how='left')
        gy = df.groupby(['WYear']).count()
        gy = gy.reset_index()[['WYear']]
    tydf = tydf[['WYear', 'Year', 'Month', 'Day', 'Flow']]    

    # Check that there is enough data
    if(tydf.empty):
        print("ERROR: Not enough data for processing.")
        sys.exit()
    
    return (tydf,gy)

##-----------------------------------------Calculations--------------------------------------------
# Daily Data
if (args.d):
    print('Formatting Daily Data: ' + filename)
    (tydf, gy) = threshold_y(df, gy)

    # Keep NaN
    if(args.k):
        tydf = pd.merge(df[['WYear', 'Year', 'Month', 'Day']], tydf, on=['WYear', 'Year', 'Month', 'Day'], how='left')
    
    # Verbose
    if(args.v):
        print(tydf)

    # Output
    tydf[['Year', 'Month', 'Day', 'Flow']].fillna(naVal).to_csv(outDaily, index=False)

##------------------------------------------------------------------------------------------------
# Calculate monthly averages
def get_month(df):
    mdf = tmdf[['Year', 'Month', 'Flow']].groupby(['Year', 'Month']).agg(['mean']).reset_index()
    mdf.columns = mdf.columns.droplevel()
    
    ## Cleanup
    mdf[['mean']] = np.round(mdf[['mean']].astype(np.double), prec)
    #mdf[['mean']] = mdf[['mean']].map()
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
    mdf.fillna(naVal).to_csv(outMonthAvg, index=False)


##------------------------------------------------------------------------------------------------
# Calculate Peak flow by water year    
def get_peak(tydf):
    pdf = tydf[tydf.groupby(['WYear'], sort=False)['Flow'].transform(max)==tydf['Flow']].copy()
    pdf.drop_duplicates(['WYear'], inplace=True)
    
    # Cleanup
    pdf[['Flow']] = np.round(pdf[['Flow']].astype(np.double), prec)
    pdf.rename(columns={'Flow':'PeakFlow'}, inplace=True)
    
    return pdf
    
if (args.p):
    print('Calculating Water Yearly Peak: ' + filename)
    if(tydf.empty):
        (tydf,gy) = threshold_y(df, gy)
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
        pdft['Date'] = pdft.apply(lambda x: str('{0:0>4g}'.format(x.Year)) + "-" + str('{0:0>2g}'.format(x.Month)) + "-" + str('{0:0>2g}'.format(x.Day))
                                if pd.notnull(x.PeakFlow) else x.PeakFlow, axis=1)        
        pdft[['WYear', 'Date']].fillna(naVal).to_csv(outPeak + "-dates.csv", index=False)

    if(args.wday):
        pdft = pdf.copy()
        pdft['FDate'] = pdft.apply(lambda x: str('{0:0>4g}'.format(x.Year + 1)) + "-" + str('{0:0>2g}'.format(x.Month)) + "-" + str('{0:0>2g}'.format(x.Day))
                                 if pd.notnull(x.PeakFlow) else x.PeakFlow, axis=1)
        pdft['FDate'] = pd.to_datetime(pdft['FDate'])
        pdft['SDate'] = pdft.apply(lambda x: str(x.WYear) + "-09-30", axis=1)
        pdft['SDate'] = pd.to_datetime(pdft['SDate'])
        pdft['DoWY'] = (pdft['FDate'] - pdft['SDate']) / np.timedelta64(1, 'D')

        pdft[['WYear', 'DoWY']].fillna(naVal).to_csv(outPeak + "-wday.csv", index=False)

    if(args.flow):
        pdf[['WYear', 'PeakFlow']].fillna(naVal).to_csv(outPeak + "-Flow.csv", index=False)

    if(not(args.dates & args.wday & args.flow)):
        pdf.fillna(naVal).to_csv(outPeak + ".csv", index=False)


##------------------------------------------------------------------------------------------------
# Calculate 7-day minimum average flow by water year. Date returned is the last day of the window
def get_low(tydf):
    if(tydf.empty):
        (tydf,gy) = threshold_y(df,gy)
    ldf = tydf
    ldf['LowFlow'] = pd.rolling_mean(tydf['Flow'], window=7)#.shift(-3)     # Shift for which day is the center
    ldf = ldf.loc[ldf.groupby('WYear')['LowFlow'].idxmin()].reset_index()
    ldf['WYear'] = pd.unique(tydf.WYear.ravel())
    ldf.drop_duplicates(['WYear'], inplace=True)


    # Cleanup
    ldf[['LowFlow']] = np.round(ldf[['LowFlow']].astype(np.double), prec)
    ldf = ldf[['WYear', 'Year', 'Month', 'Day', 'LowFlow']]

    return ldf

if (args.l):
    print('Calculating Water Yearly 7-day Low avg: ' + filename)
    if(tydf.empty):
        (tydf,gy) = threshold_y(df, gy)
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
        ldft['Date'] = ldft.apply(lambda x: str('{0:0>4g}'.format(x.Year)) + "-" + str('{0:0>2g}'.format(x.Month)) + "-" + str('{0:0>2g}'.format(x.Day))
                                if pd.notnull(x.LowFlow) else x.LowFlow, axis=1)        
        ldft[['WYear', 'Date']].fillna(naVal).to_csv(outLow + "-dates.csv", index=False)
        
    if(args.wday):
        ldft = ldf.copy()
        ldft['FDate'] = ldft.apply(lambda x: str('{0:0>4g}'.format(x.Year + 1)) + "-" + str('{0:0>2g}'.format(x.Month)) + "-" + str('{0:0>2g}'.format(x.Day))
                                 if pd.notnull(x.LowFlow) else x.LowFlow, axis=1)
        ldft['FDate'] = pd.to_datetime(ldft['FDate'])
        ldft['SDate'] = ldft.apply(lambda x: str(x.WYear) + "-09-30", axis=1)
        ldft['SDate'] = pd.to_datetime(ldft['SDate'])
        ldft['DoWY'] = (ldft['FDate'] - ldft['SDate']) / np.timedelta64(1, 'D')

        ldft[['WYear', 'DoWY']].fillna(naVal).to_csv(outLow + "-wday.csv", index=False)
        
    if(args.flow):
        ldf[['WYear', 'LowFlow']].fillna(naVal).to_csv(outLow + "-Flow.csv", index=False)

    if(not(args.dates & args.wday & args.flow)): 
        ldf.fillna(naVal).to_csv(outLow + ".csv", index=False)    

##------------------------------------------------------------------------------------------------
# Calculate Monthly Averages grouped by month
if (args.g):
    print('Calculating Monthly Average grouped by Month: ' + filename)
    if(tmdf.empty):
        (tmdf,gm) = threshold_m(df,gm)
    if(mdf.empty):
        mdf = get_month(tmdf)
    gdf = mdf[['Month', 'Avg.Flow']].groupby('Month').agg(['mean']).reset_index()
    gdf.columns = gdf.columns.droplevel()
    gdf = gdf.reindex(index=np.roll(gdf.index,3))
    gdf.columns = ['Month', 'Avg.Flow']

    #Cleanup
    gdf[['Avg.Flow']] = np.round(gdf[['Avg.Flow']].astype(np.double), prec)

    # Verbose
    if(args.v):
        print(gdf)

    # Output
    gdf.fillna(naVal).to_csv(outGroupMonth, index=False)


##------------------------------------------------------------------------------------------------
def get_extreme_moment(edf):
    edf.dropna(inplace=True)
    edf.sort(['Flow'], ascending=False, inplace=True)
    edf = edf.reset_index()[['Flow']]
    edf['Rank'] = edf.index + 1
    #edf['Rank'] = edf['Flow'].rank(ascending=0)
    n = len(edf.index)
    edf['B1i'] = edf.apply(lambda x: (n - x.Rank) / (n*(n - 1)) * x.Flow, axis=1)
    edf['B2i'] = edf.apply(lambda x: ((n - x.Rank) * (n - x.Rank - 1)) / (n * (n - 1) * (n - 2)) * x.Flow, axis=1)
    edf['B3i'] = edf.apply(lambda x: ((n - x.Rank) * (n - x.Rank - 1) * (n-x.Rank-2)) / (n * (n - 1) * (n - 2) * (n-3)) * x.Flow, axis=1)

    return edf
    
def save_quantile(edf, outname):
    n = len(edf.index)
    edf['Quantile'] = edf.apply(lambda x: (x.Rank - 0.4) / (n + 0.2), axis=1)
    edf[['Quantile']] = np.round(edf[['Quantile']].astype(np.double), prec)
    
    edf[['Quantile', 'Flow']].to_csv(outname, index=False)    

def get_extreme(edf, isFlood):

    B = [0,0,0,0,0]
    L = [0,0,0,0,0]
    c = [0,0,0,0]
    kappa = [0,0,0,0]
    alpha = [0,0,0,0]
    psi = [0,0,0,0]

    B[0] = edf['Flow'].mean()
    B[1] = edf['B1i'].sum()
    B[2] = edf['B2i'].sum()
    B[3] = edf['B3i'].sum()
    
    L[1] = B[0]
    L[2] = 2 * B[1] - B[0]
    L[3] = 6 * B[2] - 6 * B[1] + B[0]

    #print("B0: " + str(B[0]) + " B1: " + str(B[1]) + " B2: " + str(B[2]) + " B3: " + str(B[3]))
    #print("L1: " + str(L[1]) + " L2: " + str(L[2]) + " L3: " + str(L[3]))


    # GEV distribution using L moments
    c[0] = (2* L[2]) / (L[3] + 3*L[2]) - 0.630930
    kappa[0] = 7.8590 * c[0] + 2.9554 * c[0] * c[0]
    gamma = math.gamma(1+kappa[0])
    alpha[0] = kappa[0] * L[2] / (gamma * (1-math.pow(2, -kappa[0])))
    psi[0] = L[1] + alpha[0] * (gamma - 1) / kappa[0]
    
    
    # GEV parameters based on LH2 moments (Wang 1997)
    a2 = [0.5914, -2.3351, 0.6442, -0.1616]

    
    kappa[1] = a2[0] + a2[1] 
    gamma = math.gamma(1 + kappa[1])
    #alpha[1] = LH2[1] / (2 * gamma / kappa[1] * math.e(-1 * kappa[1] * math.log(3.0)) - math.e(-1 * kappa[1] * math.log(40)))
    #psi[1] = LH2[0] - alpha[1] / kappa[1] * (1.0 - gamma * math.e(10 * kappa[1] * log(30)))

    # GEV parameters based on LH4 moments (Wang 1997)
    a4 = [0.7113, -2.5383, 0.5142, -0.1027]

    # EV1 parameters based on L moments
    alpha[3] = 1.443 * L[1]
    psi[3] = L[0] - 0.5772 * alpha[3]


    #print("c: " + str(c[0]) + " k: " + str(kappa[0]) )
    #print("gamma: " + str(gamma) + " alpha:  " + str(alpha[0]) + " psi: " + str(psi[0]))


    edf = pd.DataFrame(columns=['prob'])    
    if(isFlood):
        #edf.loc[0] = 0.500       #   2 Yr
        #edf.loc[1] = 0.900       #  10 Yr
        #edf.loc[2] = 0.950       #  20 Yr
        #edf.loc[3] = 0.980       #  50 Yr
        #edf.loc[4] = 0.990       # 100 Yr
        #edf.loc[5] = 0.995       # 200 Yr
        #edf.loc[6] = 0.998       # 500 Yr
        edf.loc[0] = 0.500
        edf.loc[1] = 0.900
        edf.loc[2] = 0.980
        edf.loc[3] = 0.990
        edf.loc[4] = 0.998

        

    else:        
        #edf.loc[0] = 0.005       # 200 Yr
        #edf.loc[1] = 0.010       # 100 Yr
        #edf.loc[2] = 0.020       #  50 Yr
        #edf.loc[3] = 0.050       #  20 Yr
        #edf.loc[4] = 0.100       #  10 Yr
        #edf.loc[5] = 0.500       #   2 Yr
        #edf.loc[6] = 0.990       #   1 Yr
        edf.loc[0] = 0.100
        edf.loc[1] = 0.500
        
    edf['dist0'] = edf.apply(lambda x: psi[0] + alpha[0]/kappa[0] * (1 - (math.pow(-math.log(x.prob), kappa[0]))), axis=1)
    edf.ix[:, edf.columns != 'prob'] = np.round(edf.ix[:,edf.columns != 'prob'].astype(np.double), prec)
    
    return edf

# Calculate extreme statistics
if (args.e):
    print('Calculating Extreme Flood Stats: ' + filename)
    if(tydf.empty):
        (tydf,gy) = threshold_y(df,gy)
    if(pdf.empty):
        pdf = get_peak(tydf)

    epdf = pdf[['PeakFlow']].copy()
    epdf.columns = ['Flow']    
    epdf = get_extreme_moment(epdf)

    # Quantiles
    if(args.quantiles):
        save_quantile(epdf, outExQPeak)
        
    epdf = get_extreme(epdf, True)
    
    # Verbose
    if(args.v):
        print("\nPeak Flows")
        print(epdf.set_index('prob').T)
        
    epdf['prob'] = epdf['prob'].apply(lambda x : round(1/ (1 - x)))
    epdf = epdf.rename(columns = {'prob':'returnYr'})
    epdf.to_csv(outExSPeak, index=False)

    ##--------------------------------------------------------------
    
    print('Calculating Extreme Low Stats: ' + filename)    
    if(ldf.empty):
        ldf = get_low(tydf)

    eldf = ldf[['LowFlow']].copy()    
    eldf.columns = ['Flow']
    eldf = get_extreme_moment(eldf)

    # Quantiles
    if(args.quantiles):
        save_quantile(eldf, outExQLow)

    eldf = get_extreme(eldf, False)

    # Verbose
    if(args.v):
        print("\nLow Flows")
        print(eldf.set_index('prob').T)
        
    #eldf.set_index('prob').T.to_csv(outExSLow)
    eldf['prob'] = eldf['prob'].apply(lambda x : round(1 / x))
    eldf = eldf.rename(columns = {'prob':'returnYr'})
    eldf.to_csv(outExSLow, index=False)

##------------------------------------------------------------------------------------------------
# Creates a diagnostics table showing how many NA values are found in each month of each year
if (args.x):
    print('Creating diagnostic table: ' + filename)

    # By Year
    xdf = df[['Year', 'Month', 'Flow']].groupby(['Year', 'Month']).agg({'Flow': lambda x: x.isnull().sum()}).reset_index()
    xdf.columns = ['Year', 'Month', 'count']
    xdf = xdf.pivot_table(index='Year', columns='Month', values='count')
    for i in range(1,12):
        if i not in xdf:
            xdf[str(i)] = 0
    xdf = xdf[[1,2,3,4,5,6,7,8,9,10,11,12]]    
    xdf.columns = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

    # Verbose
    if(args.v):
        print_full(xdf)

    # By Water Year
    xdf2 = df[['WYear', 'Month', 'Flow']].groupby(['WYear', 'Month']).agg({'Flow': lambda x: x.isnull().sum()}).reset_index()
    xdf2.columns = ['WYear', 'Month', 'count']
    xdf2 = xdf2.pivot_table(index='WYear', columns = 'Month', values='count')
    for i in range(1,12):
        if i not in xdf2:
            xdf2[str(i)] = 0
    xdf2 = xdf2[[10,11,12,1,2,3,4,5,6,7,8,9]]    
    xdf2.columns = ['Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep']

    # Verbose
    if(args.v):
        print_full(xdf2)

    # output
    xdf.to_csv(outDiagn + "-year.csv" )
    xdf2.to_csv(outDiagn + "-wyear.csv")
