# Preparing and cleaning the data

# CRSP database of the daily stock prices of the S&P 500
path = getwd()
source("header.R")
database = read.table("sp500data.csv", header = 1, sep = ",", as.is = 1, quote="")
sp500crsp = database

# Tickers and the index of each stock start and end date
tickers = data.matrix(unique(sp500crsp[,1]))
col1 = sp500crsp[,1]
indexes_start = match(tickers, col1)
indexes_end = indexes_start - 1
indexes_end[1:(length(indexes_end)-1)] = indexes_end[2:length(indexes_end)]
indexes_end[length(indexes_end)] = length(col1)

# We will keep only those having at least x days of data 
indexes_end - indexes_start # Shows that most of the stocks contains 2265 datapoints
to_keep = (indexes_end - indexes_start) == 2265

# Create a vector of unique dates
sample = sp500crsp[1:indexes_end[1],] # (indexes_end - indexes_start) shows also that the first stock contains those 2265 dates
dates_vector = unique(as.character(sample[,2])) # unique list of dates
dates_vector = as.Date(dates_vector,format = "%m/%d/%Y") # better date format
dates_vector = dates_vector[ order(dates_vector) ] # put the dates in order

# Convert also  all the dates of the database to the same format
dates_column = as.character(sp500crsp[,2])
dates_column = as.Date(dates_column,format = "%m/%d/%Y") # better date format
sp500crsp[,2] = dates_column

# All stocks time series will have the same set of date: dates_vector
# When a price is missing at one point, we will carry over the last value
stocks_list=c() # Empty data container
temp_col = cbind(1:length(dates_vector)) # Empty data container
standardized_matrix = data.frame(dates_vector,temp_col)
for (i in 1:length(tickers))
{
  if(to_keep[i]==1) # If the current stock is to be kept, add it to the list
  {
    # Data of the current stock
    current_ticker = tickers[i]
    start = indexes_start[i]
    end = indexes_end[i]
    chunk = sp500crsp[start:end,2:3]
    
    # Merge to standardized with same set of dates
    current_stock = merge(standardized_matrix,chunk,by=1,all=1)
    
    # Carry over the last value when NA
    stocks_zoo = zoo(current_stock)
    stocks_zoo = na.locf(stocks_zoo)
    
    # Add it to the list and name it
    stocks_list$temp = stocks_zoo
    index = length(stocks_list)
    names(stocks_list)[index] = current_ticker
    
    # Keep only the price column
    stocks_list[[index]] = stocks_list[[index]][,-2]
    stocks_list[[index]] = stocks_list[[index]][,-1]
  }
  
}
sp500crsp = stocks_list

# repackage the data into a dataframe
df=data.frame( sapply(1:length(sp500crsp), function(i) sp500crsp[[i]][,]) )
df=df_factor_to_matrix_numeric(df)

# check for NA's:
row_sums=rowSums(df)
col_sums=colSums(df)
sum(is.na(row_sums)) # lots of rows containing NA's (2116)
sum(is.na(col_sums)) # only one column containing NA's

# deal with NA's: 
col_sums # it is column 347
alt_df = df[,-347] # by removing it ...
row_sums=rowSums(alt_df)
col_sums=colSums(alt_df)
sum(is.na(row_sums)) # no row containing NA anymore !
sum(is.na(col_sums)) # no column containing NA anymore !

sp500crsp = sp500crsp[-347] # Drop that column. 
sp500crsp = sp500crsp[-233] # Drop also that column, it produced NA's when using the data.
sp500crsp = sp500crsp[-262] # Drop also that column, it produced NA's when using the data. Now we have clean data
save(sp500crsp, file=file.path(path, "sp500crsp.rda"))

