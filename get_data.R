source("compute_matrices.R")
source("useful.R")
 
 
##### GET THE CLOSEST DATES TO THOSE REQUESTED
get_dates <- function(dates,start_date,end_date)
{
  temp_index = 0
  condition = TRUE
  temp_start_date = start_date
  while (condition)
  {
    temp_index = match(temp_start_date, dates)
    condition = is.na(temp_index)
    if(condition)
    {
      new_date = as.Date(temp_start_date) # Convert the date in a Date object
      new_date = new_date+1 # Add one day
      temp_start_date = as.character(new_date) # Convert back the date in the original Character object
    }
  }
  condition = TRUE
  temp_end_date = end_date
  while (condition)
  {
    temp_index = match(temp_end_date, dates)
    condition = is.na(temp_index)
    if(condition)
    {
      new_date = as.Date(temp_end_date) # Convert the date in a Date object
      new_date = new_date-1 # Remove one day
      temp_end_date = as.character(new_date) # Convert back the date in the original Character object
    }
  }

  return(c(temp_start_date, temp_end_date))
}


##### GET THE MATRIX OF PRICES
get_closing_prices <- function(tickers,start_date,end_date)
{
  # Create the column of dates for the matrix of stock prices
  matching_dates = get_dates(dates_vector,start_date,end_date)
  index_start = match(matching_dates[1], dates_vector)
  index_end = match(matching_dates[2], dates_vector)
  subset_dates_vector = dates_vector[index_start:index_end]
  
  output = cbind(subset_dates_vector) # Data container for the matrix of prices
  
  for (i in 1:length(tickers)) 
  {
    current_ticker = tickers[i]
    # Slice the subset of price for that ticker
    temp_output = stocks[[current_ticker]][index_start:index_end,]
    colnames(temp_output)[2]= current_ticker
    output = merge(output,temp_output,by = 1,all = 1)
  }
  
  return(output)
}


# k is the number of stocks
get_log_returns_sp500 <- function(k,days)
{
  last_day = length(sp500crsp[[1]])
  raw_output = sapply(1:k, function(i) sp500crsp[[i]][(last_day-days+1):last_day])
  data_df = data.frame(raw_output)
  data_matrix = df_factor_to_matrix_numeric(data_df)
  log_returns = log(data_matrix[2:dim(data_matrix)[1],] / data_matrix[1:(dim(data_matrix)[1]-1),]) 
  if(sum(is.na(log_returns))) {browser()}
  return(log_returns)
}
