# Useful functions 

##### CONVERT DATAFRAME COLUMN OF FACTORS TO DATAFRAME COLUMN OF NUMERIC VALUES
as.numeric.factor <- function(column) {as.numeric(levels(column))[column]}


##### CONVERT DATAFRAME OF FACTORS TO MATRIX OF NUMERIC VALUES
df_factor_to_matrix_numeric <- function(df) {as.matrix(as.data.frame(lapply(df, as.numeric.factor)))}


##### FIND THE MINIMUM VALUE OF A MINIMUM CLUSTER
# Computes a rolling average of 4 observations in order to identify clusters of low errors (regions of mimima)
# This is useful since the single lowest error of all the set could be due to chance and be surrounded by significantly higher values
cluster_minimum <- function(dataset, window_length)
{
  l = window_length
  rolling_window = c() # Empty data container
  
  for (i in l:length(dataset)) # for every window of l observations
  {
    rolling_average = mean( dataset[(i-l+1):i] ) # compute its mean
    rolling_window[i-l+1] = rolling_average # save the result
  }
  
  names(rolling_window) =  l:length(dataset) # indice of the m of the last observation of the window (reference observation)
  # Pick the cluster with the lowest average error
  cluster_index =  which.min(rolling_window)
  reference_index = as.numeric( names(rolling_window)[cluster_index] )
  cluster = dataset[(reference_index-l+1): reference_index]
  # Pick the m corresponding to the lowest error among that minimum cluster
  minimimum_error_index = which.min( cluster )
  minimum = cluster[minimimum_error_index]
  minimum_name = names(minimum )
    
  return(list( minimum_name, minimum, cluster )) }


##### MAKE CLUSTER (foreach)
# Defining the cluster definition depending if under unix or windows OS (for parallel computing)
make_cluster <- function()
{
  if(.Platform[1]=="unix")
  { cl <- makeCluster(detectCores()-1) # FOR MAC
    return(cl)
  }else if(.Platform[1]=="windows") 
  { cl <- makeCluster(detectCores()-1, type="SOCK") # FOR WINDOWS
    return(cl)
  }
}


##### MAKE CLUSTER (foreach)
# Defining the cluster definition depending if under unix or windows OS (for parallel computing)
cluster_export <- function(cl,export_list,envir)
{
  if(.Platform[1]=="unix")
  {
    return( clusterExport(cl=cl, varlist = export_list, envir = envir) ) # FOR MAC
  }else if(.Platform[1]=="windows") 
  { 
    return( clusterExport(cl=cl, export_list, envir = envir) ) # FOR WINDOWS
  }
}


##### MATRIX TO A DATAFRAME OF COORDINATES (3 COLUMNS: X,Y,Z)
# ROWS MUST BE NAMED AS THE Y VALUES
# COLUMNS MUST BE NAMED AS THE X VALUES
# THE VALUES OF THE MATRIX ARE THE VALUES OF Z
# x_increments and y_increments are for ex: x=1,2,3,4 (increment=1), x=2,4,6,8 (increment=2)...
# names must be strings
coordinates_df <- function(data,x_increments,y_increments,x_name,y_name,z_name)
{
  coordinates_df = data.frame( k=as.vector(col(data)), n=as.vector(row(data)), Errors=as.vector(data))
  coordinates_df$k = coordinates_df$k *x_increments
  coordinates_df$n = coordinates_df$n *y_increments
  names(coordinates_df)[1] = x_name
  names(coordinates_df)[2] = y_name
  names(coordinates_df)[3] = z_name
  return(coordinates_df)
}
  