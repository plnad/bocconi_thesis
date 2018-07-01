##### PRODUCE ONE SIMULATED ERRORS RESULT IN THE SEARCH OF THE OPTIMAL M REGION
one_result_m_s = function(n, k, option1,option2,option3, b, iterations)
{
  # x is an [(number of days) x (number of stocks)] returns matrix, or following the paper: [n x k]
  x = get_log_returns_sp500(k,252)
  x = x[(rowSums(x)!=0),] # Remove the days where no stocks in the dataset were trading
  
  # Number of random correlated observations
  x = normalize_matrix(x)
  benchmark = cov.shrink(x)
  x_random_correlated = random_correlated_matrix(benchmark,n)
  x = x_random_correlated
  
  # Parameters for Abadir estimators
  m_s_optimisation = seq(max(round(0.05*n),2),min(round(n*0.95),n-1),ceiling(n/50)) # Vector of m to try in the optimisation
  
  # Parallel computation
  # Defining the cluster definition depending if under unix or windows OS (for parallel computing)
  cl = make_cluster()
  registerDoParallel(cl)
  output_optimisation = foreach(i = 1:iterations,
                                .combine = c,
                                .packages = packages,
                                .export = c("find_optimal_m_both","x","m_s_optimisation","b","benchmark","option1", "option2", "option3")
  ) %dopar% find_optimal_m_both(x,m_s_optimisation,b,benchmark,option1,option2,option3)
  
  stopImplicitCluster()
  
  errors = sapply(0:(iterations-1), function(i) output_optimisation[(2+i*4):(4+i*4)]) # This is going to  give a list of 30 with 3 rows in each list. Access element 3 of the 25th list is b[3,25]
  average_error_both = Reduce("+",errors[1,]) /iterations
  m_cluster = cluster_minimum(average_error_both,10)
  m_s_optimal = as.numeric(names(m_cluster[[3]]))

  return(errors) }

##### FOR THE SEARCH OF THE OPTIMAL M REGION
##### PRODUCE A MATRIX OF ERRORS RESULTS FOR EACH COMBINATION ON THE GRID FORMED BY n_s and n_k
##### FOR EVERY POINT ON IT, SIMULATIONS FOR EACH M WITHIN A RANGE ARE PERFORMED
collect_results_m_s = function(n_s, k_s, option1,option2,option3,  b,iterations)
{
  results = matrix(list(), nrow = length(n_s), ncol = length(k_s))
  rownames(results) = n_s
  colnames(results) = k_s
  n_points = length(n_s)*length(k_s)
  
  for (i in 1:length(n_s))
  {
    i=1
    for (j in 1:length(k_s))
    {
      current_point = (i-1)*length(n_s) + j
      results[[i,j]] = one_result_m_s(n_s[i],k_s[j], option1,option2,option3, b, iterations)
      print(paste("Row:(",i,") Done"," Overall progress:",round(i/length(n_s),4)*100, "% "))
      flush.console()
    }
    save(results, file=file.path(path, "results_optimisation.rda"))
  }
  return(results)}


##### PRODUCE ONE ERROR RESULT OF A SIMULATION (CLASSICAL COVARIANCE MATRIX)
one_result_cov = function(x, n, k)
{
  source("compute_matrices.R")
  library("tawny")
  library("mvtnorm")
  library("matrixcalc")
  
  x = normalize_matrix(x)
  benchmark = cov.shrink(x)
  x_random_correlated = random_correlated_matrix(benchmark,n)
  x = x_random_correlated
  
  output_estimator = classic_covariance_matrix(x)
  
  error = vech_norm(correlation_matrix(output_estimator)-correlation_matrix(benchmark),2)
  
  return( error ) }


##### PRODUCE A MATRIX OF SIMULATED CLASSICAL COVARIANCE MATRIX ERRORS FOR EACH OF THE COMBINATIONS ON THE GRID FORMED BY n_s and k_s
collect_results_cov = function(n_s, k_s)
{
  # Data parameters
  days = 252 # Days sample market data before producing the benchmark 
  
  results = matrix(nrow = length(n_s), ncol = length(k_s))
  rownames(results) = n_s
  colnames(results) = k_s
  n_points = length(n_s)*length(k_s)
  
  for (i in 1:length(n_s))
  {
    x_s = lapply(1:length(k_s),
                 function(j) get_log_returns_sp500(k_s[j],days))
    
    f <- function(j)
    {
      x = matrix(x_s[[j]], byrow = 1, ncol = k_s[j])
      one_result_cov(x, n_s[i],k_s[j])
    }
    
    # Parallel computing
    cl <- makePSOCKcluster(detectCores()-1)
    export_list = list( "n_s","k_s","one_result_cov")
    cluster_export(cl,export_list,environment())
    output_estimators = parLapply(cl, 1:length(k_s), f)
    stopCluster(cl)
    
    # Put the results in the matrix
    for (j in 1:length(k_s))
    {
      current_point = (i-1)*length(n_s) + j
      results[i,j] = output_estimators[[j]]
    }
    print(paste("Row:(",i,") Done"," Overall progress:",round(i/length(n_s),4)*100, "% "))
    flush.console()
  }
  
  return(results)}


##### PRODUCE ONE ERROR RESULT OF A SIMULATION (SHRINKAGE COVARIANCE MATRIX)
one_result_shrink = function(x, n, k)
{
  source("compute_matrices.R")
  library("tawny")
  library("mvtnorm")
  library("matrixcalc")
  
  # Number of random correlated observations
  x = normalize_matrix(x)
  benchmark = cov.shrink(x)
  x_random_correlated = random_correlated_matrix(benchmark,n)
  x = x_random_correlated
  
  output_estimator = cov.shrink(x)
  
  error = vech_norm(correlation_matrix(output_estimator)-correlation_matrix(benchmark),2)
  
  return( error ) }


##### PRODUCE A MATRIX OF SIMULATED SHRINKAGE COVARIANCE MATRIX ERRORS FOR EACH OF THE COMBINATIONS ON THE GRID FORMED BY n_s and k_s
collect_results_shrink = function(n_s, k_s)
{
  # Data parameters
  days = 252 # Days sample market data before producing the benchmark 
  results = matrix(nrow = length(n_s), ncol = length(k_s))
  rownames(results) = n_s
  colnames(results) = k_s
  n_points = length(n_s)*length(k_s)
  
  for (i in 1:length(n_s))
  {
    x_s = lapply(1:length(k_s),
                 function(j) get_log_returns_sp500(k_s[j],days))
    
    f <- function(j)
    {
      x = matrix(x_s[[j]], byrow = 1, ncol = k_s[j])
      one_result_shrink(x, n_s[i],k_s[j])
    }
    # Parallel computing
    cl <- makePSOCKcluster(detectCores()-1)
    export_list = list( "n_s","k_s","one_result_shrink")
    cluster_export(cl,export_list,environment())
    output_estimators = parLapply(cl, 1:length(k_s), f)
    stopCluster(cl)
    
    # Put the results in the matrix
    for (j in 1:length(k_s))
    {
      current_point = (i-1)*length(n_s) + j
      results[i,j] = output_estimators[[j]]
    }
    print(paste("Row:(",i,") Done"," Overall progress:",round(i/length(n_s),4)*100, "% "))
    flush.console()
  }
  return(results)}


##### PRODUCE ERROR RESULTS OF ONE GENERAL ESTIMATOR SIMULATION 
one_result_generalized_m = function(x, n, k, option1,option2,option3, s,m_lower,m_upper,granularity, n_points,current_point)
{
  # Number of random correlated observations
  x = normalize_matrix(x)
  benchmark = cov.shrink(x)
  x_random_correlated = random_correlated_matrix(benchmark,n)
  x = x_random_correlated
  
  # Parameters for Abadir estimators
  m_s = seq(max(round(m_lower*n),2),min(round(n*m_upper),n-1),ceiling(n*granularity)) # Vector of m to try in the optimisation
  
  output_estimator = estimator_generalized_m_averaged_par(x,m_s,s,option1,option2,option3)
  
  errors = vech_norm(correlation_matrix(output_estimator)-correlation_matrix(benchmark),2)
  
  return( errors ) }


##### PRODUCE A MATRIX OF SIMULATED GENERAL ESTIMATOR ERRORS FOR EACH OF THE COMBINATIONS ON THE GRID FORMED BY n_s and k_s
collect_results_generalized_m = function(n_s, k_s, option1,option2,option3,  s,m_lower,m_upper,granularity)
{
  # Data parameters
  days = 252 # Days sample market data before producing the benchmark 
  
  results = matrix(nrow = length(n_s), ncol = length(k_s))
  rownames(results) = n_s
  colnames(results) = k_s
  n_points = length(n_s)*length(k_s)
  
  for (i in 1:length(n_s))
  {
    for (j in 1:length(k_s))
    {
      current_point = (i-1)*length(k_s) + j
      x_s = lapply(1:length(k_s),
                   function(j) get_log_returns_sp500(k_s[j],days))
      x = matrix(x_s[[j]], byrow = 1, ncol = k_s[j])
      
      results[[i,j]] = one_result_generalized_m(x, n_s[i],k_s[j], option1,option2,option3, s,m_lower,m_upper,granularity, n_points,current_point)
      print(paste("Element:(",i,",",j,") Done"," Overall progress:",round(current_point/(length(n_s)*length(k_s)),4)*100, "% "))
      flush.console()
    }
    save(results, file=file.path(path2output, "results_generalized_row.rda"))
  }
  return(results)}


##### PRODUCE ERROR RESULTS OF ONE GRAND AVERAGE ESTIMATOR BOOTSTRAP 
one_result_bootstrap = function(x, n, k, m, b, option1,option2,option3)
{
  # Number of random correlated observations
  x = normalize_matrix(x)
  benchmark = cov.shrink(x)
  x_random_correlated = random_correlated_matrix(benchmark,n)
  x = x_random_correlated
  
  output_estimator = bootstrap_par(x,m,b,benchmark,option1,option2,option3)
  
  errors = vech_norm(correlation_matrix(output_estimator)-correlation_matrix(benchmark),2)
  
  return( errors ) }


##### PRODUCE A MATRIX OF GRAND AVERAGE ESTIMATOR BOOTSTRAPS ERRORS FOR EACH OF THE COMBINATIONS ON THE GRID FORMED BY n_s and k_s
collect_results_bootstrap = function(n_s, k_s, m_percent, option1,option2,option3,  b)
{
  # Data parameters
  days = 252 # Days sample market data before producing the benchmark 
  
  results = matrix(nrow = length(n_s), ncol = length(k_s))
  m_s = round(n_s*m_percent)
  rownames(results) = n_s
  colnames(results) = k_s
  n_points = length(n_s)*length(k_s)
  
  for (i in 1:length(n_s))
  {
    for (j in 1:length(k_s))
    {
      current_point = (i-1)*length(k_s) + j
      x_s = lapply(1:length(k_s),
                   function(j) get_log_returns_sp500(k_s[j],days))
      x = matrix(x_s[[j]], byrow = 1, ncol = k_s[j])
      
      results[[i,j]] = one_result_bootstrap(x, n_s[i],k_s[j], m_s[i], b, option1,option2,option3)
      print(paste("Element:(",i,",",j,") Done"," Overall progress:",round(current_point/(length(n_s)*length(k_s)),4)*100, "% "))
      flush.console()
    }
    save(results, file=file.path(path2output, "results_bootstrap_row.rda"))
  }
  return(results)}
