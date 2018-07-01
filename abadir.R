
# Every function in this file requires:
# x: a matrix of data
# m: the cutoff row to split the rows of x in two subset
# option1: (0): use the mean of the matrix of the subset created by splitting the matrix (as in equations (2.5))
  # or (1): use the mean of the whole x (as in proposition (2.1))
# option2: (0): use sigma as defined in equation (2.5) 
  # or (1): use sigma=cov.shrink(x) as instead 
# option3: (0): use the eigenvectors as defined in equation (2.6)
  # or (1): use the eigenvectors from the first subset of x
  # or (2): use the eigenvectors from the second subset of x
  # or (3): use the eigenvector from sigma=cov.shrink(x)
# So the basic model showned in the paper uses the 3 respective options as (0,0,0) or (1,0,0)
# See the definition of the function "estimator" to see the implementation
# The equations referred to are the ones in Abadir's used at the time, available in the same folder as this code

##### NEW ESTIMATOR
# Compute one covariance matrix SIGMA~ from equation (2.6)
estimator <- function(x,m,option1,option2,option3)
{
  source("compute_matrices.R")
  source("abadir.R")
  library("matrixcalc")
  library("tawny")
  
  n = dim(x)[1]
  k = dim(x)[2]
  
  ### Equation (2.3) (split the data into 2 matrices x1 and x2)
  x1 = x[1:m,]# [m x k]
  x2 = x[(m+1):n,]# [(n-m) x k]
  
  ##### CLASSIC COVARIANCE MATRIX (MARKOWITZ)
  # Compute the covariance matrix named SIGMA
  sigma = classic_covariance_matrix(x) # [k x k] 
  # Create the spectral decomposition of SIGMA
  output_spectral_decomposition = spectral_decomposition(sigma,k)
  # Unload the output and transform list elements as matrices
  lambda = matrix(unlist(output_spectral_decomposition[1]),ncol = k,byrow = 1)
  p = matrix(unlist(output_spectral_decomposition[2]),ncol = k,byrow = 1)
  
  ### Equation (2.4) SIGMA1 decomposition (SIGMA1 = P1 LAMBDA1 P1')
  # Same procedure for the covariance matrix named SIGMA1
  sigma1 = matrix(ncol = k, nrow = k)
  if (option2 == 0) { sigma1 = classic_covariance_matrix(x1) 
  }else { sigma1 = cov.shrink(x1)  }
  
  output_spectral_decomposition1 = spectral_decomposition(sigma1,k)
  lambda1 = matrix(unlist(output_spectral_decomposition1[1]),ncol = k,byrow = 1)
  p1 = matrix(unlist(output_spectral_decomposition1[2]),ncol = k,byrow = 1)
  
  ### Equation (2.5) computing lambda~
  # Compute the covariance matrix named SIGMA2
  sigma2 = matrix(ncol = k, nrow = k)
  if (option1 == 0) { sigma2 = classic_covariance_matrix(x2) 
  }else { sigma2 = classic_covariance_matrix(x2,x)  }

  # Compute lambda~
  lambda_tilde = diag( t(p1) %*% sigma2  %*% p1 )*identity_matrix(k)
  
  # Choice of the eigenvectors to be used in Equation (2.6)
  sigma3 = matrix(ncol = k, nrow = k)
  P = matrix(ncol = k, nrow = k)
  if (option3 == 0) { P=p
  }else if (option3 == 1) { P=p1
  }else if (option3 == 2) 
  {
    output_spectral_decomposition2 = spectral_decomposition(sigma2,k)
    lambda2 = matrix(unlist(output_spectral_decomposition2[1]),ncol = k,byrow = 1)
    p2 = matrix(unlist(output_spectral_decomposition2[2]),ncol = k,byrow = 1)
    P = p2
  }else if (option3 == 3) 
  {   
    sigma3 = cov.shrink(x)
    output_spectral_decomposition3 = spectral_decomposition(sigma3,k)
    lambda3 = matrix(unlist(output_spectral_decomposition3[1]),ncol = k,byrow = 1)
    p3 = matrix(unlist(output_spectral_decomposition3[2]),ncol = k,byrow = 1)
    P = p3
  }
  
  ### Equation (2.6) computing the new estimator SIGMA~
  sigma_tilde = P %*% lambda_tilde %*% t(P)
  
  return(sigma_tilde)
}


##### GENERALIZED ESTIMATOR
# Equation (2.15)
estimator_generalized <- function(x,m,number_of_samples,option1,option2,option3)
{
  # For parallel computation exporting
  source("compute_matrices.R")
  source("abadir.R")
  
  # Compute the average of s samples
  rand = sapply(1:number_of_samples, function(i)  sample(dim(x)[1],replace = 0)) # Non-repeated random samplings of rows
  
  # Bootstrapped Abadir's estimator
  sampled_estimators = lapply(1:number_of_samples,
                              function(i) estimator(x[rand[,i],], m, option1,option2,option3) )
  samples_sum = Reduce("+",sampled_estimators)
  generalized_estimator =  (1/number_of_samples) * samples_sum # Average computation
  
  return(generalized_estimator)
}


##### GENERALIZED ESTIMATOR OVER MULTIPLE M's
# Equation (3.1)
estimator_generalized_m_averaged <- function(x,m_s,number_of_samples,option1,option2,option3)
{
  generalized_estimators = lapply(1:length(m_s),
                                       function(i) estimator_generalized(x,m_s[i],number_of_samples,option1,option2,option3))
  generalized_estimator_m_sum = Reduce("+",generalized_estimators)
  generalized_estimator_m_averaged =  (1/length(m_s)) * generalized_estimator_m_sum # Average computation
  
  return(generalized_estimator_m_averaged)
}


##### GENERALIZED ESTIMATOR OVER MULTIPLE M's, USING PARALLEL COMPUTING
# Equation (3.1)
estimator_generalized_m_averaged_par <- function(x,m_s,number_of_samples,option1,option2,option3)
{
  f <- function(i)
  {
    m=m_s[i]
    estimator_generalized(x,m,number_of_samples,option1,option2,option3)
  }
  
  # Parallel computing
  cl <- makePSOCKcluster(detectCores()-1)
  export_list = list("x","m_s","number_of_samples","option1","option2","option3","estimator_generalized")
  cluster_export(cl,export_list,environment())
  output_estimators = parLapply(cl, 1:length(m_s), f)
  stopCluster(cl)

  # Bootstrapped generalized estimator
  generalized_estimator_m_sum = Reduce("+",output_estimators)
  generalized_estimator_m_averaged =  (1/length(m_s)) * generalized_estimator_m_sum # Average computation
  
  return(generalized_estimator_m_averaged)
}


##### BOOTSTRAPPED MATRICES
bootstrap <- function(x,m,number_of_bootstraps,benchmark,option1,option2,option3)
{
  n = dim(x)[1]
  k = dim(x)[2]
  m = round(m)

  rand = sapply(1:number_of_bootstraps, function(i) round(runif(n,1,n))) # Repeated random samplings of rows
  
  classical_covariance = ones_matrix(k) # Empty data container
  bootstraps_cov = list() # Empty data container
  
  if (missing(benchmark))
  {
    # Bootstrapped classical covariance matrix
    bootstraps_cov = lapply(1:number_of_bootstraps,
                               function(i) classic_covariance_matrix(x[rand[,i],]))
    bootstraps_sum = Reduce("+",bootstraps_cov)
    classical_covariance =  ( n/ ((n-1)*number_of_bootstraps) ) * bootstraps_sum # Average computation
    
  }else {classical_covariance=benchmark}
  
  # Bootstrapped Abadir's estimator
  bootstraps_estimator = lapply(1:number_of_bootstraps,
                              function(i) estimator(x[rand[,i],], m, option1,option2,option3) )
  bootstraps_sum = Reduce("+",bootstraps_estimator)
  generalized_estimator =  (1/number_of_bootstraps) * bootstraps_sum # Average computation
  
  # Errors
  differences = lapply(1:number_of_bootstraps, function(i) classical_covariance - bootstraps_estimator[[i]])
  norms_l1 = sapply(1:number_of_bootstraps, function(i) vech_norm(differences[[i]], 1) )
  norms_l2 = sapply(1:number_of_bootstraps, function(i) vech_norm(differences[[i]], 2) )
  
  return( list( classical_covariance, generalized_estimator, mean(norms_l1), mean(norms_l2), bootstraps_cov, bootstraps_estimator, rand ) ) }


##### BOOTSTRAPPED MATRICES (COVARIANCE AND CORRELATION)
bootstrap_both <- function(x,m,number_of_bootstraps,benchmark,option1,option2,option3)
{
  n = dim(x)[1]
  k = dim(x)[2]
  m = round(m)
  # Because resampling with repetition can cause covariance matrices to be singular (2 rows can be the same),
  # We bootstrap a correlation matrix every average of 3 covariance matrices
  j = max( floor(number_of_bootstraps/3) ,1) # Number of correlation matrices
  # Get the bootstrap of the covariances
  bootstrapping_cov = bootstrap(x,m,number_of_bootstraps,benchmark,option1,option2,option3)
  classical_covariance = bootstrapping_cov[1]
  generalized_estimator = bootstrapping_cov[2]
  norm_l1_cov = bootstrapping_cov[3]
  norm_l2_cov = bootstrapping_cov[4]
  
  rand = matrix(unlist( bootstrapping_cov[7] ),byrow = 1, ncol = number_of_bootstraps)
  temp_estimators = bootstrapping_cov[6]

  if (missing(benchmark))
  {  
    temp_covariances = bootstrapping_cov[5]
    
    # Bootstrapped classical correlation
    temp_cov_averages = lapply( 0:(j-1),
                                function(i) ( temp_covariances[[1]][[(1+i*3)]]/3 + temp_covariances[[1]][[(2+i*3)]]/3 + temp_covariances[[1]][[(3+i*3)]]/3) )
    bootstraps = lapply( 1:j, function(i) correlation_matrix(temp_cov_averages[[i]]))
    bootstraps_sum = Reduce("+",bootstraps)
    classical_correlation =  ( n/ (n*j) ) * bootstraps_sum # Average computation
    
  }else{classical_correlation = correlation_matrix(benchmark)}
  
  # Bootstrapped Abadir's estimator correlation
  temp_corr_averages = lapply( 0:(j-1),
                              function(i) ( temp_estimators[[1]][[(1+i*3)]]/3 + temp_estimators[[1]][[(2+i*3)]]/3 + temp_estimators[[1]][[(3+i*3)]]/3) )
  bootstraps = lapply( 1:j, function(i) correlation_matrix(temp_corr_averages[[i]]))
  bootstraps_sum = Reduce("+",bootstraps)
  generalized_correlation =  ( n/ (n*j) ) * bootstraps_sum # Average computation
  
  # Errors
  differences = lapply( 1:j, function(i) classical_correlation - bootstraps[[i]])
  norms_l1_corr = sapply( 1:j, function(i) vech_norm(differences[[i]], 1) )
  norms_l2_corr = sapply( 1:j, function(i) vech_norm(differences[[i]], 2) )

    return( list( classical_covariance, generalized_estimator, norm_l1_cov, norm_l2_cov, 
                classical_correlation, generalized_correlation, mean(norms_l1_corr), mean(norms_l2_corr) ) ) }

##### BOOTSTRAPPED MATRICES, USING PARALLEL COMPUTING
bootstrap_par <- function(x,m,number_of_bootstraps,benchmark,option1,option2,option3)
{
  
  n = dim(x)[1]
  k = dim(x)[2]
  m = round(m)
  
  rand = sapply(1:number_of_bootstraps, function(i) round(runif(n,1,n))) # Repeated random samplings of rows
  
  classical_covariance = ones_matrix(k) # Empty data container
  bootstraps_cov = list() # Empty data container
  
  if (missing(benchmark))
  {
    # Bootstrapped classical covariance matrix
    bootstraps_cov = lapply(1:number_of_bootstraps,
                            function(i) classic_covariance_matrix(x[rand[,i],]))
    bootstraps_sum = Reduce("+",bootstraps_cov)
    classical_covariance =  ( n/ ((n-1)*number_of_bootstraps) ) * bootstraps_sum # Average computation
    
  }else {classical_covariance=benchmark}
  
  f <- function(i)
  {
    estimator(x[rand[,i],], m, option1,option2,option3)
  }
  
  # Parallel computing
  cl <- makePSOCKcluster(detectCores()-1)
  export_list = list("x","m","number_of_bootstraps","benchmark","option1","option2","option3","estimator")
  cluster_export(cl,export_list,environment())
  bootstraps_estimator = parLapply(cl, 1:number_of_bootstraps, f)
  stopCluster(cl)
  
  # Bootstrapped estimator
  bootstrapped_estimator_sum = Reduce("+",bootstraps_estimator)
  bootstrapped_estimator_averaged =  (1/number_of_bootstraps) * bootstrapped_estimator_sum # Average computation
  
  return( bootstrapped_estimator_averaged ) }


##### OPTIMAL m based on the errors from the covariance matrix from Abadir estimator
# Return a list containing ( optimal m, vector of errors for each m used )
find_optimal_m_cov <- function(x,m_s,number_of_bootstraps,option1,option2,option3)
{
  errors = lapply(m_s,
                  function(m_s)  bootstrap(x,m_s,number_of_bootstraps,option1,option2,option3)[3] )
  errors = unlist(errors)
  names(errors) = m_s
  
  # Since the single lowest error of all the set could be due to chance and be surrounded by significantly higher values, 
  # I Compute a rolling average of 4 observations in order to identify clusters of low errors (regions of mimima)
  optimal_m = unlist(cluster_minimum(errors, 4)[1])
  
  return( list( optimal_m, errors ) ) }


##### OPTIMAL m based on the errors from the correlation matrix based on the covariance matrix from Abadir estimator
# Return a list containing ( optimal m, vector of errors for each m used )
find_optimal_m_corr <- function(x,m_s,number_of_bootstraps,option1,option2,option3)
{
  errors = lapply(m_s,
                  function(m_s)  bootstrap_both(x,m_s,number_of_bootstraps,option1,option2,option3)[7] )
  errors = unlist(errors)
  names(errors) = m_s
  
  # Since the single lowest error of all the set could be due to chance and be surrounded by significantly higher values, 
  # I Compute a rolling average of 4 observations in order to identify clusters of low errors (regions of mimima)
  optimal_m = unlist(cluster_minimum(errors, 4)[1])
  
  return( list( optimal_m, errors ) ) }


##### OPTIMAL m based on the errors from both (from the correlation matrix based on the covariance matrix from Abadir estimator)
# Return a list containing ( optimal m, vector of errors for each m used )
find_optimal_m_both <- function(x,m_s,number_of_bootstraps,benchmark,option1,option2,option3)
{
  # In order for the parallelization of the computations to work, we need to provide all the files to load to execute the code
  # And this even if we put it in the main.
  header=source("header.R")
  
  output_bootstrapping = list() # Empty data container
  
  if (missing(benchmark))
  {
    output_bootstrapping = lapply(m_s,
                        function(m_s)  bootstrap_both(x,m_s,number_of_bootstraps,option1,option2,option3) )
  }else
  {
    output_bootstrapping = lapply(m_s,
                        function(m_s)  bootstrap_both(x,m_s,number_of_bootstraps,benchmark,option1,option2,option3) )
  }
  
  errors_cov = sapply(1:length(m_s),
                      function(i) unlist(output_bootstrapping[[i]][[3]]) )
  names(errors_cov) = m_s
  
  errors_corr = sapply(1:length(m_s),
                      function(i) unlist(output_bootstrapping[[i]][[7]]) )
  names(errors_corr) = m_s

  # Standardize them to their lowest error
  standardized_errors_cov = errors_cov/min(errors_cov)-1
  standardized_errors_corr = errors_corr/min(errors_corr)-1
  # Create an index to minimize (simply an average of their standardized errors)
  errors_both = (standardized_errors_cov + standardized_errors_corr)/2
  
  # Since the single lowest error of all the set could be due to chance and be surrounded by significantly higher values, 
  # I Compute a rolling average of 4 observations in order to identify clusters of low errors (regions of mimima)
  optimal_m = unlist(cluster_minimum(errors_both, 4)[1])
  
  return( list(optimal_m, errors_both, errors_cov, errors_corr ) ) }

