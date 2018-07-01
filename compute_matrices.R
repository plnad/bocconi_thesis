# The equations referred to are the ones in Abadir's used at the time, available in the same folder as this code

##### IDENTITY MATRIX (Function to create identity matrices)
identity_matrix <- function(dimension)
{ return( diag(dimension) ) }


##### ONES MATRIX (Function to create matrix of ones)
# n is for the number of rows, ncol is optional
# If only n is specified, then the matrix will be an [n x n]
# Otherwise it will be an [n x ncol]
ones_matrix <- function(n,ncol)
{ 
  if(missing(ncol)) {return( matrix(rep(1,len = n*n), nrow = n) )}
  else{return( matrix(rep(1,len = n*ncol), nrow = n) )}
} 


##### DEMEANING MATRIX  (Function to create the demeaning matrix used at the top of section 2.1 of the paper)
demeaning_matrix <- function(n_observations)
{
  return( identity_matrix(n_observations) - ones_matrix(n_observations)/n_observations )
}


##### CLASSIC COVARIANCE MATRIX
# main_matrix is optional
# If main_matrix is specified, the demeaning is done using the mean of the columns of main_matrix
# Otherwise, the demeaning will be done using the mean of the columns of x
classic_covariance_matrix <- function(x,main_matrix)
{
  n = dim(x)[1]
  k = dim(x)[2]
  if (missing(main_matrix))
  {
    x_transpose = t(x) # Create the transpose. [k x n]
    m = demeaning_matrix(n) # Create the demeaning matrix. [n x n]
    covariance_matrix =  (x_transpose %*% m %*% x)/ n # [k x k] 
    
    return( covariance_matrix )
  }
  else
  {
    x = t(t(x) - colMeans(main_matrix) ) # Demeans using the main matrix mean
    x_transpose = t(x) # Create the transpose. [k x n]
    covariance_matrix =  (x_transpose %*%  x)/ n # [k x k] 
    
    return( covariance_matrix )
  }
}


##### CORRELATION MATRIX
correlation_matrix <- function(covariance_matrix)
{
k = dim(covariance_matrix)[2]
stdev_diagonal_matrix = sqrt(diag(covariance_matrix)) * identity_matrix(k)
correlation_matrix = matrix.inverse(stdev_diagonal_matrix) %*% covariance_matrix %*% matrix.inverse(stdev_diagonal_matrix) # [k x k] 

return(correlation_matrix)
}


##### SPECTRAL DECOMPOSITION
spectral_decomposition <- function(covariance_matrix,k)
{
spectral_decomposition = eigen(covariance_matrix,1)
eigenvalues = spectral_decomposition[1]
lambda = as.vector(c(unlist(eigenvalues))) * identity_matrix(k) # [k x k] 
eigenvectors = matrix( unlist(spectral_decomposition[2]),ncol = k,byrow = 1 ) # [k x k]
  
return(list(lambda,eigenvectors))
}


##### COVARIANCE
covariance <- function(x,y) {  return( mean( (x-mean(x)) * (y-mean(y)) ) ) }


##### PAIRWISE COVARIANCE
covariance_matrix_pairwise <- function(x)
{
k = dim(x)[2]

elements_half = ((k*k)-k)/2 + k
covariance_matrix = ones_matrix(k)*0
m = 1
n = 0
changed_row = 0
previous_m = 1
for (i in 1:elements_half)  
{
  n = n+1 + (m-1)*changed_row - floor(n/k)*k
  pair_covariance = covariance(x[,m], x[,n])
  covariance_matrix[m,n] =  pair_covariance
  previous_m = m
  m = m+floor(n/k)
  changed_row = m-previous_m
}
covariance_matrix = covariance_matrix + t(covariance_matrix) - diag(covariance_matrix)*identity_matrix(k)

return(covariance_matrix)
}


##### PAIRWISE CORRELATIONS
correlation_matrix_pairwise <- function(x)
{
k = dim(x)[2]

elements_half = ((k*k)-k)/2 + k # Number of elements in the upper half + the diagonal
correlation_matrix = ones_matrix(k)*0 # Empty data container

# The following is the algo to compute the right correlation at the right place in the matrix
m = 1
n = 0
changed_row = 0
previous_m = 1
for (i in 1:elements_half)  
{
  n = n+1 + (m-1)*changed_row - floor(n/k)*k
  pair_correlation = cor(x[,m], x[,n]) # Using the cor function from R
  correlation_matrix[m,n] =  pair_correlation
  previous_m = m
  m = m+floor(n/k)
  changed_row = m-previous_m
}
correlation_matrix = correlation_matrix + t(correlation_matrix) - identity_matrix(k)

return(correlation_matrix)
}


# NORMALIZE A MATRIX
normalize_matrix <-function(x)
{
  x = t(apply(x,1,"-",apply(x,2,mean)))
  x = t(apply(x,1,"/",apply(x,2,sd)))
  return(x)
}

# RANDOM CORRELATED NUMBERS
random_correlated_matrix <- function(sigma,n_observations)
{
  x = rmvnorm(n_observations,rep(0,ncol(sigma)),sigma)
  x = normalize_matrix(x)
  return(x)
}

# COMPUTE HALF-VECTORIZED NORM
vech_norm <- function(x,lnorm)
{  return( round( entrywise.norm( vech(x),lnorm ) ,15 ) ) }

