## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

## Test the code:
#### Generating a* matrix with random numbers and calculate the inversion.

## m <- makeCacheMatrix(matrix(rnorm(100),10,10))
## cacheSolve(m)

## ## cacheSolve(m)
## getting cached data
## [,1]       [,2]        [,3]       [,4]        [,5]        [,6]       [,7]       [,8]        [,9]       [,10]
## [1,]  1.10297147 -2.1214758  0.18547876  3.8982506  1.68274051  0.16918534 -2.2132809  2.2608545  0.53185842  0.97601433
## [2,]  0.49679222 -0.6144006 -0.01354220  1.4701479  0.52502071  0.59876000 -1.2599763  0.8527343  0.28580078  0.02342759
## [3,]  0.05499417 -0.5808558 -0.06039828  0.5538489  0.03667299 -0.18470567 -0.4760154  0.6010359  0.22020148  0.32391553
## [4,]  0.43354869 -0.8067562  0.23224485  1.1569423  0.28393327  0.16035061 -0.5009274  0.5545324 -0.03613986  0.27374073
## [5,]  0.03828794  0.1395654 -0.19565605  0.2390378 -0.14079636 -0.04486790 -0.1480963 -0.1442880 -0.09196995  0.13293672
## [6,] -0.86281727  0.6331191  0.59692940 -2.0889554 -0.70136381 -0.90508459  2.2029844 -1.3898632 -0.72732476 -0.65297424
## [7,] -0.78727542  1.1646003 -0.31651915 -1.9888562 -0.73293367  0.05266836  1.1818307 -1.2720720 -0.15972716 -0.52582366
## [8,] -0.21409173  0.3929644 -0.04166810 -0.4575075 -0.48739595  0.05901130  0.2980047 -0.1272085  0.10784979 -0.09097374
## [9,]  0.52455653 -0.8357945 -0.12485449  1.5656913  0.53885060  0.32396147 -0.8546510  0.9194719  0.45445542 -0.04325122
## [10,]  0.55246057 -0.8569321 -0.24380843  1.7461915  0.67514466  0.28170585 -1.5948301  1.1009900  0.79890395  0.54471180
