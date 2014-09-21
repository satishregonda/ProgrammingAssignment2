## Create a list that stores matrix of values and inverse of matrix as global variables,
## and then returns inverse of the matrix either by calculating or reading values from its cache

## Write a short comment describing this function

## makeCacheMatrix has four function, 'set', 'get','setmatrixinverse' and 'getmatrixinverse'

## 'set' defines two global variables, and assigns matrix of values to a global variable; the other one
## is to store inverse of the matrix

## in the 'set' function, one can define 'mi' as a matrix of NA values using the following command
## mi <- matrix(nrow=nrow(x),ncol=ncol(x))
## and then use, is.na instead is.null in the cacheSolve function

## 'get' stores the matrix of values for which inverse to be calculated

## 'setmatrixinverse' stores inverse of matrix in 'mi'

## 'getmatrixinverse' returns inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  
  ## 'mi' can be defined as "  mi <- matrix(nrow=nrow(x),ncol=ncol(x))
  
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }

  get <- function() x
  
  setmatrixinverse <- function(matrixinverse) mi<<-matrixinverse
  
  getmatrixinverse <- function() mi
  
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}


## Write a short comment describing this function

## The function returns matrix inverse; 
## if the matrix inverse is already calculated then returns values from its cache,
## otherwise, calculates and then returns the value
## First it checks whether matrixinverse variable, 'mi' is NULL or not, if it is not then 
## the value read from its cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  mi <- x$getmatrixinverse()
  
  ## instead if(!is.null(mi)), one can use "if(!unique(is.na(as.vector(mi)))) "
  
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  matrixinverse <- solve(data)
  mi=x$setmatrixinverse(matrixinverse)
  mi
}
