library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               # to initialize the inverse as NULL
  set <- function(y){
                     x <<- y
                     inv <<- NULL
  }
  
  get <- function()x       # to get matrix x
  setinv <- function(inverse)inv<<-
  getinv <- function(){
    inver <- ginv(x)         
    inver%%x              #to obtain inverse of the matrix
    }
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}

## Write a short comment describing this function
# This function is used to get the cache data
cacheSolve <- function(x, ...) #obtain cache data
{
    inv <- x$getinv()
    if (!is.null(inv)){         #Check if inverse is NULL
                        message("Getting Cached Data")
                        return(inv)  #Return inverse value
    }
  data <- x$get()
  inv <- solve(data, ...)       #Calculating the inverse value
  x$setinv(inv)
  inv           ## Return a matrix that is the inverse of 'x'
}
                                                                                  #
