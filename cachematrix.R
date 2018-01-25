## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Funcion receives a matrix
#


makeCacheMatrix <- function(x = matrix()) 
{
  #Create list that allows to save different values of the matrix and functions to retrieve them
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- solve(x)
  
  getinverse <- function() i
  
  list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check for whether the inverse already exists and return if so
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #calculate the inverse and return it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
